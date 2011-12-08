{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStorage
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-- Most of what is connected to storage of documents - getting files from TW and Amazon
-- Also stuff for generating JPEGS from PDF's
-----------------------------------------------------------------------------
module Doc.DocStorage
    ( getFileContents
    , getFileIDContents
    , maybeScheduleRendering
    , preprocessPDF
    , scaleForPreview
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Doc.DocState
import Happstack.State (update)
--import MinutesTime
--import Misc
import System.Directory
import System.Exit
import System.IO
import System.Process
import Kontra
import qualified Amazon as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified TrustWeaver as TW
import qualified AppLogger as Log
import System.IO.Temp
import qualified MemCache
import ForkAction
import File.Model
import DB.Classes

{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: Context -> File -> IO (BS.ByteString)
getFileContents ctx file = do
  mcontent <- MemCache.get (fileid file) (ctxfilecache ctx)
  case mcontent of
    Just content -> return content
    Nothing -> do
                mcontentAWS <- AWS.getFileContents (ctxs3action ctx) file
                MemCache.put (fileid file) mcontentAWS (ctxfilecache ctx)
                return mcontentAWS

getFileIDContents :: Context -> FileID -> IO BS.ByteString
getFileIDContents ctx fid = do
  mfile <- ioRunDB (ctxdbconn ctx) . dbQuery $ GetFileByFileID fid
  case mfile of
    Just file -> getFileContents ctx file
    Nothing -> return BS.empty


{- Upload document to TW-}
_uploadDocumentFilesToTrustWeaver :: TW.TrustWeaverConf
                                 -> String
                                 -> DocumentID
                                 -> IO ()
_uploadDocumentFilesToTrustWeaver _ctxtwconf _twownername _documentid = do
  error "uploadDocumentFilesToTrustWeaver is unimplemented"
#if 0
  Just document <- query $ GetDocumentByDocumentID documentid
  let twdocumentid = show documentid
  let twdocumentdate = showDateOnly (documentmtime document)
  let File{filestorage = FileStorageMemory pdfdata} = head $ documentsealedfiles document

  -- FIXME: we should retry here if the following fails
  -- because of external reasons
  reference <- eitherLog $ TW.storeInvoice ctxtwconf twdocumentid twdocumentdate twownername pdfdata
  _ <- update $ SetDocumentTrustWeaverReference documentid reference
  return ()
#endif

resizeImageAndReturnOriginalSize :: String -> IO (BS.ByteString, Int, Int)
resizeImageAndReturnOriginalSize filepath = do
    (_,Just sizerouthandle,_, sizechecker) <- createProcess $ ( proc "identify" [filepath])
                                              { std_out = CreatePipe }
    _out <-  hGetContents sizerouthandle
    sizerexitcode <- waitForProcess sizechecker
    case sizerexitcode of
        ExitFailure _ -> return ()
        ExitSuccess -> return ()
    (_,_,_, resizer) <- createProcess $  proc "convert" ["-scale","943x1335!", filepath, filepath]
    resizerexitcode <- waitForProcess resizer
    case resizerexitcode of
        ExitFailure _ -> return ()
        ExitSuccess -> return ()
    fcontent <- BS.readFile filepath
    return (fcontent,943,1335)


scaleForPreview :: DocumentID -> BS.ByteString -> IO BS.ByteString
scaleForPreview did image = withSystemTempDirectory "preview" $ \tmppath -> do
    let fpath = tmppath ++ "/" ++ show did ++ ".jpg"
    BS.writeFile fpath image
    (_,_,_, resizer) <- createProcess $  proc "convert" ["-scale","190x270!", fpath, fpath]
    resizerexitcode <- waitForProcess resizer
    case resizerexitcode of
        ExitFailure _ -> return ()
        ExitSuccess -> return ()
    fcontent <- BS.readFile fpath
    return fcontent
    
{- |
   Convert PDF to jpeg images of pages
 -}
convertPdfToJpgPages :: Context
                     -> FileID
                     -> DocumentID
                     -> IO JpegPages
convertPdfToJpgPages ctx fid docid = withSystemTempDirectory "pdf2jpeg" $ \tmppath -> do
  let sourcepath = tmppath ++ "/source.pdf"

  content <- getFileIDContents ctx fid

  BS.writeFile sourcepath content

  let gs = ctxgscmd ctx
      gsproc = (proc gs [ "-sDEVICE=jpeg"
                        , "-sOutputFile=" ++ tmppath ++ "/output-%d.jpg"
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , "-dTextAlphaBits=4"
                        , "-dGraphicsAlphaBits=4"
                        --, "-r91.361344537815126050420168067227"
                        , "-r190"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  errcontent <- BS.hGetContents errhandle
  outcontent <- BS.hGetContents outhandle

  exitcode <- waitForProcess gsProcHandle

  result <- case exitcode of
    ExitFailure _ -> do
        systmp <- getTemporaryDirectory
        (path,handle) <- openTempFile systmp ("pdf2jpg-failed-" ++ show fid ++ "-.pdf")
        Log.error $ "Cannot pdf2jpg (doc #" ++ show docid ++ ", file #" ++ show fid ++ "): " ++ path
        BS.hPutStr handle content
        hClose handle

        return $ JpegPagesError (errcontent `BS.append` outcontent)
    ExitSuccess -> do
                  let pathofx x = tmppath ++ "/output-" ++ show x ++ ".jpg"
                  let existingPages x = do
                                          exists <- doesFileExist (pathofx x)
                                          if exists
                                            then  fmap (x:) $ existingPages (x+1)
                                            else return []
                  listofpages <- existingPages (1::Integer)
                  x <- forM listofpages $ \x -> resizeImageAndReturnOriginalSize (pathofx x)
                       `catch` \_ -> do
                                    fcontent <- BS.readFile (pathofx x)
                                    return (fcontent,943,1335)

                  return (JpegPages x)
  return result

{- | Shedules rendering od a file. After forked process is done, images will be put in shared memory.
 FIXME: this is so convoluted that I'm getting lost in this function. Make it clear.
 -}
maybeScheduleRendering :: Kontrakcja m
                       => FileID
                       -> DocumentID
                       -> m JpegPages
maybeScheduleRendering fileid docid = do
  ctx@Context{ ctxnormalizeddocuments = mvar } <- getContext
  (p, start) <- liftIO $ modifyMVar mvar $ \setoffilesrenderednow ->
      case Map.lookup fileid setoffilesrenderednow of
         Just pages ->
           return (setoffilesrenderednow, (pages, False))
         Nothing -> do
           return (Map.insert fileid JpegPagesPending setoffilesrenderednow, (JpegPagesPending, True))

  when start $
       forkAction ("Rendering file #" ++ show fileid ++ " of doc #" ++ show docid) $ \conn' -> do
                let newctx = ctx { ctxdbconn = conn' }
                jpegpages <- convertPdfToJpgPages newctx fileid docid
                case jpegpages of
                     JpegPagesError errmsg -> do
                         _ <- update $ ErrorDocument docid $ BS.toString errmsg
                         return ()
                     _                     -> return ()
                modifyMVar_ mvar (\filesrenderednow -> return (Map.insert fileid jpegpages filesrenderednow))
  return p


{- |  Convert PDF to uncompress it. -}
preprocessPDF :: Context
              -> BS.ByteString
              -> DocumentID
              -> IO BS.ByteString
preprocessPDF ctx content docid = withSystemTempDirectory "preprocess_gs" $ \tmppath -> do
  let sourcepath = tmppath ++ "/source.pdf"
  let outputpath = tmppath ++ "/output.pdf"

  BS.writeFile sourcepath content

  let gs = ctxgscmd ctx
      gsproc = (proc gs [ "-sDEVICE=pdfwrite"
                        , "-sOutputFile=" ++ outputpath
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  _errcontent <- BSL.hGetContents errhandle
  _outcontent <- BSL.hGetContents outhandle

  exitcode <- waitForProcess gsProcHandle

  result <- case exitcode of
    ExitFailure _ -> do
        systmp <- getTemporaryDirectory
        (path,handle) <- openTempFile systmp ("preprocess-failed-" ++ show docid ++ "-.pdf")
        Log.error $ "Cannot preprocess pdf (doc #" ++ show docid ++ "): " ++ path
        BS.hPutStr handle content
        hClose handle
        return content
    ExitSuccess -> do
        BS.readFile outputpath

  return result
