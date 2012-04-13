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
    , scaleForPreview
    , FileError(..)
    , preCheckPDF
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Data.Typeable
import DB.Classes
import Doc.DocStateData
import File.Model
import ForkAction
import Kontra
import Misc
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import qualified Amazon as AWS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Log
import qualified MemCache
import qualified SealSpec as Seal
import Redirect

instance GuardRight FileError where
  guardRight (Right b)            = return b
  guardRight (Left fe)            = do
                                     Log.error $ show fe
                                     internalError

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
  mfile <- ioRunDB (ctxdbenv ctx) . dbQuery $ GetFileByFileID fid
  case mfile of
    Just file -> getFileContents ctx file
    Nothing -> return BS.empty


{- Upload document to TW-}

{-  This function was implemented the other day, but bitrotted. Rescue it if needed.
 
_uploadDocumentFilesToTrustWeaver :: TW.TrustWeaverConf
                                 -> String
                                 -> DocumentID
                                 -> IO ()
_uploadDocumentFilesToTrustWeaver _ctxtwconf _twownername _documentid = do
  error "uploadDocumentFilesToTrustWeaver is unimplemented"

  Just document <- query $ GetDocumentByDocumentID documentid
  let twdocumentid = show documentid
  let twdocumentdate = showDateOnly (documentmtime document)
  let File{filestorage = FileStorageMemory pdfdata} = head $ documentsealedfiles document

  -- FIXME: we should retry here if the following fails
  -- because of external reasons
  reference <- eitherLog $ TW.storeInvoice ctxtwconf twdocumentid twdocumentdate twownername pdfdata
  _ <- update $ SetDocumentTrustWeaverReference documentid reference
  return ()
-}

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
       forkAction ("Rendering file #" ++ show fileid ++ " of doc #" ++ show docid) $ \env' -> do
                let newctx = ctx { ctxdbenv = env' }
                jpegpages <- convertPdfToJpgPages newctx fileid docid
                case jpegpages of
                     JpegPagesError _errmsg -> do
                                        -- FIXME: need to report this error somewhere
                         -- _ <- runDBUpdate $ ErrorDocument docid $ BS.toString errmsg
                         return ()
                     _                     -> return ()
                modifyMVar_ mvar (\filesrenderednow -> return (Map.insert fileid jpegpages filesrenderednow))
  return p

data FileError = FileSizeError Int Int
               | FileFormatError
               | FileNormalizeError BSL.ByteString
               | FileSealingError BSL.ByteString
               | FileOtherError String
               deriving (Eq, Ord, Show, Read, Typeable)

instance Error FileError where
    strMsg = FileOtherError

preCheckPDF' :: String
             -> BS.ByteString
             -> String
             -> IO (Either FileError BS.ByteString)
preCheckPDF' gscmd content tmppath = 
    runErrorT $ do
      checkSize
      checkHeader
      checkNormalize
      checkSealing
      readOutput
  where
    sourcepath = tmppath ++ "/source.pdf"
    normalizedpath = tmppath ++ "/normalized.pdf"
    sealedpath = tmppath ++ "/sealed.pdf"

    sizeLimit = 10 * 1000 * 1000
    contentLength = BS.length content

    headerPattern = BS.pack "%PDF-1."

    sealSpec = Seal.SealSpec
               { Seal.input          = normalizedpath
               , Seal.output         = sealedpath
               , Seal.documentNumber = "An example text"
               , Seal.persons        = []
               , Seal.secretaries    = []
               , Seal.history        = []
               , Seal.initials       = "An example text"
               , Seal.hostpart       = "An example text"
               , Seal.fields         = []
               , Seal.staticTexts    = sealingTexts
               , Seal.attachments    = []
               }

    sealingTexts = Seal.SealingTexts
                   { Seal.verificationTitle  = "An example text"
                   , Seal.docPrefix          = "An example text"
                   , Seal.signedText         = "An example text"
                   , Seal.partnerText        = "An example text"
                   , Seal.secretaryText      = "An example text"
                   , Seal.orgNumberText      = "An example text"
                   , Seal.eventsText         = "An example text"
                   , Seal.dateText           = "An example text"
                   , Seal.historyText        = "An example text"
                   , Seal.verificationFooter = ["An example text", "An example text", "An example text"]
                   }

    checkSize = do
      when (contentLength > sizeLimit) $
           throwError (FileSizeError sizeLimit contentLength)

    checkHeader = do
      when (not $ headerPattern `BS.isPrefixOf` content) $ do
        throwError (FileFormatError)

    checkNormalize = do
      liftIO $ BS.writeFile sourcepath content
    
      (exitcode,_stdout,stderr1) <- liftIO $ readProcessWithExitCode' gscmd 
                                   [ "-sDEVICE=pdfwrite"
                                   , "-sOutputFile=" ++ normalizedpath
                                   , "-dSAFER"
                                   , "-dBATCH"
                                   , "-dNOPAUSE"
                                   , sourcepath
                                   ] BSL.empty
      when (exitcode /= ExitSuccess ) $ do
        liftIO $ do
          systmp <- getTemporaryDirectory
          (_path,handle) <- openTempFile systmp ("pre-normalize-failed-.pdf")
          BS.hPutStr handle content
          hClose handle

        throwError (FileNormalizeError stderr1)


    checkSealing = do
      (exitcode,_stdout,stderr1) <- liftIO $ readProcessWithExitCode' "dist/build/pdfseal/pdfseal" 
                                   [] (BSL.pack (show sealSpec))
      when (exitcode /= ExitSuccess ) $ do
        liftIO $ do
          systmp <- getTemporaryDirectory
          (_path,handle) <- openTempFile systmp ("pre-sealing-failed-.pdf")
          BS.hPutStr handle content
          hClose handle

        throwError (FileSealingError stderr1)
      
    readOutput = liftIO $ BS.readFile normalizedpath

-- | The 'preCheckPDF' function should be invoked just after receiving
-- uploaded document from user and before it gets into the
-- database. It does the following:
--
-- - Checks if the file is not too large
--
-- - Checks if beggining bytes
-- are '%PDF-1.' designating a PDF format
--
-- - Normalizes using GhostScript pdfwrite command. This is required
-- as we need to process 1.4 version documents maksimum and pdfwrite
-- ensures PDF files are in 1.4 (uncompressed structure) form.
--
-- - Tries to do pdfseal process with empty data, so we know in
-- advance if it did work or not
--
-- Return value is either a 'BS.ByteString' with normalized document
-- content or 'FileError' enumeration stating what is going on.
--
preCheckPDF :: String
            -> BS.ByteString
            -> IO (Either FileError BS.ByteString)
preCheckPDF gscmd content = 
  withSystemTempDirectory "precheck" $ \tmppath -> do
    value <- preCheckPDF' gscmd content tmppath `Prelude.catch` \e -> return (Left (FileOtherError (show e)))
    case value of
      Left x -> Log.error $ "preCheckPDF: " ++ show x
      Right _ -> return ()
    return value
