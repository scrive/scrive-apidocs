{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
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
module Doc.DocStorage(
      getFileContents
    , uploadDocumentFileToAmazon 
    , uploadDocumentFilesToTrustWeaver
    , maybeScheduleRendering
    , preprocessPDF) where

import Control.Concurrent
import Control.Monad
import Doc.DocState
import Happstack.State (update,query)
import MinutesTime
import Misc
import System.Directory
import System.Exit
import System.IO
import System.Process
import Kontra
import qualified Amazon as AWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.Map as Map
import qualified TrustWeaver as TW
import qualified AppLogger as Log
import System.IO.Temp
import qualified MemCache
import Data.Char


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

{- Upload document to Amazon -}
uploadDocumentFileToAmazon :: FilePath
                                 -> AWS.S3Action
                                 -> DocumentID 
                                 -> FileID
                                 -> IO ()
uploadDocumentFileToAmazon docstore ctxs3action docid fileid1 = do
  Just doc <- query $ GetDocumentByDocumentID docid
  let files = documentfiles doc ++ documentsealedfiles doc
  case filter (\x -> fileid x == fileid1) files  of
    [file] -> do
      AWS.uploadFile docstore ctxs3action file
      return ()
    _ -> return ()
  return ()

{- Upload document to TW-}
uploadDocumentFilesToTrustWeaver :: TW.TrustWeaverConf 
                                 -> String 
                                 -> DocumentID 
                                 -> IO ()
uploadDocumentFilesToTrustWeaver ctxtwconf twownername documentid = do
  Just document <- query $ GetDocumentByDocumentID documentid
  let twdocumentid = show documentid
  let twdocumentdate = showDateOnly (documentmtime document)
  let File{filestorage = FileStorageMemory pdfdata} = head $ documentsealedfiles document 
          
  -- FIXME: we should retry here if the following fails
  -- because of external reasons 
  reference <- eitherLog $ TW.storeInvoice ctxtwconf twdocumentid twdocumentdate twownername pdfdata
  _ <- update $ SetDocumentTrustWeaverReference documentid reference
  return ()


{- |
   The command line for calling ghostscript
 -}
gs :: String
gs = "gs"


resizeImageAndReturnOriginalSize filepath = do
    (_,Just sizerouthandle,_, sizechecker) <- createProcess $ ( proc "identify" [filepath])
                                              { std_out = CreatePipe }
    out <-  hGetContents sizerouthandle
    let (w,h) = readSize out
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
    return (fcontent,w,h)
  where
   readSize::[Char] -> (Int,Int) --Ugly and unsafe but I can't get info about output format so writing nicer parser is useless
   readSize ('J':'P':'E':'G':' ':rest) = let
                                          (w,hs) = span (isDigit) rest
                                          h = takeWhile (isDigit) (tail hs)
                                         in (read w,read h)
   readSize (_:rest) = readSize rest
   readSize [] = (943,1335)

{- |
   Convert PDF to jpeg images of pages
 -}
convertPdfToJpgPages :: Context
                     -> File
                     -> DocumentID
                     -> IO JpegPages
convertPdfToJpgPages ctx file docid = withSystemTempDirectory "pdf2jpeg" $ \tmppath -> do
  let sourcepath = tmppath ++ "/source.pdf"

  content <- getFileContents ctx file

  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=jpeg" 
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
        (path,handle) <- openTempFile systmp ("pdf2jpg-failed-" ++ show (fileid file) ++ "-.pdf")
        Log.error $ "Cannot pdf2jpg (doc #" ++ show docid ++ ", file #" ++ show (fileid file) ++ "): " ++ path
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
   
{- | Shedules rendering od a file. After forked process is done, images will be put in shared memory. -}
maybeScheduleRendering :: Context 
                       -> File
                       -> DocumentID
                       -> IO JpegPages
maybeScheduleRendering ctx@Context{ ctxnormalizeddocuments = mvar }
                       (file@File { fileid }) docid = do
  modifyMVar mvar $ \setoffilesrenderednow ->
      case Map.lookup fileid setoffilesrenderednow of
         Just pages -> return (setoffilesrenderednow, pages)
         Nothing -> do
           Log.forkIOLogWhenError ("error rendering file #" ++ show fileid ++ " of doc #" ++ show docid) $ do
                jpegpages <- convertPdfToJpgPages ctx file docid
                case jpegpages of
                     JpegPagesError errmsg -> do
                         _ <- update $ ErrorDocument docid $ BS.toString errmsg
                         return ()
                     _                     -> return ()
                modifyMVar_ mvar (\filesrenderednow -> return (Map.insert fileid jpegpages filesrenderednow))
           return (Map.insert fileid JpegPagesPending setoffilesrenderednow, JpegPagesPending)

{- |  Convert PDF to uncompress it. -}
preprocessPDF :: BS.ByteString
              -> DocumentID
              -> IO BS.ByteString
preprocessPDF content docid = withSystemTempDirectory "preprocess_gs" $ \tmppath -> do
  let sourcepath = tmppath ++ "/source.pdf"
  let outputpath = tmppath ++ "/output.pdf"

  BS.writeFile sourcepath content

  let gsproc = (proc gs [ "-sDEVICE=pdfwrite" 
                        , "-sOutputFile=" ++ outputpath 
                        , "-dSAFER"
                        , "-dBATCH"
                        , "-dNOPAUSE"
                        , sourcepath
                        ]) { std_out = CreatePipe
                           , std_err = CreatePipe
                           }
  (_, Just outhandle, Just errhandle, gsProcHandle) <- createProcess gsproc
  errcontent <- BSL.hGetContents errhandle
  outcontent <- BSL.hGetContents outhandle
                 
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
