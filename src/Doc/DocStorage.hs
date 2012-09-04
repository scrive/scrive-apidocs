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

import Control.Applicative
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Error
import Data.Typeable
import DB
import Doc.DocStateData
import File.Model
import ForkAction
import Kontra
import Misc
import Prelude hiding (catch)
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import qualified Amazon as AWS
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Log
import qualified MemCache as MemCache
import qualified SealSpec as Seal
import Redirect

instance GuardRight FileError where
  guardRight (Right b)            = return b
  guardRight (Left fe)            = do
                                     Log.error $ show fe
                                     internalError

{- Gets file content from somewere (Amazon for now), putting it to cache and returning as BS -}
getFileContents :: (KontraMonad m, MonadIO m) => File -> m BS.ByteString
getFileContents file = do
  ctx <- getContext
  mcontent <- MemCache.get (fileid file) (ctxfilecache ctx)
  case mcontent of
      Just content -> return content
      Nothing -> do
        mcontentAWS <- liftIO $ AWS.getFileContents (ctxs3action ctx) file
        case mcontentAWS of
          Nothing -> do
            Log.debug $ "Couldn't get content for file " ++ show (fileid file) ++ ", returning empty ByteString."
            return BS.empty
          Just contentAWS -> do
            MemCache.put (fileid file) contentAWS (ctxfilecache ctx)
            return contentAWS

getFileIDContents :: (KontraMonad m, MonadDB m) => FileID -> m BS.ByteString
getFileIDContents fid = do
  mfile <- dbQuery $ GetFileByFileID fid
  case mfile of
    Just file -> getFileContents file
    Nothing -> return BS.empty


resizeImageAndReturnOriginalSize :: String -> IO (BS.ByteString, Int, Int)
resizeImageAndReturnOriginalSize filepath = do
    (_,Just sizerouthandle,_, sizechecker) <- createProcess $ ( proc "identify" [filepath])
                                              { std_out = CreatePipe }
    _out <- hGetContents sizerouthandle
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
convertPdfToJpgPages :: (KontraMonad m, MonadDB m)
                     => String
                     -> FileID
                     -> DocumentID
                     -> m JpegPages
convertPdfToJpgPages gs fid docid = do
  content <- getFileIDContents fid
  liftIO $ withSystemTempDirectory "pdf2jpeg" $ \tmppath -> do
    let sourcepath = tmppath ++ "/source.pdf"

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
                        `catch` \(_ :: SomeException) -> do
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
  Context{ctxgscmd,ctxnormalizeddocuments} <- getContext

  -- Some debugs
  Log.debug $ "Rendering is being scheduled for document " ++ show docid
  pgs <- MemCache.size ctxnormalizeddocuments
  Log.debug $ "Total rendered pages count: " ++ show (pgs)

  -- Propper action
  v <- MemCache.get fileid ctxnormalizeddocuments
  case v of
      Just pages -> return pages
      Nothing -> do
          MemCache.put fileid JpegPagesPending ctxnormalizeddocuments
          forkAction ("Rendering file #" ++ show fileid ++ " of doc #" ++ show docid) $ do
                   jpegpages <- convertPdfToJpgPages ctxgscmd fileid docid             -- FIXME: We should report error somewere
                   MemCache.put fileid jpegpages ctxnormalizeddocuments
          return JpegPagesPending

  
data FileError = FileSizeError Int Int
               | FileFormatError
               | FileNormalizeError BSL.ByteString
               | FileSealingError BSL.ByteString
               | FileOtherError String
               deriving (Eq, Ord, Show, Read, Typeable)

instance Error FileError where
    strMsg = FileOtherError

preCheckPDFHelper :: String
                  -> BS.ByteString
                  -> String
                  -> IO (Either FileError BS.ByteString)
preCheckPDFHelper gscmd content tmppath =
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
               , Seal.staticTexts    = sealingTexts
               , Seal.attachments    = []
               , Seal.filesList      = []
               }

    sealingTexts = Seal.SealingTexts
                   { Seal.verificationTitle  = "An example text"
                   , Seal.docPrefix          = "An example text"
                   , Seal.signedText         = "An example text"
                   , Seal.partnerText        = "An example text"
                   , Seal.secretaryText      = "An example text"
                   , Seal.documentText       = "An example text"
                   , Seal.orgNumberText      = "An example text"
                   , Seal.personalNumberText = "An example text"
                   , Seal.eventsText         = "An example text"
                   , Seal.dateText           = "An example text"
                   , Seal.historyText        = "An example text"
                   , Seal.verificationFooter = "An example text"
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

      -- GhostScript sometimes breaks files by normalization. Here we
      -- detect what it calls warnings and what trully breaks a PDF.
      --
      -- Example:
      --
      -- GPL Ghostscript 9.02: Warning: 'loca' length 326 is greater than numGlyphs 162 in the font NGBUYI+Arial-BoldMT.
      --
      -- Warning about glyphs causes truncation of fonts and glyphs show empty on page.
      --
      -- If such thing happened just use source as normalized document.

      when (any (BSL.pack "is greater than numGlyphs" `BSL.isPrefixOf`) (BSL.tails stderr1)) $ do
         liftIO $ BS.writeFile normalizedpath content

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
            -> IO (Either FileError Binary)
preCheckPDF gscmd content =
  withSystemTempDirectory "precheck" $ \tmppath -> do
    value <- preCheckPDFHelper gscmd content tmppath
      `E.catch` \(e::IOError) -> return (Left (FileOtherError (show e)))
    case value of
      Left x -> Log.error $ "preCheckPDF: " ++ show x
      Right _ -> return ()
    return $ Binary <$> value
