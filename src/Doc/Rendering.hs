-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.Rendering
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-- Most of what is connected to storage of documents - getting files from TW and Amazon
-- Also stuff for generating JPEGS from PDF's
-----------------------------------------------------------------------------
module Doc.Rendering
    ( maybeScheduleRendering
    , scaleForPreview
    , FileError(..)
    , preCheckPDF
    , getNumberOfPDFPages
    , getPageSizeOfPDFInPoints
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
import Utils.IO
import Utils.Read
import Utils.String
import Prelude hiding (catch)
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Log
import qualified MemCache as MemCache
import qualified SealSpec as Seal
import Redirect
import File.Storage
import Data.Char
import Data.Maybe

instance GuardRight FileError where
  guardRight (Right b)            = return b
  guardRight (Left fe)            = do
                                     Log.error $ show fe
                                     internalError

scaleForPreview :: BS.ByteString -> IO BS.ByteString
scaleForPreview image = withSystemTempDirectory "preview" $ \tmppath -> do
    let fpath = tmppath ++ "/source.png"
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
                     => FileID
                     -> Int
                     -> m JpegPages
convertPdfToJpgPages fid widthInPixels = do
  content <- getFileIDContents fid
  liftIO $ withSystemTempDirectory "pdf2png" $ \tmppath -> do
    let sourcepath = tmppath ++ "/source.pdf"

    BS.writeFile sourcepath content

    (exitcode,outcontent,errcontent) <-
      liftIO $ readProcessWithExitCode' "mudraw"
               [ "-o", tmppath ++ "/output-%d.png"
               , "-w", show widthInPixels
               , "-b", "8"
               , sourcepath
               ] BSL.empty

    let pathOfPage n = tmppath ++ "/output-" ++ show n ++ ".png"
    let readPagesFrom n = (do
                       contentx <- BS.readFile (pathOfPage n)
                       followingPages <- readPagesFrom (n+1 :: Int)
                       return $ (contentx, widthInPixels, widthInPixels * 842 `div` 595) : followingPages)
             `catch` \(_ :: SomeException) -> return []

    result <- case exitcode of
      ExitFailure _ -> do
        systmp <- getTemporaryDirectory
        (path,handle) <- openTempFile systmp ("pdf2png-failed-" ++ show fid ++ "-.pdf")
        Log.error $ "Cannot pdf2png of file #" ++ show fid ++ ": " ++ path
        BS.hPutStr handle content
        hClose handle

        return $ JpegPagesError (concatChunks errcontent `BS.append` concatChunks outcontent)
      ExitSuccess -> do
        pages <- readPagesFrom 1
        return (JpegPages pages)
    return result

{- | Shedules rendering od a file. After forked process is done, images will be put in shared memory.
 FIXME: this is so convoluted that I'm getting lost in this function. Make it clear.
 -}
maybeScheduleRendering :: Kontrakcja m
                       => FileID
                       -> m JpegPages
maybeScheduleRendering fileid = do
  Context{ctxnormalizeddocuments} <- getContext

  -- Some debugs
  Log.debug $ "Rendering is being scheduled for file #" ++ show fileid
  pgs <- MemCache.size ctxnormalizeddocuments
  Log.debug $ "Total rendered pages count: " ++ show (pgs)

  -- Propper action
  v <- MemCache.get fileid ctxnormalizeddocuments
  case v of
      Just pages -> return pages
      Nothing -> do
          MemCache.put fileid JpegPagesPending ctxnormalizeddocuments
          forkAction ("Rendering file #" ++ show fileid) $ do
                   jpegpages <- convertPdfToJpgPages fileid 943            -- FIXME: We should report error somewere
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

preCheckPDFHelper :: BS.ByteString
                  -> String
                  -> IO (Either FileError BS.ByteString)
preCheckPDFHelper content tmppath =
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

      (exitcode,stdout1,stderr1) <- liftIO $ readProcessWithExitCode' "mubusy"
                                   [ "clean"
                                   , "-ggg"
                                   , sourcepath
                                   , normalizedpath
                                   ] BSL.empty
      when (exitcode /= ExitSuccess ) $ do
        liftIO $ do
          systmp <- getTemporaryDirectory
          (_path,handle) <- openTempFile systmp ("pre-normalize-failed-.pdf")
          BS.hPutStr handle content
          hClose handle

        throwError (FileNormalizeError (BSL.pack $ BSL.unpack stdout1 ++ " "++ BSL.unpack stderr1))

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
-- - Checks if the file is not too large.
--
-- - Checks if beggining bytes are '%PDF-1.' designating a PDF format.
--
-- - Normalizes using mubusy clean command. This is required as we
-- need to process 1.4 version documents maximum that have
-- uncompressed structure.
--
-- - Tries to do pdfseal process with empty data, so we know in
-- advance if it did work or not.
--
-- Return value is either a 'BS.ByteString' with normalized document
-- content or 'FileError' enumeration stating what is going on.
--
preCheckPDF :: BS.ByteString
            -> IO (Either FileError Binary)
preCheckPDF content =
  withSystemTempDirectory "precheck" $ \tmppath -> do
    value <- preCheckPDFHelper content tmppath
      `E.catch` \(e::IOError) -> return (Left (FileOtherError (show e)))
    case value of
      Left x -> Log.error $ "preCheckPDF: " ++ show x
      Right _ -> return ()
    return $ Binary <$> value


findStringAfterKey :: String -> BS.ByteString -> [String]
findStringAfterKey key content =
    (map (BSC.unpack . dropAfterKey) . findKeys) content
  where
    keyPacked = BSC.pack ("/" ++ key)
    keyLength1 = BSC.length keyPacked
    findKeys x = case BSC.breakSubstring keyPacked x of
                     (_, r) -> r : if BS.null r
                                   then []
                                   else findKeys (BSC.drop 1 r)
    dropAfterKey = BSC.drop keyLength1

getNumberOfPDFPages :: BS.ByteString -> Int
getNumberOfPDFPages content =
    maximum (1 : (catMaybes . map readNumber . findStringAfterKey "Count") content)
  where
    readNumber = maybeRead . takeWhile isDigit . dropWhile isSpace

getRotateOfPDFPages :: BS.ByteString -> [Int]
getRotateOfPDFPages content =
    (catMaybes . map readNumber . findStringAfterKey "Rotate") content
  where
    readNumber = maybeRead . takeWhile isDigit . dropWhile isSpace

getBoxSizesOfPDFPages :: String -> BS.ByteString -> [(Double,Double)]
getBoxSizesOfPDFPages box content =
  catMaybes (map maybeReadBox boxStrings)
  where
    boxStrings = findStringAfterKey box content
    maybeReadBox = x . catMaybes . map maybeRead . words . takeWhile (/=']') . drop 1 . dropWhile (/='[') . take 1000
    x [l,b,r,t] = Just (r-l,t-b)
    x _ = Nothing

getPageSizeOfPDFInPoints :: BS.ByteString -> (Double,Double)
getPageSizeOfPDFInPoints content =
  head (getPageSizeOfPDFInPointsList content ++ [(595, 842)])
     -- Defaults to A4

getPageSizeOfPDFInPointsList :: BS.ByteString -> [(Double,Double)]
getPageSizeOfPDFInPointsList content =
  if any isSwapping rotates
     then map swap (cropBoxes ++ mediaBoxes)
     else cropBoxes ++ mediaBoxes
  where
     mediaBoxes = getBoxSizesOfPDFPages "MediaBox" content
     cropBoxes = getBoxSizesOfPDFPages "CropBox" content
     rotates = getRotateOfPDFPages content
     swap (w,h) = (h,w)
     isSwapping rot = rot == 270 || rot == 90
