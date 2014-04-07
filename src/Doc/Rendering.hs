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
    , FileError(..)
    , preCheckPDF
    , getNumberOfPDFPages
    , getPageSizeOfPDFInPoints
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Data.Typeable
import DB
import Doc.RenderedPages
import File.Model
import ForkAction
import Kontra
import Utils.IO
import Utils.Read
import Utils.String
import System.Directory
import System.Exit
import System.IO
import qualified System.IO.Temp
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Log
import qualified Amazon as AWS
import qualified MemCache as MemCache
import File.Storage
import Data.Char
import Data.Maybe
import Control.Monad.Trans.Control
--import Control.Monad.Base

withSystemTempDirectory :: (MonadBaseControl IO m) => String -> (String -> m a) -> m a
withSystemTempDirectory = liftBaseOp . System.IO.Temp.withSystemTempDirectory

{- |
   Convert PDF to jpeg images of pages
 -}
convertPdfToJpgPages :: forall m . (KontraMonad m, Log.MonadLog m, MonadDB m, MonadIO m, MonadBaseControl IO m, AWS.AmazonMonad m)
                     => FileID
                     -> Int
                     -> Bool
                     -> m RenderedPages
convertPdfToJpgPages fid widthInPixels wholeDocument = do
  content <- getFileIDContents fid
  withSystemTempDirectory "mudraw" $ \tmppath -> do
    let sourcepath = tmppath ++ "/source.pdf"

    liftIO $ BS.writeFile sourcepath content

    (exitcode,outcontent,errcontent) <-
      liftIO $ readProcessWithExitCode' "mudraw"
               (concat [ ["-o", tmppath ++ "/output-%d.png"]
                       , ["-w", show widthInPixels]
                       , ["-b", "8"]
                       , [sourcepath]
                       , ["1" | False <- return wholeDocument] -- render only first page if so desired
                       ]) BSL.empty

    let pathOfPage n = tmppath ++ "/output-" ++ show n ++ ".png"

    -- readPagesFrom opens a file. If the file does not exists then it
    -- stops, if open is successful the file is prepended to a list of
    -- files and a file with number larger by 1 is tried
    let readPagesFrom :: Int -> m [BS.ByteString]
        readPagesFrom n = do
          econtentx <- E.try (liftIO $ BS.readFile (pathOfPage n))
          case econtentx of
            Right contentx -> do
               followingPages <- readPagesFrom (n+1 :: Int)
               return $ (contentx : followingPages)
            Left (_ :: IOError) -> do
               -- we could not open this file, it means it does not exists
               return []

    result <- case exitcode of
      ExitFailure _ -> liftIO $ do
        systmp <- getTemporaryDirectory
        (path,handle) <- openTempFile systmp ("mudraw-failed-" ++ show fid ++ "-.pdf")
        Log.attention_ $ "Cannot mudraw of file #" ++ show fid ++ ": " ++ path
        BS.hPutStr handle content
        hClose handle

        return $ RenderedPagesError (concatChunks errcontent `BS.append` concatChunks outcontent)
      ExitSuccess -> do
        pages <- readPagesFrom 1
        return (RenderedPages pages)
    return result

{- | Shedules rendering od a file. After forked process is done, images will be put in shared memory.
 FIXME: this is so convoluted that I'm getting lost in this function. Make it clear.
 -}
maybeScheduleRendering :: Kontrakcja m
                       => FileID
                       -> Int
                       -> Bool
                       -> m RenderedPages
maybeScheduleRendering fileid pageWidthInPixels wholeDocument = do
  Context{ctxnormalizeddocuments} <- getContext

  -- Some debugs
  Log.mixlog_ $ "Rendering is being scheduled for file #" ++ show fileid
  pgs <- MemCache.size ctxnormalizeddocuments
  Log.mixlog_ $ "Total rendered pages count: " ++ show (pgs)

  -- Propper action
  let key = (fileid,pageWidthInPixels,wholeDocument)
  v <- MemCache.get key ctxnormalizeddocuments
  case v of
      Just pages -> return pages
      Nothing -> do
          MemCache.put key (RenderedPagesPending []) ctxnormalizeddocuments
          forkAction ("Rendering file #" ++ show fileid) $ do
                   jpegpages <- convertPdfToJpgPages fileid pageWidthInPixels wholeDocument            -- FIXME: We should report error somewere
                   MemCache.put key jpegpages ctxnormalizeddocuments
          return (RenderedPagesPending [])


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
      readOutput
  where
    sourcepath = tmppath ++ "/source.pdf"
    normalizedpath = tmppath ++ "/normalized.pdf"

    sizeLimit = 10 * 1000 * 1000
    contentLength = BS.length content

    headerPattern = BS.pack "%PDF-1."

    checkSize = do
      when (contentLength > sizeLimit) $
           throwError (FileSizeError sizeLimit contentLength)

    checkHeader = do
      when (not $ headerPattern `BS.isPrefixOf` content) $ do
        throwError (FileFormatError)

    checkNormalize = do
      liftIO $ BS.writeFile sourcepath content

      (exitcode,stdout1,stderr1) <- liftIO $ readProcessWithExitCode' "mutool"
                                   [ "clean"
                                   , "-ggg"
                                   , sourcepath
                                   , normalizedpath
                                   ] BSL.empty
      case exitcode of
        ExitSuccess -> return ()
        ExitFailure code -> do
          liftIO $ do
            systmp <- getTemporaryDirectory
            (_path,handle) <- openTempFile systmp ("pre-normalize-failed-.pdf")
            BS.hPutStr handle content
            hClose handle

          throwError (FileNormalizeError (BSL.pack ("Exit code " ++ show code ++ "\n") `BSL.append`
                                          stdout1 `BSL.append`
                                          BSL.pack "\n" `BSL.append`
                                          stderr1))

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
            -> IO (Either FileError (Binary BS.ByteString))
preCheckPDF content =
  withSystemTempDirectory "precheck" $ \tmppath -> do
    res <- preCheckPDFHelper content tmppath
      `E.catch` \(e::IOError) -> return (Left (FileOtherError (show e)))
    case res of
      Left x -> Log.attention_ $ "preCheckPDF: " ++ show x
      Right _ -> return ()
    return $ Binary <$> res

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
