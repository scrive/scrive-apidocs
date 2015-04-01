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
    ( getRenderedPages
    , FileError(..)
    , preCheckPDF
    , getNumberOfPDFPages
    , getPageSizeOfPDFInPoints
    ) where

import Control.Monad.Base
import Control.Monad.Catch hiding (handle)
import Control.Monad.Error
import Control.Monad.Trans.Control
import Data.Char
import Data.Typeable
import Numeric
import System.Directory
import System.Exit
import System.IO
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified System.IO.Temp

import DB
import Doc.RenderedPages
import File.Model
import File.Storage
import ForkAction
import Kontra
import KontraPrelude
import Utils.IO
import Utils.Read
import qualified Amazon as AWS
import qualified Log
import qualified MemCache as MemCache

withSystemTempDirectory :: (MonadBaseControl IO m) => String -> (String -> m a) -> m a
withSystemTempDirectory = liftBaseOp . System.IO.Temp.withSystemTempDirectory


readNumberedFiles :: (MonadBaseControl IO m) => (Int -> FilePath) -> Int -> [BS.ByteString] -> m [BS.ByteString]
readNumberedFiles pathFromNumber currentNumber accum = do
  econtentx <- E.try (liftBase $ BS.readFile (pathFromNumber currentNumber))
  case econtentx of
    Right contentx -> do
       readNumberedFiles pathFromNumber (succ currentNumber) (contentx : accum)
    Left (_ :: IOError) -> do
       -- we could not open this file, it means it does not exists
       return (reverse accum)


{- |
   Convert PDF to jpeg images of pages
 -}
runRendering :: (KontraMonad m, Log.MonadLog m, MonadDB m, MonadThrow m, MonadIO m, MonadBaseControl IO m, AWS.AmazonMonad m)
                     => RenderedPagesCache
                     -> FileID
                     -> Int
                     -> RenderingMode
                     -> m RenderedPages
runRendering _renderedPages fid widthInPixels renderingMode = do
  content <- getFileIDContents fid
  withSystemTempDirectory "mudraw" $ \tmppath -> do
    let sourcepath = tmppath ++ "/source.pdf"

    liftIO $ BS.writeFile sourcepath content

    let pathOfPage n = tmppath ++ "/output-" ++ show n ++ ".png"

    -- Note: Infrstructure here is prepared for existence of
    -- background thread that will sniff already rendered
    -- pages. Although it is not that easy to do that, as that thread
    -- will need to wait for a file (n+1) to appear before reading
    -- file (n). Potentials for race conditions also abound. Remember
    -- about final condition as this is not clear when this thread is
    -- supposed to finish.

    (exitcode,_outcontent,_errcontent) <-
      liftIO $ readProcessWithExitCode' "mudraw"
               (concat [ ["-o", tmppath ++ "/output-%d.png"]
                       , ["-w", show widthInPixels]
                       , ["-b", "8"]
                       , [sourcepath]
                       , ["1" | RenderingModeFirstPageOnly <- return renderingMode] -- render only first page if so desired
                       ]) BSL.empty

    when (exitcode /= ExitSuccess) $
      Log.attention_ $ "mudraw process for pdf->png render failed, will look for rendered images anyway"
    pages <- readNumberedFiles pathOfPage 1 []
    when (null pages) $ do
      systmp <- liftIO $ getTemporaryDirectory
      (path, handle) <- liftIO $ openTempFile systmp $ "mudraw-failed-" ++ show fid ++ "-.pdf"
      Log.attention_ $ "Cannot mudraw of file #" ++ show fid ++ ": " ++ path
      liftIO $ BS.hPutStr handle content
      liftIO $ hClose handle
    return $ RenderedPages True pages


-- | 'getRenderedPages' returns 'RenderedPages' for document 'fileid'
-- requested to be rendered with width 'pageWidthInPixels' and also a
-- flag if full document should be rendered or only first page is
-- enough.
--
-- This function return immediatelly, so look for 'RenderedPages'
-- first argument to see if the page that is requested has already
-- been rendered. 'RenderedPages' is the final state of rendering.
--
-- This function has internal caching system based on 'Context'
getRenderedPages :: Kontrakcja m
                 => FileID
                 -> Int
                 -> RenderingMode
                 -> m RenderedPages
getRenderedPages fileid pageWidthInPixels renderingMode = do
  let clampedPageWidthInPixels =
        min 4000 (max 100 pageWidthInPixels)
  Context{ctxnormalizeddocuments} <- getContext
  -- Propper action
  let key = (fileid,clampedPageWidthInPixels,renderingMode)
  v <- MemCache.get key ctxnormalizeddocuments
  case v of
    Just pages -> return pages
    Nothing -> do
      MemCache.put key (RenderedPages False []) ctxnormalizeddocuments
      forkAction ("Rendering file #" ++ show fileid) $ do
        -- FIXME: We should report error somewere
        jpegpages <- runRendering ctxnormalizeddocuments fileid clampedPageWidthInPixels renderingMode
        MemCache.put key jpegpages ctxnormalizeddocuments
      return (RenderedPages False [])


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

      -- dont rely on mutool exit code - for mupdf-1.6 it's always 1
      -- just check if the output file is there
      (_, stdout1, stderr1) <- liftIO $ readProcessWithExitCode' "mutool"
                                   [ "clean"
                                   , "-ggg"
                                   , sourcepath
                                   , normalizedpath
                                   ] BSL.empty
      flag <- liftIO $ doesFileExist normalizedpath
      when (not flag) $ do
        liftIO $ do
          systmp <- getTemporaryDirectory
          (_path,handle) <- openTempFile systmp ("pre-normalize-failed-.pdf")
          BS.hPutStr handle content
          hClose handle

        throwError $ FileNormalizeError $ BSL.concat [ BSL.pack ("Exit failure \n")
                                                     , stdout1
                                                     , BSL.pack "\n"
                                                     , stderr1
                                                     ]

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
preCheckPDF :: (Log.MonadLog m, MonadBaseControl IO m) => BS.ByteString
            -> m (Either FileError (Binary BS.ByteString))
preCheckPDF content =
  liftBaseOp (withSystemTempDirectory "precheck") $ \tmppath -> do
    res <- liftBase (preCheckPDFHelper content tmppath)
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

getNumberOfPDFPages :: BS.ByteString -> IO (Either String Int)
getNumberOfPDFPages content = do
  systmp <- getTemporaryDirectory
  (path, handle) <- openTempFile systmp "mutool-input.pdf"
  BS.hPutStr handle content
  hClose handle
  (exitCode, stdout', stderr') <- readProcessWithExitCode' "mutool" ["info", "-m", path] BSL.empty
  removeFile path
  return $ case exitCode of
    ExitSuccess -> case find ("Pages: " `BSL.isPrefixOf`) $ BSL.lines stdout' of
                    Just line -> case readDec $ BSL.unpack $ BSL.drop (BSL.length "Pages: ") line of
                                  [(x, "")] -> Right x
                                  _ -> Left $ "Unparsable mutool info output about number of pages"
                    Nothing -> Left $ "Couldn't find number of pdf pages in mutool output"
    ExitFailure code -> Left $ "mutool info failed with return code " ++ show code ++ ", and stderr: " ++ BSL.unpack stderr'

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
  $head (getPageSizeOfPDFInPointsList content ++ [(595, 842)])
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
