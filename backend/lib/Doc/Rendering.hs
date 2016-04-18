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

import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Catch hiding (handle)
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Char
import Data.Typeable
import Log
import Numeric
import System.Directory
import System.Exit
import System.IO
import System.Process hiding (readProcessWithExitCode)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Control.Concurrent.Thread.Lifted as LT
import qualified Control.Exception.Lifted as E
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Unjson as Unjson
import qualified System.IO.Temp

import DB
import Doc.Logging
import Doc.RenderedPages
import File.Model
import File.Storage
import ForkAction
import Kontra
import KontraPrelude
import qualified Amazon as AWS
import qualified MemCache as MemCache

data PageInfo = PageInfo {
    piPage :: !Int
  , piTime :: !Double
  } deriving (Eq, Ord, Show)

instance ToJSON PageInfo where
  toJSON PageInfo{..} = object [
      "page" .= piPage
    , "time" .= piTime
    ]

data RenderingStats = RenderingStats {
    rsTotal    :: !Double
  , rsFileSize :: !Int
  , rsPages    :: !Int
  , rsAverage  :: !Double
  , rsFastest  :: !PageInfo
  , rsSlowest  :: !PageInfo
  } deriving (Eq, Ord, Show)

instance ToJSON RenderingStats where
  toJSON RenderingStats{..} = object [
      "total"    .= rsTotal
    , "filesize" .= rsFileSize
    , "pages"    .= rsPages
    , "average"  .= rsAverage
    , "fastest"  .= rsFastest
    , "slowest"  .= rsSlowest
    ]

----------------------------------------

data RemoveJavaScriptSpec = RemoveJavaScriptSpec
    { input          :: String
    , output         :: String
    }
    deriving (Eq,Ord,Show,Read)

unjsonRemoveJavaScriptSpec :: Unjson.UnjsonDef RemoveJavaScriptSpec
unjsonRemoveJavaScriptSpec = Unjson.objectOf $ pure RemoveJavaScriptSpec
    <*> Unjson.field "input" input "Path for source document"
    <*> Unjson.field "output" output "Output path"

withSystemTempDirectory :: (MonadBaseControl IO m) => String -> (String -> m a) -> m a
withSystemTempDirectory = liftBaseOp . System.IO.Temp.withSystemTempDirectory

-- |Convert PDF to PNG images of pages
runRendering
  :: forall m. (KontraMonad m, MonadLog m, MonadDB m, MonadThrow m, MonadMask m, MonadBaseControl IO m, AWS.AmazonMonad m)
  => BS.ByteString
  -> Int
  -> RenderingMode
  -> RenderedPages
  -> m ()
runRendering fileContent widthInPixels renderingMode rp = do
  withSystemTempDirectory "mudraw" $ \tmpPath -> do
    let sourcePath = tmpPath ++ "/source.pdf"
    liftBase $ BS.writeFile sourcePath fileContent
    let pagePath n = tmpPath ++ "/output-" ++ show n ++ ".png"
        -- Run mutool through stdbuf and set its output buffering to
        -- line buffering as communication via pipes uses block
        -- buffering by default and we would not be able to get page
        -- info immediately.
        mutoolDraw = (proc "stdbuf" $ concat [
            ["-oL"]
          , ["mutool", "draw"]
          , ["-o", tmpPath ++ "/output-%d.png"]
          , ["-w", show widthInPixels]
          , ["-A", "8"]
          , ["-st"]
          , [sourcePath]
          , ["1" | RenderingModeFirstPageOnly <- return renderingMode]
          ]) {
            std_out = CreatePipe
          , std_err = CreatePipe
          }

    (Nothing, Just hout, Just herr, ph) <- liftBase $ createProcess_ "mutool draw" mutoolDraw

    -- Collect stderr in parallel to stdout so that its buffer won't get full.
    (_, getStdErr) <- liftBase (LT.fork $ E.evaluate . force =<< LT.hGetContents herr)

    let cleanupProcess = do
          (err, ec) <- liftBase $ do
            err <- getStdErr
            hClose hout
            hClose herr
            ec <- waitForProcess ph
            return (err, ec)
          logInfo "Rendering completed" $ object [
              "code" .= show ec
            , case err of
                Right msg -> "stderr" .= msg
                Left ex   -> "error"  .= show ex
            ]

    (`finally` cleanupProcess) $ do
      fetchPages pagePath (T.pack sourcePath) (pagesCount rp) hout
      fetchStatistics hout
  where
    fetchStatistics :: Handle -> m ()
    fetchStatistics h = do
      stats <- liftBase $ T.hGetContents h
      case P.parseOnly (renderingStatsParser $ BS.length fileContent) stats of
        Right rs -> logInfo "Rendering statistics" $ toJSON rs
        Left err -> logAttention "Couldn't parse RenderingStats" $ object [
            "error" .= err
          , "stdout" .= stats
          ]

    fetchPages :: (Int -> FilePath) -> T.Text -> Int -> Handle -> m ()
    fetchPages pagePath sourcePath n h = go 1
      where
        go k | k > n = return ()
        go k = liftBase (hIsEOF h) >>= \case
          True  -> return ()
          False -> do
            line <- liftBase $ T.hGetLine h
            case P.parseOnly (pageInfoParser sourcePath) line of
              Left _ -> do
                logInfo "Mutool returned something else than PageInfo" $ object [
                    "stdout" .= line
                  ]
                go k
              Right info@PageInfo{..} -> localData ["page" .= piPage] $ do
                content <- liftBase (E.try . BS.readFile $ pagePath piPage) >>= \case
                  Right content -> return content
                  Left (e::IOError) -> do
                    logAttention "Couldn't open page file" $ object [
                        "error" .= show e
                      ]
                    return BS.empty
                putPage rp piPage content >>= \case
                  True  -> logInfo "Page retrieved successfully" $ toJSON info
                  False -> logAttention_ "Page already in place"
                go $ k + 1

    msToSecsM :: P.Parser Double -> P.Parser Double
    msToSecsM = fmap (/ 1000)

    renderingStatsParser :: Int -> P.Parser RenderingStats
    renderingStatsParser fileSize = pure RenderingStats
      <* P.string "total "
      <*> msToSecsM P.double
      <*> pure fileSize
      <* P.string "ms / "
      <*> P.decimal
      <* P.string " pages for an average of "
      <*> msToSecsM P.double
      <* P.string "ms"
      <* P.endOfLine
      <* P.string "fastest "
      <*> edgeCase
      <* P.endOfLine
      <* P.string "slowest "
      <*> edgeCase
      where
        edgeCase :: P.Parser PageInfo
        edgeCase = pure PageInfo
          <* P.string "page "
          <*> P.decimal
          <* P.string ": "
          <*> msToSecsM P.double
          <* P.string "ms"

    pageInfoParser :: T.Text -> P.Parser PageInfo
    pageInfoParser pdf = pure PageInfo
      <* P.string "page "
      <* P.string pdf
      <* P.space
      <*> P.decimal
      <* P.space
      <*> msToSecsM P.double
      <* P.string "ms"


-- | 'getRenderedPages' returns 'RenderedPages' for document 'fileid'
-- requested to be rendered with width 'pageWidthInPixels' and also a
-- flag if full document should be rendered or only first page is
-- enough.
--
-- This function has internal caching system based on 'Context'
getRenderedPages :: Kontrakcja m
                 => FileID
                 -> Int
                 -> RenderingMode
                 -> m RenderedPages
getRenderedPages fid pageWidthInPixels renderingMode = logFile fid $ do
  let clampedPageWidthInPixels =
        min 4000 (max 100 pageWidthInPixels)
  Context{ctxnormalizeddocuments} <- getContext
  let key = (fid, clampedPageWidthInPixels, renderingMode)
  MemCache.fetch ctxnormalizeddocuments key $ do
    fileContent <- getFileIDContents fid
    liftBase (getNumberOfPDFPages fileContent) >>= \case
      Left err -> do
        logAttention "getNumberOfPDFPages failed" $ object [
            "error" .= err
          ]
        internalError
      Right pagesNo -> do
        rp <- renderedPages pagesNo
        forkAction "Rendering file" $ do
          runRendering fileContent clampedPageWidthInPixels renderingMode rp
        return rp

data FileError = FileSizeError Int Int
               | FileFormatError
               | FileNormalizeError BSL.ByteString
               | FileRemoveJavaScriptError BSL.ByteString
               | FileSealingError BSL.ByteString
               | FileOtherError String
               deriving (Eq, Ord, Show, Read, Typeable)

preCheckPDFHelper :: BS.ByteString
                  -> String
                  -> IO (Either FileError BS.ByteString)
preCheckPDFHelper content tmppath =
    runExceptT $ do
      checkSize
      checkHeader
      checkRemoveJavaScript
      checkNormalize
      readOutput
  where
    sourcepath = tmppath ++ "/source.pdf"
    jsremovedpath = tmppath ++ "/jsremoved.pdf"
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

    checkRemoveJavaScript = do
      liftIO $ BS.writeFile sourcepath content
      (code,stdout1,stderr1) <- liftIO $ do
        let jsremovespecpath = tmppath ++ "/jsremove.json"
        let config = RemoveJavaScriptSpec { input = sourcepath, output = jsremovedpath }
        let json_config = Unjson.unjsonToByteStringLazy unjsonRemoveJavaScriptSpec config

        liftIO $ BSL.writeFile jsremovespecpath json_config
        readProcessWithExitCode "java" ["-jar", "scrivepdftools/scrivepdftools.jar", "remove-javascript", jsremovespecpath] (BSL.empty)
      case code of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          throwError $ FileRemoveJavaScriptError $ BSL.concat [ BSL.pack ("Exit failure \n")
                                                              , stdout1
                                                              , BSL.pack "\n"
                                                              , stderr1
                                                              ]


    checkNormalize = do

      -- dont rely on mutool exit code - for mupdf-1.6 it's always 1
      -- just check if the output file is there
      (_, stdout1, stderr1) <- liftIO $ readProcessWithExitCode "mutool"
                                   [ "clean"
                                   , "-gg"
                                   , jsremovedpath
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
preCheckPDF :: (MonadLog m, MonadBaseControl IO m) => BS.ByteString
            -> m (Either FileError (Binary BS.ByteString))
preCheckPDF content =
  liftBaseOp (withSystemTempDirectory "precheck") $ \tmppath -> do
    res <- liftBase (preCheckPDFHelper content tmppath)
      `E.catch` \(e::IOError) -> return (Left (FileOtherError (show e)))
    case res of
      Left x -> logAttention "preCheckPDF failed" $ object [
          "error" .= show x
        ]
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
  (exitCode, stdout', stderr') <- readProcessWithExitCode "mutool" ["info", path] BSL.empty
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
