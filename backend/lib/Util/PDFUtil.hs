-----------------------------------------------------------------------------
-- |
-- Module      :  Util.PDFUtil
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  not portable
--
-----------------------------------------------------------------------------
module Util.PDFUtil
    ( renderPage
    , FileError(..)
    , preCheckPDF
    , preCheckPDFs
    , getNumberOfPDFPages
    , pickPages
    , clipHighlightImageFromPage
    ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson
import Data.Typeable
import Log
import Numeric
import System.Directory
import System.Exit
import System.IO hiding (stderr, stdout)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import System.Timeout.Lifted
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T

import Kontra
import Log.Utils
import PdfToolsLambda.Class
import Utils.Directory

renderPage
  :: forall m
   . (MonadBaseControl IO m, MonadLog m)
  => BS.ByteString
  -> Int
  -> Int
  -> m (Maybe BS.ByteString)
renderPage fileContent pageNo widthInPixels = do
  restrictTime . withSystemTempDirectory' "mudraw" $ \tmpPath -> do
    let sourcePath = tmpPath <> "/source.pdf"
    logInfo "Temp file write" $ object
      ["bytes_written" .= BS.length fileContent, "originator" .= ("renderPage" :: Text)]
    liftBase $ BS.writeFile sourcePath fileContent
    let pagePath      = tmpPath <> "/output-" <> show pageNo <> ".png"
        pageQuantPath = tmpPath <> "/output-quant-" <> show pageNo <> ".png"
    (mudrawcode, mudrawout, mudrawerr) <- liftBase $ readProcessWithExitCode
      "mutool"
      (concat
        [ ["draw"]
        , ["-o", tmpPath <> "/output-%d.png"]
        , ["-w", show widthInPixels]
        , ["-A", "8"]
        , ["-st"]
        , [sourcePath]
        , [show pageNo]
        ]
      )
      BSL.empty
    mbs <- case mudrawcode of
      ExitSuccess -> do
        let successHandler
              | widthInPixels <= 1040 = liftBase $ Just <$> BS.readFile pagePath
              | otherwise = do
                (pngqcode, pngqout, pngqerr) <- liftBase $ readProcessWithExitCode
                  "pngquant"
                  ["--speed", "10", pagePath, "--output", pageQuantPath]
                  BSL.empty
                case pngqcode of
                  ExitSuccess -> liftBase $ Just <$> BS.readFile pageQuantPath
                  _           -> do
                    logAttentionWithPage
                      "Quanting of page failed"
                      [ "exit_code" .= show pngqcode
                      , "stdout" `equalsExternalBSL` pngqout
                      , "stderr" `equalsExternalBSL` pngqerr
                      ]
                    -- fallback to unquanted (?) file
                    liftBase $ Just <$> BS.readFile pagePath
            errorHandler (_ :: IOError) = do
                      -- mupdf will return last page if pdf has less pages - and not trigger content.
                      -- We detect this issue with IOError - output file will have name with real page
                      -- number and not requested one.
              logAttentionWithPage
                "Reading page content failed. Probably PDF has less pages."
                []
              return Nothing
        successHandler `E.catch` errorHandler
      _ -> do
        logAttentionWithPage
          "Rendering of page failed"
          [ "exit_code" .= show mudrawcode
          , "stdout" `equalsExternalBSL` mudrawout
          , "stderr" `equalsExternalBSL` mudrawerr
          ]
        return Nothing
    whenJust
      mbs
      (\bs -> logInfo "Temp file read"
        $ object ["bytes_read" .= BS.length bs, "originator" .= ("renderPage" :: Text)]
      )
    return mbs

  where
    logAttentionWithPage msg l = logAttention msg (object (("page" .= pageNo) : l))
    restrictTime :: (MonadBaseControl IO m, MonadLog m) => m (Maybe r) -> m (Maybe r)
    restrictTime m = timeout (30 * 1000000) m >>= \case
      Just r  -> return r
      Nothing -> do
        logAttentionWithPage "Rendering didn't finish in 30 seconds, aborting" []
        return Nothing

data FileError = FileSizeError Int Int
               | FileFormatError
               | FileNormalizeError BSL.ByteString
               | FileRemoveJavaScriptError
               | FileFlatteningError BSL.ByteString
               | FileSealingError BSL.ByteString
               | FileOtherError Text
               deriving (Eq, Ord, Show, Read, Typeable)

preCheckPDFsHelper
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadLog m
     , PdfToolsLambdaMonad m
     , KontraMonad m
     )
  => [BS.ByteString]
  -> Text
  -> m (Either FileError [BS.ByteString])
preCheckPDFsHelper contents tmppath = runExceptT $ do
  mapM_ checkSize contents
  checkRemoveJavaScript
  checkFlatten
  mapM_ checkNormalize $ zip [1 ..] contents
  mapM readOutput [1 .. length contents]
  where
    tmppath' = T.unpack tmppath

    flattenedpath :: Int -> String
    flattenedpath num = tmppath' <> "/flattened" <> show num <> ".pdf"
    jsremovedpath :: Int -> String
    jsremovedpath num = tmppath' <> "/jsremoved" <> show num <> ".pdf"
    normalizedpath :: Int -> String
    normalizedpath num = tmppath' <> "/normalized" <> show num <> ".pdf"

    sizeLimit = 10 * 1000 * 1000

    checkSize content =
      let contentLength = BS.length content
      in  when (contentLength > sizeLimit)
            $ throwError (FileSizeError sizeLimit contentLength)

    checkRemoveJavaScript = do
      forM_ (zip [1 ..] contents) $ \(num, content) -> do
        nc <- callPdfToolsCleaning $ BSL.fromStrict content
        case nc of
          Just c  -> liftBase $ BS.writeFile (jsremovedpath num) c
          Nothing -> throwError FileRemoveJavaScriptError

    checkFlatten = do
      let flattenerJar = "scrivepdftools/scrivepdftoolsflattener.jar"
      forM_ (take (length contents) [1 ..]) $ \num -> do
        (code, stdout, stderr) <- liftBase $ readProcessWithExitCode
          "java"
          ["-jar", flattenerJar, jsremovedpath num, flattenedpath num]
          BSL.empty
        case code of
          ExitSuccess   -> return ()
          ExitFailure _ -> do
            throwError . FileFlatteningError $ BSL.concat
              [BSL.pack "Exit failure \n", stdout, BSL.pack "\n", stderr]
    checkNormalize (num, content) = do
      -- dont rely on mutool exit code - for mupdf-1.6 it's always 1
      -- just check if the output file is there
      (_, stdout, stderr) <- liftBase $ readProcessWithExitCode
        "mutool"
        ["clean", "-gg", flattenedpath num, normalizedpath num]
        BSL.empty
      flag <- liftBase . doesFileExist $ normalizedpath num
      unless flag $ do
        liftBase $ do
          systmp     <- getTemporaryDirectory
          (_path, h) <- openTempFile systmp "pre-normalize-failed-.pdf"
          BS.hPutStr h content
          hClose h

        throwError . FileNormalizeError $ BSL.concat
          [BSL.pack "Exit failure \n", stdout, BSL.pack "\n", stderr]

    readOutput num = do
      bs <- liftBase . BS.readFile $ normalizedpath num
      logInfo "Temp file read" $ object
        ["bytes_read" .= BS.length bs, "originator" .= ("preCheckPDFsHelper" :: Text)]
      return bs

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
preCheckPDFs
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadLog m
     , PdfToolsLambdaMonad m
     , KontraMonad m
     )
  => [BS.ByteString]
  -> m (Either FileError [BS.ByteString])
preCheckPDFs contents = withSystemTempDirectory' "precheck" $ \tmppath -> do
  res <- preCheckPDFsHelper contents (T.pack tmppath)
    `E.catch` \(e :: IOError) -> return (Left (FileOtherError (showt e)))
  case res of
    Left  x -> logInfo "preCheckPDF failed" $ object ["error" .= show x]
    Right _ -> return ()
  return res

preCheckPDF
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadCatch m
     , MonadLog m
     , PdfToolsLambdaMonad m
     , KontraMonad m
     )
  => BS.ByteString
  -> m (Either FileError BS.ByteString)
preCheckPDF content = do
  res <- preCheckPDFs [content]
  case res of
    Left  e   -> return $ Left e
    Right [x] -> return $ Right x
    _         -> unexpectedError "preCheckPDFs returned wrong amount of contents"


getNumberOfPDFPages :: BS.ByteString -> IO (Either Text Int)
getNumberOfPDFPages content = do
  systmp    <- getTemporaryDirectory
  (path, h) <- openTempFile systmp "mutool-input.pdf"
  BS.hPutStr h content
  hClose h
  (exitCode, stdout', stderr') <- readProcessWithExitCode "mutool"
                                                          ["info", path]
                                                          BSL.empty
  removeFile path
  return $ case exitCode of
    ExitSuccess -> case find ("Pages: " `BSL.isPrefixOf`) $ BSL.lines stdout' of
      Just line -> case readDec . BSL.unpack $ BSL.drop (BSL.length "Pages: ") line of
        [(x, "")] -> Right x
        _         -> Left "Unparsable mutool info output about number of pages"
      Nothing -> Left "Couldn't find number of pdf pages in mutool output"
    ExitFailure code ->
      Left
        $  "mutool info failed with return code "
        <> showt code
        <> ", and stderr: "
        <> T.pack (BSL.unpack stderr')

pickPages
  :: (MonadLog m, MonadBaseControl IO m)
  => [Int]
  -> BS.ByteString
  -> m (Maybe BS.ByteString)
pickPages pages content = do
  withSystemTempDirectory' "remove-pages" $ \tmppath -> do
    let inputpath  = tmppath <> "/input.pdf"
    let outputpath = tmppath <> "/output.pdf"
    logInfo "Temp file write" $ object
      ["bytes_written" .= BS.length content, "originator" .= ("pickPages" :: Text)]
    liftBase $ BS.writeFile inputpath content
    (exitCode, mutoolout, mutoolerr) <- liftBase $ readProcessWithExitCode
      "mutool"
      (["clean", "-g", inputpath, outputpath] <> map show pages)
      BSL.empty
    case exitCode of
      ExitSuccess    -> Just <$> liftBase (BS.readFile outputpath)
      ExitFailure ec -> do
        logAttention "pickPages failed" $ object
          [ "exit_code" .= show ec
          , "stdout" `equalsExternalBSL` mutoolout
          , "stderr" `equalsExternalBSL` mutoolerr
          ]
        return Nothing

clipHighlightImageFromPage
  :: (MonadLog m, MonadBaseControl IO m)
  => BS.ByteString
  -> BS.ByteString
  -> Int
  -> m (Maybe BS.ByteString)
clipHighlightImageFromPage pdfFileContent highlightFileContent pageNo = do
  logInfo_ "clipHighlightImageFromPage: starting..."
  withSystemTempDirectory' "highlight-mask" $ \tmpPath -> do
    -- Set all the filepaths that we will use within tmpPath
    let pdfRenderedPagePath       = tmpPath <> "/pdf.png"
        highlightImagePath        = tmpPath <> "/highlight.png"
        thresholdOutputPath       = tmpPath <> "/threshold.png"
        maskedHighlightOutputPath = tmpPath <> "/masked_highlight.png"
    -- Write the highlight image and find its dimensions using 'identify'
    logInfo "Temp file write" $ object
      [ "bytes_written" .= BS.length highlightFileContent
      , "originator" .= ("clipHighlightImageFromPage" :: Text)
      ]
    liftBase $ BS.writeFile highlightImagePath highlightFileContent
    (identifyCode, identifyStdout, identifyStderr) <- liftBase
      $ readProcessWithExitCode "identify" ["-format", "(%w,%h)", highlightImagePath] ""
    case identifyCode of
      ExitFailure code -> do
        logAttention
            "clipHighlightImageFromPage: identify failed to get highlight dimensions"
          $ object
              [ "exit_code" .= code
              , "stdout" .= show identifyStdout
              , "stderr" .= show identifyStderr
              ]
        return Nothing
      ExitSuccess -> do
        case maybeRead (T.pack $ BSL.unpack identifyStdout) :: Maybe (Int, Int) of
          Nothing -> do
            logAttention_
              "clipHighlightImageFromPage: could not read dimensions from identify output"
            return Nothing
          Just (highlightWidth, highlightHeight) -> do
            -- Now get the rendered page with the same width
            mRenderedPageContents <- renderPage pdfFileContent pageNo highlightWidth
            case mRenderedPageContents of
              Nothing -> do
                logAttention_
                  "clipHighlightImageFromPage: Did not get rendered page contents"
                return Nothing
              Just renderedPageContents -> do
                      -- Write the rendered page that we have to file
                logInfo "Temp file write" $ object
                  [ "bytes_written" .= BS.length renderedPageContents
                  , "originator" .= ("clipHighlightImageFromPage" :: Text)
                  ]
                liftBase $ BS.writeFile pdfRenderedPagePath renderedPageContents
                -- First force resize the rendered page to match the highlight image
                -- then threshold at 50% (arbitrary, seemed to work well) to use as mask
                (thresholdCode, thresholdStdout, thresholdStderr) <-
                  liftBase $ readProcessWithExitCode
                    "convert"
                    [ pdfRenderedPagePath
                    , "-resize"
                    , show highlightWidth <> "x" <> show highlightHeight <> "!"
                    , "-threshold"
                    , "50%"
                    , thresholdOutputPath
                    ]
                    ""
                case thresholdCode of
                  ExitFailure code -> do
                    logAttention "clipHighlightImageFromPage: convert failed to threshold"
                      $ object
                          [ "exit_code" .= code
                          , "stdout" .= show thresholdStdout
                          , "stderr" .= show thresholdStderr
                          ]
                    return Nothing
                  ExitSuccess -> do
                    -- Then clip the highlight based on the underlying page...
                    (clipCode, clipStdOut, clipStderr) <-
                      liftBase $ readProcessWithExitCode
                        "convert"
                        [ "-channel"
                        , "Alpha"
                        , "-compose"
                        , "src-out"
                        , "-composite"
                        , "-transparent"
                        , "white"
                        , "-fuzz"
                        , "10%"
                        , thresholdOutputPath
                        , highlightImagePath
                        , "PNG32:" <> maskedHighlightOutputPath
                        ]
                        ""
                    case clipCode of
                      ExitFailure code -> do
                        logAttention "clipHighlightImageFromPage: convert failed to clip"
                          $ object
                              [ "exit_code" .= code
                              , "stdout" .= show clipStdOut
                              , "stderr" .= show clipStderr
                              ]
                        return Nothing
                      ExitSuccess -> do
                        maskedImageContents <- liftBase
                          $ BS.readFile maskedHighlightOutputPath
                        if BS.length maskedImageContents > 0
                          then do
                            logInfo_ "clipHighlightImageFromPage: done!"
                            return $ Just maskedImageContents
                          else do
                            logAttention_
                              "clipHighlightImageFromPage: final image had zero length"
                            return Nothing
