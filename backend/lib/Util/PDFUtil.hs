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
    , getNumberOfPDFPages
    , pickPages
    ) where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Typeable
import Log
import Numeric
import System.Directory
import System.Exit
import System.IO
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import System.Timeout.Lifted
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Unjson as Unjson

import KontraPrelude
import Log.Utils
import Utils.Directory

data RemoveJavaScriptSpec = RemoveJavaScriptSpec
    { input          :: String
    , output         :: String
    }
    deriving (Eq,Ord,Show,Read)

unjsonRemoveJavaScriptSpec :: Unjson.UnjsonDef RemoveJavaScriptSpec
unjsonRemoveJavaScriptSpec = Unjson.objectOf $ pure RemoveJavaScriptSpec
    <*> Unjson.field "input" input "Path for source document"
    <*> Unjson.field "output" output "Output path"

renderPage :: forall m. (MonadBaseControl IO m, MonadLog m)
  => BS.ByteString
  -> Int
  -> Int
  -> m (Maybe BS.ByteString)
renderPage fileContent pageNo widthInPixels = do
  restrictTime $ withSystemTempDirectory' "mudraw" $ \tmpPath -> do
    let sourcePath = tmpPath ++ "/source.pdf"
    liftBase $ BS.writeFile sourcePath fileContent
    let pagePath = tmpPath ++ "/output-" ++ show pageNo ++ ".png"
        -- Run mutool through stdbuf and set its output buffering to
        -- line buffering as communication via pipes uses block
        -- buffering by default and we would not be able to get page
        -- info immediately.
    (mudrawcode,mudrawout,mudrawerr) <- liftBase $ readProcessWithExitCode "mutool" (concat [
            ["draw"]
          , ["-o", tmpPath ++ "/output-%d.png"]
          , ["-w", show widthInPixels]
          , ["-A", "8"]
          , ["-st"]
          , [sourcePath]
          , [show pageNo]
          ]) (BSL.empty)
    case mudrawcode of
      ExitSuccess -> do
        (liftBase (Just <$> BS.readFile pagePath))
          `E.catch` \(_::IOError) -> do
            -- mupdf will return last page if pdf has less pages - and not trigger content.
            -- We detect this issue with IOError - output file will have name with real page
            -- number and not requested one.
            logAttentionWithPage "Reading page content failed. Probably PDF has less pages." []
            return Nothing
      _ -> do
        logAttentionWithPage "Rendering of page failed" $ [
          "exit_code" .= show mudrawcode,
          "stdout" `equalsExternalBSL` mudrawout,
          "stderr" `equalsExternalBSL` mudrawerr
          ]
        return Nothing
  where
    logAttentionWithPage msg l = logAttention msg (object (("page" .= pageNo) : l))
    restrictTime ::  (MonadBaseControl IO m, MonadLog m) => m (Maybe r) -> m (Maybe r)
    restrictTime m = timeout (30 * 1000000) m >>= \case
      Just r  -> return r
      Nothing -> do
        logAttentionWithPage "Rendering didn't finish in 30 seconds, aborting" []
        return Nothing

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
            -> m (Either FileError BS.ByteString)
preCheckPDF content =
  withSystemTempDirectory' "precheck" $ \tmppath -> do
    res <- liftBase (preCheckPDFHelper content tmppath)
      `E.catch` \(e::IOError) -> return (Left (FileOtherError (show e)))
    case res of
      Left x -> logAttention "preCheckPDF failed" $ object [
          "error" .= show x
        ]
      Right _ -> return ()
    return res


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

pickPages :: (MonadLog m, MonadBaseControl IO m) => [Int] -> BS.ByteString -> m (Maybe  BS.ByteString)
pickPages pages content = do
  withSystemTempDirectory' "remove-pages" $ \tmppath -> do
    let inputpath = tmppath ++ "/input.pdf"
    let outputpath = tmppath ++ "/output.pdf"
    liftBase $ BS.writeFile inputpath content
    (exitCode, mutoolout, mutoolerr)  <- liftBase $ readProcessWithExitCode "mutool" (["clean", "-g", inputpath, outputpath] ++ (map show pages)) (BSL.empty)
    case exitCode of
      ExitSuccess -> Just <$> (liftBase $ BS.readFile outputpath)
      ExitFailure ec -> do
        logAttention "pickPages failed" $ object [
          "exit_code" .= show ec,
          "stdout" `equalsExternalBSL` mutoolout,
          "stderr" `equalsExternalBSL` mutoolerr
          ]
        return Nothing

