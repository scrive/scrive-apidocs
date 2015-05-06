module Log.Backend.StandardOutput (stdoutLogger) where

import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Time
import System.Locale
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import KontraPrelude
import Log.Data
import Log.Logger

stdoutLogger :: IO Logger
stdoutLogger = mkLogger "stdout" printLogMessage

printLogMessage :: LogMessage -> IO ()
printLogMessage LogMessage{..} = T.putStrLn . T.concat $ [
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime
  , " "
  , textifyLevel lmLevel
  , " "
  , lmComponent
  , ": "
  , lmMessage
  ] ++ if lmData == emptyObject
    then []
    else [" ", textifyData lmData]
  where
    textifyData :: Value -> T.Text
    textifyData = T.decodeUtf8 . toStrict . encodePretty' defConfig {
      confIndent = 2
    }

    textifyLevel :: LogLevel -> T.Text
    textifyLevel LogAttention = "ATTENTION"
    textifyLevel LogInfo = "INFO"
    textifyLevel LogTrace = "TRACE"
