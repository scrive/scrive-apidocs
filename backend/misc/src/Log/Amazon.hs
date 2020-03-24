module Log.Amazon
  ( awsLogger
  ) where

import Data.Aeson.Types
import Log
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.AWS as AWS

awsLogger :: MonadLog m => m AWS.Logger
awsLogger = do
  logger <- getLoggerIO
  return $ \awsLevel builder -> do
    now <- currentTime
    let level = adjustLevel awsLevel
    -- We are not interested in trace messages.
    when (level /= LogTrace) $ logger now level (builderToText builder) emptyObject
  where
    adjustLevel AWS.Error = LogAttention
    adjustLevel AWS.Info  = LogInfo
    adjustLevel AWS.Debug = LogInfo
    adjustLevel AWS.Trace = LogTrace

    builderToText =
      either (T.append "decode error: " . showt) identity
        . T.decodeUtf8'
        . BSL.toStrict
        . B.toLazyByteString
