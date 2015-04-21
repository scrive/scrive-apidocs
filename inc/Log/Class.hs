{-# LANGUAGE OverlappingInstances #-}
module Log.Class (
    UTCTime
  , MonadTime(..)
  , MonadLog(..)
  , logError
  , logInfo
  , logTrace
  , logError_
  , logInfo_
  , logTrace_
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types
import Data.Time

import Control.Monad.Trans.Control.Util
import Data.Time.Monad
import KontraPrelude
import Log.Data

-- | Represents the family of monads with logging capabilities.
class MonadTime m => MonadLog m where
  logMessage :: UTCTime -> LogLevel -> String -> Value -> m ()
  localData :: [Pair] -> m a -> m a

-- | Generic, overlapping instance.
instance (
    MonadLog m
  , Monad (t m)
  , MonadTransControl t
  ) => MonadLog (t m) where
    logMessage time level message = lift . logMessage time level message
    localData data_ m = controlT $ \run -> localData data_ (run m)

----------------------------------------

logError :: MonadLog m => String -> Value -> m ()
logError = logNow LogError

logInfo :: MonadLog m => String -> Value -> m ()
logInfo = logNow LogInfo

logTrace :: MonadLog m => String -> Value -> m ()
logTrace = logNow LogTrace

logError_ :: MonadLog m => String -> m ()
logError_ = (`logError` emptyObject)

logInfo_ :: MonadLog m => String -> m ()
logInfo_ = (`logInfo` emptyObject)

logTrace_ :: MonadLog m => String -> m ()
logTrace_ = (`logTrace` emptyObject)

----------------------------------------

logNow :: MonadLog m => LogLevel -> String -> Value -> m ()
logNow level message data_ = do
  time <- currentTime
  logMessage time level message data_
