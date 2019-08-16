{-# OPTIONS_GHC -Wwarn #-}
module HostClock.Collector(collectClockError) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Log
import qualified Control.Exception.Lifted as E

import DB (MonadDB, dbUpdate)
import HostClock.Model (InsertClockOffsetFrequency(..))
import HostClock.System (getFrequency, getOffset)

-- | Update the statistics for the host's clock error versus reference NTP servers.
collectClockError :: (MonadDB m, MonadBaseControl IO m, MonadLog m) => [Text] -> m ()
collectClockError ntpServers = do
  moffset <- (Just <$> liftBase (getOffset ntpServers)) `E.catch` \(E.SomeException e) -> do
    logAttention "HostClock.collectClockError: getOffset failed" $ object [
        "error" .= show e
      ]
    return Nothing
  freq <- liftBase getFrequency
  void $ dbUpdate $ InsertClockOffsetFrequency moffset freq
  return ()
