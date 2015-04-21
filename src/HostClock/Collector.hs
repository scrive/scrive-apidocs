{-# OPTIONS_GHC -Wwarn #-}
module HostClock.Collector(collectClockError) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as E

import DB (dbUpdate, MonadDB)
import HostClock.Model (InsertClockOffsetFrequency(..))
import HostClock.System (getOffset, getFrequency)
import KontraPrelude
import Log

-- | Update the statistics for the host's clock error versus reference NTP servers.
collectClockError :: (MonadDB m, MonadBaseControl IO m, MonadLog m) => [String] -> m ()
collectClockError ntpServers = do
  moffset <- (Just <$> liftBase (getOffset ntpServers)) `E.catch`
                \(E.SomeException e) -> do
    logError_ $ "HostClock.collectClockError: getOffset failed: " ++ show e
    return Nothing
  freq <- liftBase getFrequency
  _ <- dbUpdate $ InsertClockOffsetFrequency moffset freq
  return ()
