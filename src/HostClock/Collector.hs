module HostClock.Collector(collectClockError) where

import Control.Applicative ((<$>))
import qualified Control.Exception.Lifted as E
import DB (dbUpdate, MonadDB)
import HostClock.Model (InsertClockOffsetFrequency(..))
import HostClock.System (getOffset, getFrequency)
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Log

-- | Update the statistics for the host's clock error versus reference NTP servers.
collectClockError :: (MonadDB m, MonadBaseControl IO m, Log.MonadLog m) => [String] -> (forall a . m a -> m a) -> m ()
collectClockError ntpServers interruptible = do
  moffset <- (interruptible $ Just <$> liftBase (getOffset ntpServers)) `E.catch`
                \(E.SomeException e) -> do
    Log.attention_ $ "HostClock.collectClockError: getOffset failed: " ++ show e
    return Nothing
  freq <- interruptible $ liftBase getFrequency
  _ <- dbUpdate $ InsertClockOffsetFrequency moffset freq
  return ()
