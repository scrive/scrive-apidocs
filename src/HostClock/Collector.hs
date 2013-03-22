module HostClock.Collector(collectClockError) where

import Control.Applicative ((<$>))
import qualified Control.Exception as E
import DB (dbUpdate, MonadDB)
import HostClock.Model (InsertClockOffsetFrequency(..))
import HostClock.System (getOffset, getFrequency)
import Control.Monad.IO.Class

-- | Update the statistics for the host's clock error versus reference NTP servers.
collectClockError :: (MonadDB m, MonadIO m) => [String] -> (forall a . IO a -> IO a) -> m ()
collectClockError ntpServers interruptible = do
  moffset <- liftIO $ (interruptible $ Just <$> getOffset ntpServers) `E.catch`
                \E.SomeException{} -> return Nothing
  freq <- liftIO $ interruptible $ getFrequency
  _ <- dbUpdate $ InsertClockOffsetFrequency moffset freq
  return ()
