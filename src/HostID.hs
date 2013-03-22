module HostID
  ( HostID
  , getHostID
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO)
import DB (newtypeDeriveConvertible)
import System.Process (readProcess)

-- | String that uniquely identifies the host the service is running on.
newtype HostID = HostID String
  deriving (Show, Eq, Ord)

$(newtypeDeriveConvertible ''HostID)

getHostID :: MonadIO m => m HostID
getHostID = liftIO $ HostID <$> filter (/='\n') `fmap` readProcess "/bin/hostname" [] ""
