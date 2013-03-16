

module DB.Time
where

import DB.Core
import MinutesTime
import System.Time (getClockTime)
import Control.Monad.IO.Class

-- | Get current time as 'MinutesTime'. Warning: server should work in UTC time.
getMinutesTime :: MonadDB m => m MinutesTime
getMinutesTime = DBT $ liftIO $ (return . fromClockTime) =<< getClockTime
