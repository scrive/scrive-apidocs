module Log.Model where

import DB
import KontraPrelude
import MinutesTime

data CleanLogsOlderThanDays = CleanLogsOlderThanDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m CleanLogsOlderThanDays Int where
  update (CleanLogsOlderThanDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "logs" $ do
      sqlWhere $ "time <=" <?> past
