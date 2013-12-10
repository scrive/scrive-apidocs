module ThirdPartyStats.Migrations where

import DB
import ThirdPartyStats.Tables

asyncEventQueueChangePrimaryKeyToBigSerial :: MonadDB m => Migration m
asyncEventQueueChangePrimaryKeyToBigSerial = Migration {
    mgrTable = tableAsyncEventQueue
  , mgrFrom = 0
  , mgrDo = do
    runSQL_ "ALTER TABLE async_event_queue ALTER COLUMN sequence_number TYPE BIGINT"
}
