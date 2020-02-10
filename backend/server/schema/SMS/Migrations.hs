module SMS.Migrations
  ( removeDataFromSmses
  , smsesAddStatsIndexes
  ) where

import DB
import SMS.Tables

removeDataFromSmses :: MonadDB m => Migration m
removeDataFromSmses = Migration
  { mgrTableName = tblName tableSMSes
  , mgrFrom      = 5
  , mgrAction    = StandardMigration $ do
                     runSQL_ "ALTER TABLE smses DROP COLUMN data"
  }

smsesAddStatsIndexes :: MonadDB m => Migration m
smsesAddStatsIndexes =
  let tname = tblName tableSMSes
  in  Migration
        { mgrTableName = tname
        , mgrFrom      = 6
        , mgrAction    =
          StandardMigration $ do
            runQuery_ . sqlDropIndex tname $ (indexOnColumns ["reserved_by", "run_at"])
              { idxWhere = Just "reserved_by IS NULL AND run_at IS NOT NULL"
              }
            runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "run_at")
              { idxWhere = Just "run_at IS NOT NULL"
              }
            runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "attempts")
              { idxWhere = Just "attempts > 1 AND finished_at IS NULL"
              }
        }
