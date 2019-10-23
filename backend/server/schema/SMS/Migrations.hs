module SMS.Migrations (removeDataFromSmses) where

import DB
import SMS.Tables

removeDataFromSmses :: MonadDB m => Migration m
removeDataFromSmses = Migration
  { mgrTableName = tblName tableSMSes
  , mgrFrom      = 5
  , mgrAction    = StandardMigration $ do
                     runSQL_ "ALTER TABLE smses DROP COLUMN data"
  }
