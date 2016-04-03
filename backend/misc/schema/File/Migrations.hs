module File.Migrations where

import DB
import File.Tables
import KontraPrelude

filesAddIndexesToSpeedUpQueries :: MonadDB m => Migration m
filesAddIndexesToSpeedUpQueries = Migration {
    mgrTable = tableFiles
  , mgrFrom = 7
  , mgrDo = do
    let tname = tblName tableFiles
    runQuery_ . sqlDropIndex tname $ (indexOnColumn "purge_at") {
      idxWhere = Just "purged_time IS NOT NULL"
    }
    runQuery_ . sqlCreateIndex tname $ (indexOnColumn "purge_at") {
      idxWhere = Just "purge_at IS NOT NULL AND purged_time IS NULL"
    }
    runQuery_ . sqlCreateIndex tname $ (indexOnColumn "id") {
      idxWhere = Just "content IS NOT NULL"
    }
  }

filesAddPurgeAtColumn :: MonadDB m => Migration m
filesAddPurgeAtColumn = Migration {
    mgrTable = tableFiles
  , mgrFrom = 6
  , mgrDo = do
    let tname = tblName tableFiles
    runQuery_ $ sqlAlterTable tname [
        sqlAddColumn tblColumn { colName = "purge_at", colType = TimestampWithZoneT }
      ]
    runQuery_ . sqlCreateIndex tname $ (indexOnColumn "purge_at") {
      idxWhere = Just "purged_time IS NOT NULL"
    }
  }
