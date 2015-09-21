module File.Migrations where

import DB
import File.Tables
import KontraPrelude

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
