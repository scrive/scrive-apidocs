module User.History.Migrations
    (
     addPKToUsersHistory
    ) where

import DB
import KontraPrelude
import User.History.Tables

addPKToUsersHistory :: MonadDB m => Migration m
addPKToUsersHistory =  Migration {
    mgrTableName = tblName tableUsersHistory
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableUsersHistory) [
          sqlAddColumn $ tblColumn
                           { colName = "id"
                           , colType = BigSerialT
                           , colNullable = False }
        , sqlAddPK (tblName tableUsersHistory) (fromJust . pkOnColumn $ "id")
        ]
  }
