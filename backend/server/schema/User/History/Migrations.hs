module User.History.Migrations (
  addPKToUsersHistory
, optimiseUsersHistoryIndexes
) where

import DB
import User.History.Tables

addPKToUsersHistory :: MonadDB m => Migration m
addPKToUsersHistory = Migration
  { mgrTableName = tblName tableUsersHistory
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableUsersHistory)
        [ sqlAddColumn
          $ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
        , sqlAddPK (tblName tableUsersHistory) (fromJust . pkOnColumn $ "id")
        ]
  }

optimiseUsersHistoryIndexes :: MonadDB m => Migration m
optimiseUsersHistoryIndexes =
  let tname = tblName tableUsersHistory
  in
    Migration
      { mgrTableName = tname
      , mgrFrom      = 2
      , mgrAction    =
        StandardMigration $ mapM_
          runQuery_
          [ sqlDropIndex tname $ indexOnColumn "user_id"
          , sqlDropIndex tname $ indexOnColumn "performing_user_id"
          , sqlCreateIndexSequentially tname
            $ indexOnColumns ["user_id", "event_type", "\"time\""]
          ]
      }
