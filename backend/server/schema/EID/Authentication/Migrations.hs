module EID.Authentication.Migrations (
    addSignatoryIPToEIDAuthentications
) where

import DB
import EID.Authentication.Tables

addSignatoryIPToEIDAuthentications :: MonadDB m => Migration m
addSignatoryIPToEIDAuthentications = Migration {
    mgrTableName = tblName tableEIDAuthentications
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableEIDAuthentications)  [ sqlAddColumn $
          tblColumn { colName = "signatory_ip", colType = TextT, colNullable = True }
        ]
  }
