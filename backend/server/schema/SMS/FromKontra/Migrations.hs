module SMS.FromKontra.Migrations (createKontraInfoForSMSesTable) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import SMS.FromKontra.Tables

createKontraInfoForSMSesTable :: MonadDB m => Migration m
createKontraInfoForSMSesTable = Migration
  { mgrTableName = tblName tableKontraInfoForSMSes
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "kontra_info_for_smses"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "sms_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "sms_type", colType = SmallIntT, colNullable = False }
          , tblColumn { colName = "document_id", colType = BigIntT, colNullable = True }
          , tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = True
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "sms_id"
        , tblForeignKeys =
          [ (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
          , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                    }
          , (fkOnColumn "sms_id" "smses" "id") { fkOnDelete = ForeignKeyCascade }
          ]
        , tblIndexes = [indexOnColumn "document_id", indexOnColumn "signatory_link_id"]
        }
  }
