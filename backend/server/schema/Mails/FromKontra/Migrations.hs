module Mails.FromKontra.Migrations (createKontraInfoForMailsTable) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import Mails.FromKontra.Tables

createKontraInfoForMailsTable :: MonadDB m => Migration m
createKontraInfoForMailsTable = Migration
  { mgrTableName = tblName tableKontraInfoForMails
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "kontra_info_for_mails"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "mail_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "mail_type", colType = SmallIntT, colNullable = False }
          , tblColumn { colName = "document_id", colType = BigIntT, colNullable = True }
          , tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = True
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "mail_id"
        , tblForeignKeys =
          [ (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
          , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                    }
          , (fkOnColumn "mail_id" "mails" "id") { fkOnDelete = ForeignKeyCascade }
          ]
        , tblIndexes = [indexOnColumn "document_id", indexOnColumn "signatory_link_id"]
        }
  }
