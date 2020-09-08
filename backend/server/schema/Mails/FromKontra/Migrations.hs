module Mails.FromKontra.Migrations
  ( createKontraInfoForMailsTable
  , updateKontraInfoForMailsAggregate
  ) where

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
        { tblName        = tblName tableKontraInfoForMails
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

updateKontraInfoForMailsAggregate :: MonadDB m => Migration m
updateKontraInfoForMailsAggregate = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ mapM_
      runQuery_
      [ "DELETE FROM " <> tableName <> " WHERE document_id IS NULL"
        -- There should be no such records since all KontraInfoForMail
        -- constructors require DocumentID. But database column allows it
        -- and one can never know when something was messed up.
      , sqlAlterTable
        tableName
        [ sqlDropPK tableName
        , sqlAlterColumn "document_id" "SET NOT NULL"
        , sqlAddPK tableName . fromJust $ pkOnColumns ["mail_id", "document_id"]
        ]
      , sqlCreateIndexSequentially tableName $ indexOnColumn "mail_id"
      , sqlCreateComposite CompositeType
        { ctName    = "kontra_for_mail_aggregate_c1"
        , ctColumns = [ CompositeColumn { ccName = "mail_type", ccType = SmallIntT }
                      , CompositeColumn { ccName = "document_id", ccType = BigIntT }
                      , CompositeColumn { ccName = "signatory_link_id", ccType = BigIntT }
                      ]
        }
      ]
  }
  where tableName = "kontra_info_for_mails"
