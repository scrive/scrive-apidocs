module SMS.FromKontra.Migrations
  ( createKontraInfoForSMSesTable
  , updateKontraInfoForSMSesAggregate
  )
where

import Database.PostgreSQL.PQTypes.Checks

import DB

createKontraInfoForSMSesTable :: MonadDB m => Migration m
createKontraInfoForSMSesTable = Migration
  { mgrTableName = tableName
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = tableName
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
  where tableName = "kontra_info_for_smses"

updateKontraInfoForSMSesAggregate :: MonadDB m => Migration m
updateKontraInfoForSMSesAggregate = Migration
  { mgrTableName = tableName
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ mapM_
      runQuery_
      [ "DELETE FROM " <> tableName <> " WHERE document_id IS NULL"
        -- There should be no such records since all KontraInfoForSMS
        -- constructors require DocumentID. But database column allows it
        -- and one can never know when something was messed up.
      , sqlAlterTable
        tableName
        [ sqlDropPK tableName
        , sqlAlterColumn "document_id" "SET NOT NULL"
        , sqlAddPK tableName . fromJust $ pkOnColumns ["sms_id", "document_id"]
        ]
      , sqlCreateIndexSequentially tableName $ indexOnColumn "sms_id"
      , sqlCreateComposite CompositeType
        { ctName    = "kontra_for_sms_aggregate_c1"
        , ctColumns = [ CompositeColumn { ccName = "sms_type", ccType = SmallIntT }
                      , CompositeColumn { ccName = "document_id", ccType = BigIntT }
                      , CompositeColumn { ccName = "signatory_link_id", ccType = BigIntT }
                      ]
        }
      ]
  }
  where tableName = "kontra_info_for_smses"
