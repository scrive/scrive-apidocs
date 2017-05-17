module Partner.Migrations (
  createTablePartners
, createTablePartnerAdmins
) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import KontraPrelude
import Partner.Tables

createTablePartners :: MonadDB m => Migration m
createTablePartners = Migration {
    mgrTableName = tblName tablePartners
  , mgrFrom = 0
  , mgrAction = StandardMigration $ do
      createTable True tblTable {
          tblName = "partners"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
            , tblColumn { colName = "name", colType = TextT, colNullable = False }
            , tblColumn { colName = "default_partner", colType = BoolT, colNullable = False, colDefault = Just "false" }
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblIndexes = [uniqueIndexOnColumnWithCondition "default_partner" "default_partner"]
        , tblForeignKeys = []
        , tblInitialSetup = Nothing
        }
      runQuery_ . sqlInsert "partners" $ do
        sqlSet "name" ("Default" :: String)
        sqlSet "default_partner" True
  }

createTablePartnerAdmins :: MonadDB m => Migration m
createTablePartnerAdmins = Migration {
    mgrTableName = tblName tablePartnerAdmins
  , mgrFrom = 0
  , mgrAction = StandardMigration $ do
      createTable True tblTable {
           tblName = "partner_admins"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
            , tblColumn { colName = "partner_id", colType = BigIntT, colNullable = False}
            ]
        , tblPrimaryKey = pkOnColumns ["user_id", "partner_id"]
        , tblIndexes = []
        , tblForeignKeys =
            [ (fkOnColumn "user_id" "users" "id")  { fkOnDelete = ForeignKeyCascade }
            , (fkOnColumn "partner_id" "partners" "id") { fkOnDelete = ForeignKeyCascade }
            ]
        }
  }
