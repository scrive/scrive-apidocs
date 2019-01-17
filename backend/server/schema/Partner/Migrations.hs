module Partner.Migrations (
  createTablePartners
, createTablePartnerAdmins
, partnersAddUserGroupID
, dropPartnerAdmins
) where

import Database.PostgreSQL.PQTypes.Checks

import DB
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
    mgrTableName = "partner_admins"
  , mgrFrom = 0
  , mgrAction = StandardMigration $ do
      createTable True tblTable {
           tblName = "partner_admins"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn { colName = "user_id", colType = BigIntT, colNullable = False }
            , tblColumn { colName = "partner_id", colType = BigIntT, colNullable = False}
            ]
        , tblPrimaryKey = pkOnColumns ["partner_id", "user_id"]
        , tblIndexes = []
        , tblForeignKeys =
            [ (fkOnColumn "user_id" "users" "id")  { fkOnDelete = ForeignKeyCascade }
            , (fkOnColumn "partner_id" "partners" "id") { fkOnDelete = ForeignKeyCascade }
            ]
        }
  }

partnersAddUserGroupID :: MonadDB m => Migration m
partnersAddUserGroupID = Migration {
    mgrTableName = tblName tablePartners
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
    let tname = tblName tablePartners
    runQuery_ $ sqlAlterTable tname
      [
        sqlAddColumn $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
      , sqlAddFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
      ]
    runQuery_ $ sqlCreateIndex tname $ indexOnColumn "user_group_id"
}

dropPartnerAdmins :: MonadDB m => Migration m
dropPartnerAdmins = Migration
  { mgrTableName = "partner_admins"
  , mgrFrom = 1
  , mgrAction = DropTableMigration DropTableRestrict
  }
