module Partner.Tables (
  tablePartners
, tablePartnerAdmins
) where

import Data.Int

import DB
import KontraPrelude

tablePartners :: Table
tablePartners = tblTable {
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
  , tblInitialSetup = Just $ TableInitialSetup
      { checkInitialSetup = do
          runQuery_ . sqlSelect "partners" $ do
            sqlWhere "default_partner"
            sqlResult "id"
          (defaultPartners::[Int64]) <- fetchMany runIdentity
          return $ length defaultPartners == 1
      , initialSetup = do
          runQuery_ . sqlInsert "partners" $ do
            sqlSet "name" ("Default" :: String)
            sqlSet "default_partner" True
            sqlResult "id"
      }
  }

tablePartnerAdmins :: Table
tablePartnerAdmins = tblTable {
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
