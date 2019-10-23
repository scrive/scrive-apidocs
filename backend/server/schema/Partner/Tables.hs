module Partner.Tables (
  tablePartners
) where

import Data.Int

import DB

tablePartners :: Table
tablePartners = tblTable
  { tblName         = "partners"
  , tblVersion      = 2
  , tblColumns      =
    [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName     = "default_partner"
                , colType     = BoolT
                , colNullable = False
                , colDefault  = Just "false"
                }
    , tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
    ]
  , tblPrimaryKey   = pkOnColumn "id"
  , tblIndexes = [ uniqueIndexOnColumnWithCondition "default_partner" "default_partner"
                 , indexOnColumn "user_group_id"
                 ]
  , tblForeignKeys  =
    [(fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }]
  , tblInitialSetup = Just $ TableInitialSetup
                        { checkInitialSetup = do
                                                runQuery_ . sqlSelect "partners" $ do
                                                  sqlWhere "default_partner"
                                                  sqlResult "id"
                                                (defaultPartners :: [Int64]) <- fetchMany
                                                  runIdentity
                                                return $ length defaultPartners == 1
                        , initialSetup      = do
                                                runQuery_ . sqlInsert "partners" $ do
                                                  sqlSet "name" ("Default" :: String)
                                                  sqlSet "default_partner" True
                                                  sqlResult "id"
                        }
  }
