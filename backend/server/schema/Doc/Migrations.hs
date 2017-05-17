module Doc.Migrations (addIsReceiptToDocument,addAllowsHighlightingToSignatories,createHighlightedPagesTable,normalizeCheckboxesSize,normalizeCheckboxesFSRel) where

import Data.Int
import Database.PostgreSQL.PQTypes.Checks

import DB
import Doc.Tables
import KontraPrelude

addIsReceiptToDocument :: MonadDB m => Migration m
addIsReceiptToDocument = Migration {
    mgrTableName = tblName tableDocuments
  , mgrFrom = 43
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "documents" [
          sqlAddColumn tblColumn { colName = "is_receipt", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
  }

addAllowsHighlightingToSignatories  :: MonadDB m => Migration m
addAllowsHighlightingToSignatories  = Migration {
    mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 30
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links" [
          sqlAddColumn tblColumn { colName = "allows_highlighting", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
  }

createHighlightedPagesTable :: MonadDB m => Migration m
createHighlightedPagesTable = Migration {
    mgrTableName = tblName tableHighlightedPages
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "highlighted_pages"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id",          colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "signatory_link_id", colType = BigIntT,    colNullable = False }
      , tblColumn { colName = "page",        colType = IntegerT,  colNullable = False }
      , tblColumn { colName = "file_id",     colType = BigIntT,    colNullable = False }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblForeignKeys = [
        (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
      , fkOnColumn "file_id" "files" "id"
      ]
    , tblIndexes = [
        uniqueIndexOnColumns ["signatory_link_id", "page"]
      , indexOnColumn "signatory_link_id"
      , indexOnColumn "file_id"
      ]
    }
  }


normalizeCheckboxesSize :: MonadDB m => Migration m
normalizeCheckboxesSize = Migration {
    mgrTableName = tblName tableFieldPlacements
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runQuery_ . sqlUpdate "field_placements" $ do
        sqlSet "wrel" (0.011538 :: Double) -- New default checkbox size
        sqlSet "hrel" (0 :: Double)
        sqlWhereInSql "signatory_field_id" $ do
          sqlSelect "signatory_link_fields" $ do
            sqlWhereEq "type" (9 :: Int16) -- CheckboxT
            sqlResult "id"
  }

normalizeCheckboxesFSRel :: MonadDB m => Migration m
normalizeCheckboxesFSRel = Migration {
    mgrTableName = tblName tableFieldPlacements
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      runQuery_ . sqlUpdate "field_placements" $ do
        sqlSet "fsrel" (0 :: Double)
        sqlWhereInSql "signatory_field_id" $ do
          sqlSelect "signatory_link_fields" $ do
            sqlWhereEq "type" (9 :: Int16) -- CheckboxT
            sqlResult "id"
  }



