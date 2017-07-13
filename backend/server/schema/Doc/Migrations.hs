module Doc.Migrations (
    addIsReceiptToDocument
  , addAllowsHighlightingToSignatories
  , createHighlightedPagesTable
  , normalizeCheckboxesSize
  , normalizeCheckboxesFSRel
  , addRequiredFlagToSignatoryAttachment
  , signatoryLinkFieldsAddRadioGroupValues
) where

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

addRequiredFlagToSignatoryAttachment :: MonadDB m  => Migration m
addRequiredFlagToSignatoryAttachment = Migration {
      mgrTableName = tblName tableSignatoryAttachments
    , mgrFrom = 8
    , mgrAction = StandardMigration $ do
        runQuery_ $ sqlAlterTable "signatory_attachments" [
          sqlAddColumn tblColumn { colName = "required", colType = BoolT, colNullable = False, colDefault = Just "true"}
          ]
    }

signatoryLinkFieldsAddRadioGroupValues :: MonadDB m => Migration m
signatoryLinkFieldsAddRadioGroupValues = Migration {
    mgrTableName = tblName tableSignatoryLinkFields
  , mgrFrom = 12
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_link_fields" [
          sqlAddColumn tblColumn { colName = "radio_button_group_values", colType = ArrayT TextT }
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields"  $ map sqlDropCheck  [
          Check "check_signatory_link_fields_name_fields_are_well_defined" ""
        , Check "check_signatory_link_fields_signatures_are_well_defined" ""
        , Check "check_signatory_link_fields_checkboxes_are_well_defined" ""
        , Check "check_signatory_link_fields_other_text_fields_are_well_defined" ""
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields"  $ map sqlAddCheck [
          Check "check_signatory_link_fields_name_fields_are_well_defined" $
            "type = 1 AND name_order IS NOT NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL AND radio_button_group_values IS NULL"
            <+> "OR type <> 1"
        , Check "check_signatory_link_fields_signatures_are_well_defined" $
            "type = 8 AND name_order IS NULL AND value_bool IS NULL AND value_text IS NULL AND radio_button_group_values IS NULL"
            <+> "OR type <> 8"
        , Check "check_signatory_link_fields_checkboxes_are_well_defined" $
            "type = 9 AND name_order IS NULL AND value_bool IS NOT NULL AND value_file_id IS NULL AND value_text IS NULL AND radio_button_group_values IS NULL"
            <+> "OR type <> 9"
        , Check "check_signatory_link_fields_other_text_fields_are_well_defined" $
            "(type = ANY (ARRAY[3, 4, 5, 6, 7, 10])) AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL AND radio_button_group_values IS NULL"
            <+> "OR NOT (type = ANY (ARRAY[3, 4, 5, 6, 7, 10]))"
        , Check "check_signatory_link_fields_radio_buttons_are_well_defined" $
            "type = 11 AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND radio_button_group_values IS NOT NULL"
            <+> "OR type <> 11"
        ]
  }
