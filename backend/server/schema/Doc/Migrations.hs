module Doc.Migrations
  ( addIndexForEfficientJoinToSignatoryLinkMagicHashes
  , addIsReceiptToDocument
  , addAllowsHighlightingToSignatories
  , addPKToDocumentTags
  , createHighlightedPagesTable
  , normalizeCheckboxesSize
  , normalizeCheckboxesFSRel
  , addRequiredFlagToSignatoryAttachment
  , signatoryLinkFieldsAddRadioGroupValues
  , addEditableBySignatoryFlag
  , signatoryLinkFieldsAddCustomValidation
  , addSearchColumnsToDocument
  , addAuthorUserIDToDocuments
  , addHidePnElogToSignatories
  , createSignatoryLinkConsentQuestionsTable
  , addConsentTitleToSignatoryLink
  , removeSearchTermsIndex
  , addShareableLinkHashToDocuments
  , addAuthenticationToViewArchivedMethodToSignatories
  , changeIsPartnerColumnToSignatoryRole
  , createSignatoryLinkMagicHashes
  , addTemplateInfoToDocuments
  , addCanBeForwardedToSignatories
  , createApiCallbackResults
) where

import Data.Int
import Database.PostgreSQL.PQTypes.Checks

import DB
import Doc.Tables

addIndexForEfficientJoinToSignatoryLinkMagicHashes :: MonadDB m => Migration m
addIndexForEfficientJoinToSignatoryLinkMagicHashes = Migration {
    mgrTableName = tblName tableSignatoryLinkMagicHashes
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runSQL_ $ smconcat
        [ "CREATE INDEX"
        , "idx__signatory_link_magic_hashes__signatory_link_id"
        , "ON signatory_link_magic_hashes(signatory_link_id)"
        ]
  }

addAuthenticationToViewArchivedMethodToSignatories :: MonadDB m => Migration m
addAuthenticationToViewArchivedMethodToSignatories = Migration {
    mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 33
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links"
        [ sqlAddColumn tblColumn { colName = "authentication_to_view_archived_method", colType = SmallIntT, colNullable = False, colDefault = Just "1" }
        ]
      runQuery_ $ sqlAlterTable "signatory_links"
        [ sqlAlterColumn "authentication_to_view_archived_method" "DROP DEFAULT"
        ]
  }

removeSearchTermsIndex :: MonadDB m => Migration m
removeSearchTermsIndex = Migration {
    mgrTableName = tblName tableDocuments
  , mgrFrom = 46
  , mgrAction = StandardMigration $ do
      let tname = tblName tableDocuments
      runQuery_ . sqlDropIndex tname $
          (indexOnColumn "archive_search_terms")
              { idxWhere = Just ("archive_search_terms IS NULL") }
  }

addAuthorUserIDToDocuments :: MonadDB m => Migration m
addAuthorUserIDToDocuments = Migration {
    mgrTableName = tblName tableDocuments
  , mgrFrom = 45
  , mgrAction = StandardMigration $ do
      let tname = tblName tableDocuments
      runQuery_ $ sqlAlterTable tname
        [
          sqlAddColumn $ tblColumn { colName = "author_user_id"
                                   , colType = BigIntT, colNullable = True }
        , sqlAddFK tname $ (fkOnColumn "author_user_id" "users" "id")
        ]
      runQuery_ . sqlCreateIndex tname $ indexOnColumn "author_user_id"
  }

addSearchColumnsToDocument :: MonadDB m => Migration m
addSearchColumnsToDocument = Migration {
    mgrTableName = tblName tableDocuments
  , mgrFrom = 44
  , mgrAction = StandardMigration $ do
      let tname = tblName tableDocuments
      runQuery_ $ sqlAlterTable "documents"
        [
          sqlAddColumn tblColumn { colName = "archive_search_terms", colType = TextT, colNullable = True }
        , sqlAddColumn tblColumn { colName = "archive_search_fts", colType = TSVectorT, colNullable = True }
        ]
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "archive_search_terms") { idxWhere = Just ("archive_search_terms IS NULL") }
      runQuery_ . sqlCreateIndex tname $ (indexOnColumnWithMethod "archive_search_fts" GIN)
  }

addPKToDocumentTags :: MonadDB m => Migration m
addPKToDocumentTags = Migration {
    mgrTableName = tblName tableDocumentTags
  , mgrFrom = 2
  , mgrAction = StandardMigration $ do
      -- the index created by introducing the pk supercedes the existing one
      runQuery_ $ sqlDropIndex (tblName tableDocumentTags)
                               (indexOnColumn "document_id")
      runQuery_ $ sqlAlterTable (tblName tableDocumentTags) [
          sqlAddPK (tblName tableDocumentTags)
                   (fromJust . pkOnColumns $ ["document_id", "name"])

        ]
}

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

addEditableBySignatoryFlag :: MonadDB m  => Migration m
addEditableBySignatoryFlag = Migration {
      mgrTableName = tblName tableSignatoryLinkFields
    , mgrFrom = 13
    , mgrAction = StandardMigration $ do
        runQuery_ $ sqlAlterTable "signatory_link_fields" [
          sqlAddColumn tblColumn { colName = "editable_by_signatory", colType = BoolT}
          ]

        runQuery_ . sqlUpdate "signatory_link_fields" $ do
          sqlSet "editable_by_signatory" False
          sqlWhere "type = 6 OR type = 10"

        runQuery_ $ sqlAlterTable "signatory_link_fields"  $ [ sqlAddCheck $
          Check "check_signatory_link_fields_editable_by_signatory__well_defined" $
            "(type = ANY (ARRAY[6, 10])) AND editable_by_signatory IS NOT NULL"
            <+> "OR (type <> ALL (ARRAY[6, 10])) AND editable_by_signatory IS NULL"
          ]

    }

signatoryLinkFieldsAddCustomValidation :: MonadDB m => Migration m
signatoryLinkFieldsAddCustomValidation = Migration {
    mgrTableName = tblName tableSignatoryLinkFields
  , mgrFrom = 14
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_link_fields" [
          sqlAddColumn tblColumn { colName = "custom_validation_pattern", colType = TextT, colNullable = True }
        , sqlAddColumn tblColumn { colName = "custom_validation_positive_example", colType = TextT, colNullable = True }
        , sqlAddColumn tblColumn { colName = "custom_validation_tooltip", colType = TextT, colNullable = True }
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields"  $ map sqlAddCheck [
          Check "check_signatory_link_fields_custom_validations_are_well_defined" $
                    "custom_validation_pattern IS NULL \
                \AND custom_validation_positive_example IS NULL \
                \AND custom_validation_tooltip IS NULL \
             \OR type = 7 \
                \AND custom_validation_pattern IS NOT NULL \
                \AND custom_validation_positive_example IS NOT NULL \
                \AND custom_validation_tooltip IS NOT NULL"
        ]
  }

addHidePnElogToSignatories  :: MonadDB m => Migration m
addHidePnElogToSignatories  = Migration {
    mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 31
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links" [
          sqlAddColumn tblColumn { colName = "hide_pn_elog", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
  }

createSignatoryLinkConsentQuestionsTable :: MonadDB m => Migration m
createSignatoryLinkConsentQuestionsTable = Migration {
    mgrTableName = tblName tableSignatoryLinkConsentQuestions
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable {
      tblName = "signatory_link_consent_questions"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id",                colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "signatory_link_id", colType = BigIntT,    colNullable = False }
      , tblColumn { colName = "position",          colType = SmallIntT,  colNullable = False }
      , tblColumn { colName = "title",             colType = TextT,      colNullable = False }
      , tblColumn { colName = "positive_option",   colType = TextT,      colNullable = False }
      , tblColumn { colName = "negative_option",   colType = TextT,      colNullable = False }
      , tblColumn { colName = "response",          colType = BoolT,      colNullable = True }
      , tblColumn { colName = "description_title", colType = TextT,      colNullable = True }
      , tblColumn { colName = "description_text",  colType = TextT,      colNullable = True }
     ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblChecks = [
        Check "description_all_or_nothing"
          "description_title IS NOT NULL AND description_text IS NOT NULL\
          \ OR description_title IS NULL AND description_text IS NULL"
      ]
    , tblForeignKeys = [
        (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
      ]
    , tblIndexes = [
        indexOnColumn "signatory_link_id"
      , tblIndex { idxColumns = ["signatory_link_id", "\"position\""], idxUnique = True }
      ]
    }
  }

addConsentTitleToSignatoryLink :: MonadDB m => Migration m
addConsentTitleToSignatoryLink = Migration {
    mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 32
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableSignatoryLinks) [
          sqlAddColumn tblColumn { colName = "consent_title", colType = TextT, colNullable = True }
        ]
  }

addShareableLinkHashToDocuments :: MonadDB m => Migration m
addShareableLinkHashToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom = 47
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable (tblName tableDocuments)
        [ sqlAddColumn tblColumn { colName = "shareable_link_hash", colType = BigIntT }
        ]
  }

changeIsPartnerColumnToSignatoryRole :: MonadDB m => Migration m
changeIsPartnerColumnToSignatoryRole = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 34
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links"
        [ sqlAlterColumn "is_partner"
          "SET DATA TYPE smallint USING CASE is_partner WHEN false THEN 1 ELSE 2 END"
        ]
      runQuery_ $ sqlAlterTable "signatory_links"
        [ "RENAME COLUMN is_partner TO signatory_role" ]
  }

addCanBeForwardedToSignatories  :: MonadDB m => Migration m
addCanBeForwardedToSignatories  = Migration {
    mgrTableName = tblName tableSignatoryLinks
  , mgrFrom = 35
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links" [
          sqlAddColumn tblColumn { colName = "can_be_forwarded", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
  }

createSignatoryLinkMagicHashes :: MonadDB m => Migration m
createSignatoryLinkMagicHashes = Migration
  { mgrTableName = tblName tableSignatoryLinkMagicHashes
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable
      { tblName = "signatory_link_magic_hashes"
      , tblVersion = 1
      , tblColumns =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
          , tblColumn { colName = "expiration_time", colType = TimestampWithZoneT, colNullable = False }
          ]
      , tblPrimaryKey = pkOnColumn "id"
      , tblForeignKeys =
          [ (fkOnColumn "signatory_link_id" "signatory_links" "id")
              { fkOnDelete = ForeignKeyCascade }
          ]
      , tblIndexes = [tblIndex { idxColumns = ["hash", "signatory_link_id"], idxUnique = True }]
      }
  }

addTemplateInfoToDocuments :: MonadDB m => Migration m
addTemplateInfoToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom = 48
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable "documents"
        [ sqlAddColumn tblColumn
            { colName = "template_id", colType = BigIntT }
        , sqlAddColumn tblColumn
            { colName = "from_shareable_link"
            , colType = BoolT
            , colNullable = False
            , colDefault = Just "false"
            }
        , sqlAddCheck $ Check
            "check_from_shareable_link_has_template_id"
            "from_shareable_link = false OR template_id IS NOT NULL"
        ]
      runQuery_ . sqlCreateIndex "documents" $ indexOnColumn "template_id"
  }

createApiCallbackResults :: MonadDB m => Migration m
createApiCallbackResults = Migration
  { mgrTableName = tblName tableApiCallbackResult
  , mgrFrom = 0
  , mgrAction = StandardMigration $ createTable True tblTable
      { tblName = "api_callback_result"
      , tblVersion = 1
      , tblColumns = [
          tblColumn { colName = "document_id",     colType = BigIntT, colNullable = False }
        , tblColumn { colName = "callback_result", colType = TextT }
        ]
      , tblPrimaryKey = pkOnColumn "document_id"
      , tblForeignKeys = [
          (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
        ]
      , tblIndexes = []
      }
  }
