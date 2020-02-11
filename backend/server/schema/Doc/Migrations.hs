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
  , addShowArrow
  , addNotificationDeliveryMethodToSignatories
  , addMailConfirmationDeliveryStatusToSignatoryLinks
  , createApiCallbackResults
  , addFolderIDColumnToDocuments
  , addIndexOnShareableLinkHash
  , renameDocumentComposites
  , dropTokenFromDocumentSessionTokens
  , addSignatoryAccessTokensTable
  , updateCompositeTypesForSignatoryAccessTokens
  , dropSignatoryLinkMagicHashesTable
  , dropMagicHashFromSignatories
  , addAuthorDeletedFlags
  , addUGIDForEIDToDocuments
) where

import Data.Int
import Database.PostgreSQL.PQTypes.Checks

import DB
import Doc.Tables
import Doc.Tokens.Tables

addAuthorDeletedFlags :: MonadDB m => Migration m
addAuthorDeletedFlags = Migration
  { mgrTableName = "documents"
  , mgrFrom      = 53
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "documents"
        [ sqlAddColumn
          $ tblColumn { colName = "author_deleted", colType = TimestampWithZoneT }
        , sqlAddColumn
          $ tblColumn { colName = "author_really_deleted", colType = TimestampWithZoneT }
        , sqlAddColumn $ tblColumn { colName = "author_deleted_filled", colType = BoolT }
        ]
  }

renameDocumentComposites :: MonadDB m => Migration m
renameDocumentComposites = Migration
  { mgrTableName = "documents"
  , mgrFrom      = 52
  , mgrAction    =
    StandardMigration $ do
      runSQL_ "ALTER TYPE document RENAME TO document_c1"
      runSQL_ "ALTER TYPE main_file RENAME TO main_file_c1"
      runSQL_ "ALTER TYPE author_attachment RENAME TO author_attachment_c1"
      runSQL_ "ALTER TYPE signatory_attachment RENAME TO signatory_attachment_c1"
      runSQL_ "ALTER TYPE signatory_link RENAME TO signatory_link_c1"
      runSQL_
        "ALTER TYPE signatory_link_magic_hash RENAME TO signatory_link_magic_hash_c1"
      runSQL_ "ALTER TYPE document_tag RENAME TO document_tag_c1"
      runSQL_ "ALTER TYPE field_placement RENAME TO field_placement_c1"
      runSQL_ "ALTER TYPE placement_anchor RENAME TO placement_anchor_c1"
      runSQL_ "ALTER TYPE signatory_field RENAME TO signatory_field_c1"
      runSQL_ "ALTER TYPE highlighted_page RENAME TO highlighted_page_c1"
      runSQL_
        "ALTER TYPE signatory_consent_question RENAME TO signatory_consent_question_c1"
  }


addNotificationDeliveryMethodToSignatories :: MonadDB m => Migration m
addNotificationDeliveryMethodToSignatories =
  let tableName  = tblName tableSignatoryLinks
      columnName = "notification_delivery_method"
  in  Migration
        { mgrTableName = tableName
        , mgrFrom      = 37
        , mgrAction    = StandardMigration $ do
                           runQuery_ $ sqlAlterTable
                             tableName
                             [ sqlAddColumn tblColumn { colName     = columnName
                                                      , colType     = SmallIntT
                                                      , colNullable = False
                                                      , colDefault  = Just "0"
                                                      }
                             ]
                           runQuery_ $ sqlAlterTable
                             tableName
                             [sqlAlterColumn columnName "DROP DEFAULT"]
        }

addIndexForEfficientJoinToSignatoryLinkMagicHashes :: MonadDB m => Migration m
addIndexForEfficientJoinToSignatoryLinkMagicHashes = Migration
  { mgrTableName = "signatory_link_magic_hashes"
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runSQL_ $ smconcat
                       [ "CREATE INDEX"
                       , "idx__signatory_link_magic_hashes__signatory_link_id"
                       , "ON signatory_link_magic_hashes(signatory_link_id)"
                       ]
  }

addAuthenticationToViewArchivedMethodToSignatories :: MonadDB m => Migration m
addAuthenticationToViewArchivedMethodToSignatories = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 33
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAddColumn tblColumn
                           { colName     = "authentication_to_view_archived_method"
                           , colType     = SmallIntT
                           , colNullable = False
                           , colDefault  = Just "1"
                           }
                       ]
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAlterColumn "authentication_to_view_archived_method"
                                        "DROP DEFAULT"
                       ]
  }

removeSearchTermsIndex :: MonadDB m => Migration m
removeSearchTermsIndex = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 46
  , mgrAction    = StandardMigration $ do
                     let tname = tblName tableDocuments
                     runQuery_ . sqlDropIndex tname $ (indexOnColumn "archive_search_terms")
                       { idxWhere = Just ("archive_search_terms IS NULL")
                       }
  }

addAuthorUserIDToDocuments :: MonadDB m => Migration m
addAuthorUserIDToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 45
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableDocuments
      runQuery_ $ sqlAlterTable
        tname
        [ sqlAddColumn $ tblColumn { colName     = "author_user_id"
                                   , colType     = BigIntT
                                   , colNullable = True
                                   }
        , sqlAddValidFK tname $ (fkOnColumn "author_user_id" "users" "id")
        ]
      runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "author_user_id"
  }

addSearchColumnsToDocument :: MonadDB m => Migration m
addSearchColumnsToDocument = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 44
  , mgrAction    =
    StandardMigration $ do
      let tname = tblName tableDocuments
      runQuery_ $ sqlAlterTable
        "documents"
        [ sqlAddColumn tblColumn { colName     = "archive_search_terms"
                                 , colType     = TextT
                                 , colNullable = True
                                 }
        , sqlAddColumn tblColumn { colName     = "archive_search_fts"
                                 , colType     = TSVectorT
                                 , colNullable = True
                                 }
        ]
      runQuery_ . sqlCreateIndexSequentially tname $ (indexOnColumn "archive_search_terms"
                                                     )
        { idxWhere = Just ("archive_search_terms IS NULL")
        }
      runQuery_
        . sqlCreateIndexSequentially tname
        $ (indexOnColumnWithMethod "archive_search_fts" GIN)
  }

addPKToDocumentTags :: MonadDB m => Migration m
addPKToDocumentTags = Migration
  { mgrTableName = tblName tableDocumentTags
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
      -- the index created by introducing the pk supercedes the existing one
                     runQuery_ $ sqlDropIndex (tblName tableDocumentTags)
                                              (indexOnColumn "document_id")
                     runQuery_ $ sqlAlterTable
                       (tblName tableDocumentTags)
                       [ sqlAddPK (tblName tableDocumentTags)
                                  (fromJust . pkOnColumns $ ["document_id", "name"])
                       ]
  }

addIsReceiptToDocument :: MonadDB m => Migration m
addIsReceiptToDocument = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 43
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "documents"
                       [ sqlAddColumn tblColumn { colName     = "is_receipt"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                       ]
  }

addAllowsHighlightingToSignatories :: MonadDB m => Migration m
addAllowsHighlightingToSignatories = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 30
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAddColumn tblColumn { colName     = "allows_highlighting"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                       ]
  }

createHighlightedPagesTable :: MonadDB m => Migration m
createHighlightedPagesTable = Migration
  { mgrTableName = tblName tableHighlightedPages
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "highlighted_pages"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName = "page", colType = IntegerT, colNullable = False }
          , tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
          ]
        , tblPrimaryKey  = pkOnColumn "id"
        , tblForeignKeys =
          [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                    }
          , fkOnColumn "file_id" "files" "id"
          ]
        , tblIndexes     = [ uniqueIndexOnColumns ["signatory_link_id", "page"]
                           , indexOnColumn "signatory_link_id"
                           , indexOnColumn "file_id"
                           ]
        }
  }

normalizeCheckboxesSize :: MonadDB m => Migration m
normalizeCheckboxesSize = Migration
  { mgrTableName = tblName tableFieldPlacements
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlUpdate "field_placements" $ do
                       sqlSet "wrel" (0.011538 :: Double) -- New default checkbox size
                       sqlSet "hrel" (0 :: Double)
                       sqlWhereInSql "signatory_field_id" $ do
                         sqlSelect "signatory_link_fields" $ do
                           sqlWhereEq "type" (9 :: Int16) -- CheckboxT
                           sqlResult "id"
  }

normalizeCheckboxesFSRel :: MonadDB m => Migration m
normalizeCheckboxesFSRel = Migration
  { mgrTableName = tblName tableFieldPlacements
  , mgrFrom      = 2
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlUpdate "field_placements" $ do
                       sqlSet "fsrel" (0 :: Double)
                       sqlWhereInSql "signatory_field_id" $ do
                         sqlSelect "signatory_link_fields" $ do
                           sqlWhereEq "type" (9 :: Int16) -- CheckboxT
                           sqlResult "id"
  }

addRequiredFlagToSignatoryAttachment :: MonadDB m => Migration m
addRequiredFlagToSignatoryAttachment = Migration
  { mgrTableName = tblName tableSignatoryAttachments
  , mgrFrom      = 8
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_attachments"
                       [ sqlAddColumn tblColumn { colName     = "required"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "true"
                                                }
                       ]
  }

signatoryLinkFieldsAddRadioGroupValues :: MonadDB m => Migration m
signatoryLinkFieldsAddRadioGroupValues = Migration
  { mgrTableName = tblName tableSignatoryLinkFields
  , mgrFrom      = 12
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "signatory_link_fields"
        [ sqlAddColumn tblColumn { colName = "radio_button_group_values"
                                 , colType = ArrayT TextT
                                 }
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields" $ map
        sqlDropCheck
        [ "check_signatory_link_fields_name_fields_are_well_defined"
        , "check_signatory_link_fields_signatures_are_well_defined"
        , "check_signatory_link_fields_checkboxes_are_well_defined"
        , "check_signatory_link_fields_other_text_fields_are_well_defined"
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields" $ map
        sqlAddValidCheck
        [ tblCheck
          { chkName      = "check_signatory_link_fields_name_fields_are_well_defined"
          , chkCondition =
            "type = 1 AND name_order IS NOT NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL AND radio_button_group_values IS NULL OR type <> 1"
          }
        , tblCheck
          { chkName      = "check_signatory_link_fields_signatures_are_well_defined"
          , chkCondition =
            "type = 8 AND name_order IS NULL AND value_bool IS NULL AND value_text IS NULL AND radio_button_group_values IS NULL OR type <> 8"
          }
        , tblCheck
          { chkName      = "check_signatory_link_fields_checkboxes_are_well_defined"
          , chkCondition =
            "type = 9 AND name_order IS NULL AND value_bool IS NOT NULL AND value_file_id IS NULL AND value_text IS NULL AND radio_button_group_values IS NULL OR type <> 9"
          }
        , tblCheck
          { chkName = "check_signatory_link_fields_other_text_fields_are_well_defined"
          , chkCondition =
            "(type = ANY (ARRAY[3, 4, 5, 6, 7, 10])) AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL AND radio_button_group_values IS NULL OR NOT (type = ANY (ARRAY[3, 4, 5, 6, 7, 10]))"
          }
        , tblCheck
          { chkName      = "check_signatory_link_fields_radio_buttons_are_well_defined"
          , chkCondition =
            "type = 11 AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND radio_button_group_values IS NOT NULL OR type <> 11"
          }
        ]
  }

addEditableBySignatoryFlag :: MonadDB m => Migration m
addEditableBySignatoryFlag = Migration
  { mgrTableName = tblName tableSignatoryLinkFields
  , mgrFrom      = 13
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "signatory_link_fields"
        [sqlAddColumn tblColumn { colName = "editable_by_signatory", colType = BoolT }]

      runQuery_ . sqlUpdate "signatory_link_fields" $ do
        sqlSet "editable_by_signatory" False
        sqlWhere "type = 6 OR type = 10"

      runQuery_ $ sqlAlterTable
        "signatory_link_fields"
        [ sqlAddValidCheck $ tblCheck
            { chkName = "check_signatory_link_fields_editable_by_signatory__well_defined"
            , chkCondition =
              "(type = ANY (ARRAY[6, 10])) AND editable_by_signatory IS NOT NULL OR (type <> ALL (ARRAY[6, 10])) AND editable_by_signatory IS NULL"
            }
        ]
  }

signatoryLinkFieldsAddCustomValidation :: MonadDB m => Migration m
signatoryLinkFieldsAddCustomValidation = Migration
  { mgrTableName = tblName tableSignatoryLinkFields
  , mgrFrom      = 14
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "signatory_link_fields"
        [ sqlAddColumn tblColumn { colName     = "custom_validation_pattern"
                                 , colType     = TextT
                                 , colNullable = True
                                 }
        , sqlAddColumn tblColumn { colName     = "custom_validation_positive_example"
                                 , colType     = TextT
                                 , colNullable = True
                                 }
        , sqlAddColumn tblColumn { colName     = "custom_validation_tooltip"
                                 , colType     = TextT
                                 , colNullable = True
                                 }
        ]
      runQuery_ $ sqlAlterTable "signatory_link_fields" $ map
        sqlAddValidCheck
        [ tblCheck
            { chkName = "check_signatory_link_fields_custom_validations_are_well_defined"
            , chkCondition =
              "custom_validation_pattern \       \IS NULL \
                 \AND custom_validation_positive_example IS NULL \
                 \AND custom_validation_tooltip \       \IS NULL \
              \OR \  \type = 7 \
                 \AND custom_validation_pattern \       \IS NOT NULL \
                 \AND custom_validation_positive_example IS NOT NULL \
                 \AND custom_validation_tooltip \       \IS NOT NULL"
            }
        ]
  }

addHidePnElogToSignatories :: MonadDB m => Migration m
addHidePnElogToSignatories = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 31
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAddColumn tblColumn { colName     = "hide_pn_elog"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                       ]
  }

createSignatoryLinkConsentQuestionsTable :: MonadDB m => Migration m
createSignatoryLinkConsentQuestionsTable = Migration
  { mgrTableName = tblName tableSignatoryLinkConsentQuestions
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "signatory_link_consent_questions"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName = "position", colType = SmallIntT, colNullable = False }
          , tblColumn { colName = "title", colType = TextT, colNullable = False }
          , tblColumn { colName     = "positive_option"
                      , colType     = TextT
                      , colNullable = False
                      }
          , tblColumn { colName     = "negative_option"
                      , colType     = TextT
                      , colNullable = False
                      }
          , tblColumn { colName = "response", colType = BoolT, colNullable = True }
          , tblColumn { colName     = "description_title"
                      , colType     = TextT
                      , colNullable = True
                      }
          , tblColumn { colName     = "description_text"
                      , colType     = TextT
                      , colNullable = True
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "id"
        , tblChecks      =
          [ tblCheck
              { chkName      = "description_all_or_nothing"
              , chkCondition =
                "description_title IS NOT NULL AND description_text IS NOT NULL\
          \ OR description_title IS NULL AND description_text IS NULL"
              }
          ]
        , tblForeignKeys =
          [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                    }
          ]
        , tblIndexes     = [ indexOnColumn "signatory_link_id"
                           , tblIndex { idxColumns = ["signatory_link_id", "\"position\""]
                                      , idxUnique  = True
                                      }
                           ]
        }
  }

addConsentTitleToSignatoryLink :: MonadDB m => Migration m
addConsentTitleToSignatoryLink = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 32
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableSignatoryLinks)
                       [ sqlAddColumn tblColumn { colName     = "consent_title"
                                                , colType     = TextT
                                                , colNullable = True
                                                }
                       ]
  }

addShareableLinkHashToDocuments :: MonadDB m => Migration m
addShareableLinkHashToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 47
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       (tblName tableDocuments)
                       [ sqlAddColumn tblColumn { colName = "shareable_link_hash"
                                                , colType = BigIntT
                                                }
                       ]
  }

changeIsPartnerColumnToSignatoryRole :: MonadDB m => Migration m
changeIsPartnerColumnToSignatoryRole = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 34
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "signatory_links"
        [ sqlAlterColumn
            "is_partner"
            "SET DATA TYPE smallint USING CASE is_partner WHEN false THEN 1 ELSE 2 END"
        ]
      runQuery_
        $ sqlAlterTable "signatory_links" ["RENAME COLUMN is_partner TO signatory_role"]
  }

addCanBeForwardedToSignatories :: MonadDB m => Migration m
addCanBeForwardedToSignatories = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 35
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAddColumn tblColumn { colName     = "can_be_forwarded"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                       ]
  }

createSignatoryLinkMagicHashes :: MonadDB m => Migration m
createSignatoryLinkMagicHashes = Migration
  { mgrTableName = "signatory_link_magic_hashes"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "signatory_link_magic_hashes"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
          , tblColumn { colName     = "expiration_time"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          ]
        , tblPrimaryKey  = pkOnColumn "id"
        , tblForeignKeys =
          [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                    }
          ]
        , tblIndexes     = [ tblIndex { idxColumns = ["hash", "signatory_link_id"]
                                      , idxUnique  = True
                                      }
                           ]
        }
  }

addTemplateInfoToDocuments :: MonadDB m => Migration m
addTemplateInfoToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 48
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "documents"
                       [ sqlAddColumn tblColumn { colName = "template_id"
                                                , colType = BigIntT
                                                }
                       , sqlAddColumn tblColumn { colName     = "from_shareable_link"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "false"
                                                }
                       , sqlAddValidCheck $ tblCheck
                         { chkName      = "check_from_shareable_link_has_template_id"
                         , chkCondition =
                           "from_shareable_link = false OR template_id IS NOT NULL"
                         }
                       ]
                     runQuery_ . sqlCreateIndexSequentially "documents" $ indexOnColumn
                       "template_id"
  }

addShowArrow :: MonadDB m => Migration m
addShowArrow = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 49
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "documents"
                       [ sqlAddColumn tblColumn { colName     = "show_arrow"
                                                , colType     = BoolT
                                                , colNullable = False
                                                , colDefault  = Just "true"
                                                }
                       ]
  }

addMailConfirmationDeliveryStatusToSignatoryLinks :: MonadDB m => Migration m
addMailConfirmationDeliveryStatusToSignatoryLinks = Migration
  { mgrTableName = tblName tableSignatoryLinks
  , mgrFrom      = 36
  , mgrAction    = StandardMigration $ do
                     runQuery_ $ sqlAlterTable
                       "signatory_links"
                       [ sqlAddColumn tblColumn
                           { colName     = "mail_confirmation_delivery_status"
                           , colType     = SmallIntT
                           , colNullable = False
                           , colDefault  = Just "3"
                           }
                       ]
  }

createApiCallbackResults :: MonadDB m => Migration m
createApiCallbackResults = Migration
  { mgrTableName = tblName tableApiCallbackResult
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "api_callback_result"
        , tblVersion     = 1
        , tblColumns     = [ tblColumn { colName     = "document_id"
                                       , colType     = BigIntT
                                       , colNullable = False
                                       }
                           , tblColumn { colName = "callback_result", colType = TextT }
                           ]
        , tblPrimaryKey  = pkOnColumn "document_id"
        , tblForeignKeys =
          [(fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }]
        , tblIndexes     = []
        }
  }

addFolderIDColumnToDocuments :: MonadDB m => Migration m
addFolderIDColumnToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 50
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "documents"
        [ sqlAddColumn tblColumn { colName = "folder_id", colType = BigIntT }
        , sqlAddValidFK (tblName tableDocuments) $ (fkOnColumn "folder_id" "folders" "id")
        ]
      runQuery_ . sqlCreateIndexSequentially "documents" $ indexOnColumn "folder_id"
  }

addIndexOnShareableLinkHash :: MonadDB m => Migration m
addIndexOnShareableLinkHash = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 51
  , mgrAction    = StandardMigration $ do
                     runQuery_ . sqlCreateIndexSequentially "documents" $ indexOnColumn
                       "shareable_link_hash"
  }

dropTokenFromDocumentSessionTokens :: MonadDB m => Migration m
dropTokenFromDocumentSessionTokens = Migration
  { mgrTableName = tblName tableDocumentSessionTokens
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ do
                     runQuery_
                       $ sqlAlterTable "document_session_tokens" [sqlDropColumn "token"]
  }

addSignatoryAccessTokensTable :: MonadDB m => Migration m
addSignatoryAccessTokensTable = Migration
  { mgrTableName = "signatory_access_tokens"
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ do
      createTable
        True
        tblTable
          { tblName        = "signatory_access_tokens"
          , tblVersion     = 1
          , tblColumns     =
            [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
            , tblColumn { colName     = "signatory_link_id"
                        , colType     = BigIntT
                        , colNullable = False
                        }
            , tblColumn { colName = "hash", colType = BigIntT, colNullable = False }
            , tblColumn { colName = "expiration_time", colType = TimestampWithZoneT }
            , tblColumn { colName = "reason", colType = SmallIntT, colNullable = False }
            ]
          , tblPrimaryKey  = pkOnColumn "id"
          , tblForeignKeys =
            [ (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                                      }
            ]
          , tblIndexes     = [ indexOnColumn "signatory_link_id"
                             , uniqueIndexOnColumnWithCondition "signatory_link_id"
                                                                "reason = 7" -- Unique API access magic hash
                             ]
          }
      runQuery_
        . sqlInsertSelect "signatory_access_tokens" "signatory_link_magic_hashes smh"
        $ do
            sqlSetCmd "signatory_link_id" "smh.signatory_link_id"
            sqlSetCmd "hash"              "smh.hash"
            sqlSetCmd "expiration_time"   "smh.expiration_time"
            sqlSetCmd "reason"            "1" -- Legacy reason
  }

updateCompositeTypesForSignatoryAccessTokens :: MonadDB m => Migration m
updateCompositeTypesForSignatoryAccessTokens = Migration
  { mgrTableName = "signatory_access_tokens"
  , mgrFrom      = 1
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlDropComposite $ "document_c1"
      runQuery_ $ sqlDropComposite $ "signatory_link_c1"
      runQuery_ $ sqlDropComposite $ "signatory_link_magic_hash_c1"
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "signatory_access_tokens_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "hash", ccType = BigIntT }
          , CompositeColumn { ccName = "reason", ccType = SmallIntT }
          , CompositeColumn { ccName = "expiration_time", ccType = TimestampWithZoneT }
          ]
        }
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "signatory_link_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "id", ccType = BigIntT }
          , CompositeColumn { ccName = "signatory_fields"
                            , ccType = ArrayT $ CustomT "signatory_field_c1"
                            }
          , CompositeColumn { ccName = "is_author", ccType = BoolT }
          , CompositeColumn { ccName = "signatory_role", ccType = SmallIntT }
          , CompositeColumn { ccName = "sign_order", ccType = IntegerT }
          , CompositeColumn { ccName = "token", ccType = BigIntT }
          , CompositeColumn { ccName = "signatory_tokens"
                            , ccType = ArrayT $ CustomT "signatory_access_tokens_c1"
                            }
          , CompositeColumn { ccName = "user_id", ccType = BigIntT }
          , CompositeColumn { ccName = "sign_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "sign_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "seen_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "seen_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "read_invitation", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "mail_invitation_delivery_status"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "sms_invitation_delivery_status"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "deleted", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "really_deleted", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "csv_contents", ccType = TextT }
          , CompositeColumn { ccName = "attachments"
                            , ccType = ArrayT $ CustomT "signatory_attachment_c1"
                            }
          , CompositeColumn { ccName = "highlighted_pages"
                            , ccType = ArrayT $ CustomT "highlighted_page_c1"
                            }
          , CompositeColumn { ccName = "sign_redirect_url", ccType = TextT }
          , CompositeColumn { ccName = "reject_redirect_url", ccType = TextT }
          , CompositeColumn { ccName = "rejection_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "rejection_reason", ccType = TextT }
          , CompositeColumn { ccName = "authentication_to_view_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "authentication_to_view_archived_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "authentication_to_sign_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "delivery_method", ccType = SmallIntT }
          , CompositeColumn { ccName = "confirmation_delivery_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "notification_delivery_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "allows_highlighting", ccType = BoolT }
          , CompositeColumn { ccName = "has_identified_to_view", ccType = BoolT }
          , CompositeColumn { ccName = "hide_pn_elog", ccType = BoolT }
          , CompositeColumn { ccName = "can_be_forwarded", ccType = BoolT }
          , CompositeColumn { ccName = "consent_title", ccType = TextT }
          , CompositeColumn { ccName = "consent_questions"
                            , ccType = ArrayT $ CustomT "signatory_consent_question_c1"
                            }
          , CompositeColumn { ccName = "mail_confirmation_delivery_status"
                            , ccType = SmallIntT
                            }
          ]
        }
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "document_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "id", ccType = BigIntT }
          , CompositeColumn { ccName = "title", ccType = TextT }
          , CompositeColumn { ccName = "signatory_links"
                            , ccType = ArrayT $ CustomT "signatory_link_c1"
                            }
          , CompositeColumn { ccName = "main_files"
                            , ccType = ArrayT $ CustomT "main_file_c1"
                            }
          , CompositeColumn { ccName = "status", ccType = SmallIntT }
          , CompositeColumn { ccName = "type", ccType = SmallIntT }
          , CompositeColumn { ccName = "ctime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "mtime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "days_to_sign", ccType = IntegerT }
          , CompositeColumn { ccName = "days_to_remind", ccType = IntegerT }
          , CompositeColumn { ccName = "timeout_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "expiration_date", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "invite_text", ccType = TextT }
          , CompositeColumn { ccName = "confirm_text", ccType = TextT }
          , CompositeColumn { ccName = "show_header", ccType = BoolT }
          , CompositeColumn { ccName = "show_pdf_download", ccType = BoolT }
          , CompositeColumn { ccName = "show_reject_option", ccType = BoolT }
          , CompositeColumn { ccName = "allow_reject_reason", ccType = BoolT }
          , CompositeColumn { ccName = "show_footer", ccType = BoolT }
          , CompositeColumn { ccName = "is_receipt", ccType = BoolT }
          , CompositeColumn { ccName = "lang", ccType = SmallIntT }
          , CompositeColumn { ccName = "sharing", ccType = SmallIntT }
          , CompositeColumn { ccName = "tags"
                            , ccType = ArrayT $ CustomT "document_tag_c1"
                            }
          , CompositeColumn { ccName = "author_attachments"
                            , ccType = ArrayT $ CustomT "author_attachment_c1"
                            }
          , CompositeColumn { ccName = "api_v1_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "api_v2_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "unsaved_draft", ccType = BoolT }
          , CompositeColumn { ccName = "object_version", ccType = BigIntT }
          , CompositeColumn { ccName = "token", ccType = BigIntT }
          , CompositeColumn { ccName = "time_zone_name", ccType = TextT }
          , CompositeColumn { ccName = "author_user_group_id", ccType = BigIntT }
          , CompositeColumn { ccName = "status_class", ccType = SmallIntT }
          , CompositeColumn { ccName = "shareable_link_hash", ccType = BigIntT }
          , CompositeColumn { ccName = "template_id", ccType = BigIntT }
          , CompositeColumn { ccName = "from_shareable_link", ccType = BoolT }
          , CompositeColumn { ccName = "show_arrow", ccType = BoolT }
          , CompositeColumn { ccName = "folder_id", ccType = BigIntT }
          ]
        }
  }

dropSignatoryLinkMagicHashesTable :: MonadDB m => Migration m
dropSignatoryLinkMagicHashesTable = Migration
  { mgrTableName = "signatory_link_magic_hashes"
  , mgrFrom      = 2
  , mgrAction    = DropTableMigration DropTableRestrict
  }

dropMagicHashFromSignatories :: MonadDB m => Migration m
dropMagicHashFromSignatories = Migration
  { mgrTableName = "signatory_links"
  , mgrFrom      = 38
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable "signatory_links" [sqlDropColumn "token"]
      runQuery_ $ sqlDropComposite $ "document_c1"
      runQuery_ $ sqlDropComposite $ "signatory_link_c1"
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "signatory_link_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "id", ccType = BigIntT }
          , CompositeColumn { ccName = "signatory_fields"
                            , ccType = ArrayT $ CustomT "signatory_field_c1"
                            }
          , CompositeColumn { ccName = "is_author", ccType = BoolT }
          , CompositeColumn { ccName = "signatory_role", ccType = SmallIntT }
          , CompositeColumn { ccName = "sign_order", ccType = IntegerT }
          , CompositeColumn { ccName = "signatory_tokens"
                            , ccType = ArrayT $ CustomT "signatory_access_tokens_c1"
                            }
          , CompositeColumn { ccName = "user_id", ccType = BigIntT }
          , CompositeColumn { ccName = "sign_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "sign_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "seen_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "seen_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "read_invitation", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "mail_invitation_delivery_status"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "sms_invitation_delivery_status"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "deleted", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "really_deleted", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "csv_contents", ccType = TextT }
          , CompositeColumn { ccName = "attachments"
                            , ccType = ArrayT $ CustomT "signatory_attachment_c1"
                            }
          , CompositeColumn { ccName = "highlighted_pages"
                            , ccType = ArrayT $ CustomT "highlighted_page_c1"
                            }
          , CompositeColumn { ccName = "sign_redirect_url", ccType = TextT }
          , CompositeColumn { ccName = "reject_redirect_url", ccType = TextT }
          , CompositeColumn { ccName = "rejection_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "rejection_reason", ccType = TextT }
          , CompositeColumn { ccName = "authentication_to_view_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "authentication_to_view_archived_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "authentication_to_sign_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "delivery_method", ccType = SmallIntT }
          , CompositeColumn { ccName = "confirmation_delivery_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "notification_delivery_method"
                            , ccType = SmallIntT
                            }
          , CompositeColumn { ccName = "allows_highlighting", ccType = BoolT }
          , CompositeColumn { ccName = "has_identified_to_view", ccType = BoolT }
          , CompositeColumn { ccName = "hide_pn_elog", ccType = BoolT }
          , CompositeColumn { ccName = "can_be_forwarded", ccType = BoolT }
          , CompositeColumn { ccName = "consent_title", ccType = TextT }
          , CompositeColumn { ccName = "consent_questions"
                            , ccType = ArrayT $ CustomT "signatory_consent_question_c1"
                            }
          , CompositeColumn { ccName = "mail_confirmation_delivery_status"
                            , ccType = SmallIntT
                            }
          ]
        }
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "document_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "id", ccType = BigIntT }
          , CompositeColumn { ccName = "title", ccType = TextT }
          , CompositeColumn { ccName = "signatory_links"
                            , ccType = ArrayT $ CustomT "signatory_link_c1"
                            }
          , CompositeColumn { ccName = "main_files"
                            , ccType = ArrayT $ CustomT "main_file_c1"
                            }
          , CompositeColumn { ccName = "status", ccType = SmallIntT }
          , CompositeColumn { ccName = "type", ccType = SmallIntT }
          , CompositeColumn { ccName = "ctime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "mtime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "days_to_sign", ccType = IntegerT }
          , CompositeColumn { ccName = "days_to_remind", ccType = IntegerT }
          , CompositeColumn { ccName = "timeout_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "expiration_date", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "invite_text", ccType = TextT }
          , CompositeColumn { ccName = "confirm_text", ccType = TextT }
          , CompositeColumn { ccName = "show_header", ccType = BoolT }
          , CompositeColumn { ccName = "show_pdf_download", ccType = BoolT }
          , CompositeColumn { ccName = "show_reject_option", ccType = BoolT }
          , CompositeColumn { ccName = "allow_reject_reason", ccType = BoolT }
          , CompositeColumn { ccName = "show_footer", ccType = BoolT }
          , CompositeColumn { ccName = "is_receipt", ccType = BoolT }
          , CompositeColumn { ccName = "lang", ccType = SmallIntT }
          , CompositeColumn { ccName = "sharing", ccType = SmallIntT }
          , CompositeColumn { ccName = "tags"
                            , ccType = ArrayT $ CustomT "document_tag_c1"
                            }
          , CompositeColumn { ccName = "author_attachments"
                            , ccType = ArrayT $ CustomT "author_attachment_c1"
                            }
          , CompositeColumn { ccName = "api_v1_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "api_v2_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "unsaved_draft", ccType = BoolT }
          , CompositeColumn { ccName = "object_version", ccType = BigIntT }
          , CompositeColumn { ccName = "token", ccType = BigIntT }
          , CompositeColumn { ccName = "time_zone_name", ccType = TextT }
          , CompositeColumn { ccName = "author_user_group_id", ccType = BigIntT }
          , CompositeColumn { ccName = "status_class", ccType = SmallIntT }
          , CompositeColumn { ccName = "shareable_link_hash", ccType = BigIntT }
          , CompositeColumn { ccName = "template_id", ccType = BigIntT }
          , CompositeColumn { ccName = "from_shareable_link", ccType = BoolT }
          , CompositeColumn { ccName = "show_arrow", ccType = BoolT }
          , CompositeColumn { ccName = "folder_id", ccType = BigIntT }
          ]
        }
  }

addUGIDForEIDToDocuments :: MonadDB m => Migration m
addUGIDForEIDToDocuments = Migration
  { mgrTableName = tblName tableDocuments
  , mgrFrom      = 54
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        "documents"
        [ sqlAddColumn tblColumn { colName = "user_group_to_impersonate_for_eid"
                                 , colType = BigIntT
                                 }
        , sqlAddValidFK (tblName tableDocuments)
          $ (fkOnColumn "user_group_to_impersonate_for_eid" "user_groups" "id")
        ]
      runQuery_ . sqlCreateIndexSequentially "documents" $ indexOnColumn
        "user_group_to_impersonate_for_eid"
      runQuery_ $ sqlDropComposite $ "document_c1"
      runQuery_ $ sqlCreateComposite $ CompositeType
        { ctName    = "document_c1"
        , ctColumns =
          [ CompositeColumn { ccName = "id", ccType = BigIntT }
          , CompositeColumn { ccName = "title", ccType = TextT }
          , CompositeColumn { ccName = "signatory_links"
                            , ccType = ArrayT $ CustomT "signatory_link_c1"
                            }
          , CompositeColumn { ccName = "main_files"
                            , ccType = ArrayT $ CustomT "main_file_c1"
                            }
          , CompositeColumn { ccName = "status", ccType = SmallIntT }
          , CompositeColumn { ccName = "type", ccType = SmallIntT }
          , CompositeColumn { ccName = "ctime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "mtime", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "days_to_sign", ccType = IntegerT }
          , CompositeColumn { ccName = "days_to_remind", ccType = IntegerT }
          , CompositeColumn { ccName = "timeout_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "expiration_date", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_time", ccType = TimestampWithZoneT }
          , CompositeColumn { ccName = "invite_ip", ccType = IntegerT }
          , CompositeColumn { ccName = "invite_text", ccType = TextT }
          , CompositeColumn { ccName = "confirm_text", ccType = TextT }
          , CompositeColumn { ccName = "show_header", ccType = BoolT }
          , CompositeColumn { ccName = "show_pdf_download", ccType = BoolT }
          , CompositeColumn { ccName = "show_reject_option", ccType = BoolT }
          , CompositeColumn { ccName = "allow_reject_reason", ccType = BoolT }
          , CompositeColumn { ccName = "show_footer", ccType = BoolT }
          , CompositeColumn { ccName = "is_receipt", ccType = BoolT }
          , CompositeColumn { ccName = "lang", ccType = SmallIntT }
          , CompositeColumn { ccName = "sharing", ccType = SmallIntT }
          , CompositeColumn { ccName = "tags"
                            , ccType = ArrayT $ CustomT "document_tag_c1"
                            }
          , CompositeColumn { ccName = "author_attachments"
                            , ccType = ArrayT $ CustomT "author_attachment_c1"
                            }
          , CompositeColumn { ccName = "api_v1_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "api_v2_callback_url", ccType = TextT }
          , CompositeColumn { ccName = "unsaved_draft", ccType = BoolT }
          , CompositeColumn { ccName = "object_version", ccType = BigIntT }
          , CompositeColumn { ccName = "token", ccType = BigIntT }
          , CompositeColumn { ccName = "time_zone_name", ccType = TextT }
          , CompositeColumn { ccName = "author_user_group_id", ccType = BigIntT }
          , CompositeColumn { ccName = "status_class", ccType = SmallIntT }
          , CompositeColumn { ccName = "shareable_link_hash", ccType = BigIntT }
          , CompositeColumn { ccName = "template_id", ccType = BigIntT }
          , CompositeColumn { ccName = "from_shareable_link", ccType = BoolT }
          , CompositeColumn { ccName = "show_arrow", ccType = BoolT }
          , CompositeColumn { ccName = "folder_id", ccType = BigIntT }
          , CompositeColumn { ccName = "user_group_to_impersonate_for_eid"
                            , ccType = BigIntT
                            }
          ]
        }
  }
