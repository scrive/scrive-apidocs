module FeatureFlags.Migrations (
  createFeatureFlags
, featureFlagsAddNOAuthToSign
, featureFlagsAddSMSPinAuthToView
, featureFlagsAddDKAuthToSign
, featureFlagsAddUserGroupID
, featureFlagsDropCompanyID
, featureFlagsAddStandardAuthAndFlagsForAdmin
, featureFlagsAddEmailInvitation
, featureFlagsAddFIAuthToView
, featureFlagsAddEmailConfirmation
, featureFlagsAddShareableLinks
, featureFlagsAddForwarding
, featureFlagsAddNotificationDeliveryMethod
, featureFlagsRemoveDefaultValuesFromColumns
, renameFeatureFlagsComposite
) where

import Control.Monad.Catch
import Database.PostgreSQL.PQTypes.Checks

import DB
import FeatureFlags.Tables

renameFeatureFlagsComposite :: MonadDB m => Migration m
renameFeatureFlagsComposite = Migration
  { mgrTableName = "feature_flags"
  , mgrFrom = 14
  , mgrAction = StandardMigration $ do
      runSQL_ "ALTER TYPE feature_flags_ct RENAME TO feature_flags_c1"
  }

createFeatureFlags :: MonadDB m => Migration m
createFeatureFlags = Migration {
    mgrTableName = tblName tableFeatureFlags
  , mgrFrom = 0
  , mgrAction = StandardMigration $ do
      createTable True tblTable {
        tblName = "feature_flags"
        , tblVersion = 1
        , tblColumns =
          [ tblColumn { colName = "company_id", colType = BigSerialT, colNullable = False }
          , tblColumn { colName = "can_use_templates", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_branding", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_author_attachments", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_signatory_attachments", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_mass_sendout", colType = BoolT, colNullable = False, colDefault = Just "true" }

          , tblColumn { colName = "can_use_sms_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_sms_confirmations", colType = BoolT, colNullable = False, colDefault = Just "true" }

          , tblColumn { colName = "can_use_dk_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_no_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_se_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_se_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
          , tblColumn { colName = "can_use_sms_pin_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
          ]
        , tblPrimaryKey = pkOnColumn "company_id"
        , tblForeignKeys = [
            (fkOnColumn "company_id" "companies" "id") { fkOnDelete = ForeignKeyCascade }
          ]
        }
      runQuery_ . sqlInsertSelect "feature_flags" "companies c" $ do
        sqlSetCmd "company_id" "c.id"
  }

featureFlagsAddNOAuthToSign :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddNOAuthToSign = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 1
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [ sqlAddColumn $
        tblColumn { colName = "can_use_no_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddSMSPinAuthToView :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddSMSPinAuthToView = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 2
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [ sqlAddColumn $
        tblColumn { colName = "can_use_sms_pin_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddDKAuthToSign :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddDKAuthToSign = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 3
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [ sqlAddColumn $
        tblColumn { colName = "can_use_dk_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddUserGroupID :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddUserGroupID = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 4
, mgrAction = StandardMigration $ do
    let tname = tblName tableFeatureFlags
    runQuery_ $ sqlAlterTable tname
      [
        sqlAddColumn $ tblColumn { colName = "user_group_id", colType = BigIntT, colNullable = True }
      ,  sqlAddValidFK tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeySetNull }
      ]
    runQuery_ . sqlCreateIndexSequentially tname $ indexOnColumn "user_group_id"
}


featureFlagsDropCompanyID :: MonadDB m => Migration m
featureFlagsDropCompanyID = Migration {
    mgrTableName = tblName tableFeatureFlags
  , mgrFrom = 5
  , mgrAction = StandardMigration $ do
      let tname = tblName tableFeatureFlags
      runQuery_ $ sqlAlterTable tname [
          sqlAlterColumn "user_group_id" "SET NOT NULL"
        , sqlDropFK tname $ (fkOnColumn "company_id" "companies" "id")
        , sqlDropFK tname $ (fkOnColumn "user_group_id" "user_groups" "id")
        , sqlAddValidFK  tname $ (fkOnColumn "user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
        , sqlDropPK tname
        , sqlAddPK tname (fromJust $ pkOnColumn "user_group_id")
        , sqlDropColumn "company_id"
        ]
      runQuery_ $ sqlDropIndex tname $ (indexOnColumn "user_group_id")

  }

featureFlagsAddStandardAuthAndFlagsForAdmin :: MonadDB m => Migration m
featureFlagsAddStandardAuthAndFlagsForAdmin = Migration {
    mgrTableName = tname
  , mgrFrom = 6
  , mgrAction = StandardMigration $ do
      runQuery_ $ sqlAlterTable tname
        [ sqlAddColumn $
            tblColumn { colName = "can_use_standard_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
        , sqlAddColumn $
            tblColumn { colName = "can_use_standard_authentication_to_sign", colType = BoolT, colNullable = False, colDefault = Just "true" }
        , sqlAddColumn $
            tblColumn { colName = "flags_for_admin", colType = BoolT, colNullable = False, colDefault = Just "false" }
        ]
      runQuery_ $ sqlAlterTable tname
        [ sqlAlterColumn "flags_for_admin" "DROP DEFAULT"
        , sqlDropPK tname
        , sqlAddPK tname (fromJust $ pkOnColumns ["user_group_id", "flags_for_admin"])
        ]
      runSQL_ $ "INSERT INTO feature_flags (flags_for_admin,user_group_id," <+>
                " can_use_templates,can_use_branding,can_use_author_attachments," <+>
                " can_use_signatory_attachments,can_use_mass_sendout," <+>
                " can_use_sms_invitations,can_use_sms_confirmations," <+>
                " can_use_dk_authentication_to_view,can_use_dk_authentication_to_sign," <+>
                " can_use_no_authentication_to_view,can_use_no_authentication_to_sign," <+>
                " can_use_se_authentication_to_view,can_use_se_authentication_to_sign," <+>
                " can_use_sms_pin_authentication_to_view,can_use_sms_pin_authentication_to_sign," <+>
                " can_use_standard_authentication_to_view,can_use_standard_authentication_to_sign" <+>
                ") SELECT true, user_group_id," <+>
                " can_use_templates,can_use_branding,can_use_author_attachments," <+>
                " can_use_signatory_attachments,can_use_mass_sendout," <+>
                " can_use_sms_invitations,can_use_sms_confirmations," <+>
                " can_use_dk_authentication_to_view,can_use_dk_authentication_to_sign," <+>
                " can_use_no_authentication_to_view,can_use_no_authentication_to_sign," <+>
                " can_use_se_authentication_to_view,can_use_se_authentication_to_sign," <+>
                " can_use_sms_pin_authentication_to_view,can_use_sms_pin_authentication_to_sign," <+>
                " can_use_standard_authentication_to_view,can_use_standard_authentication_to_sign" <+>
                "FROM feature_flags WHERE flags_for_admin = false"
  }
  where tname = tblName tableFeatureFlags

featureFlagsAddEmailInvitation :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddEmailInvitation = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 7
, mgrAction = StandardMigration .
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [
        sqlAddColumn $ tblColumn { colName = "can_use_email_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
      , sqlAddColumn $ tblColumn { colName = "can_use_api_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
      , sqlAddColumn $ tblColumn { colName = "can_use_pad_invitations", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddFIAuthToView :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddFIAuthToView = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 8
, mgrAction = StandardMigration $ do
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [ sqlAddColumn $
        tblColumn { colName = "can_use_fi_authentication_to_view", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddEmailConfirmation :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddEmailConfirmation = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 9
, mgrAction = StandardMigration .
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [
        sqlAddColumn $ tblColumn { colName = "can_use_email_confirmations", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddShareableLinks :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddShareableLinks = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 10
, mgrAction = StandardMigration .
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [
        sqlAddColumn $ tblColumn { colName = "can_use_shareable_links", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}

featureFlagsAddForwarding :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsAddForwarding = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 11
, mgrAction = StandardMigration .
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [
        sqlAddColumn $ tblColumn { colName = "can_use_forwarding", colType = BoolT, colNullable = False, colDefault = Just "true" }
      ]
}


featureFlagsRemoveDefaultValuesFromColumns :: (MonadThrow m, MonadDB m) => Migration m
featureFlagsRemoveDefaultValuesFromColumns = Migration {
  mgrTableName = tblName tableFeatureFlags
, mgrFrom = 12
, mgrAction = StandardMigration .
    runQuery_ $ sqlAlterTable (tblName tableFeatureFlags)  [
          sqlAlterColumn "can_use_templates" "DROP DEFAULT"
        , sqlAlterColumn "can_use_branding" "DROP DEFAULT"
        , sqlAlterColumn "can_use_author_attachments" "DROP DEFAULT"
        , sqlAlterColumn "can_use_signatory_attachments" "DROP DEFAULT"
        , sqlAlterColumn "can_use_mass_sendout" "DROP DEFAULT"
        , sqlAlterColumn "can_use_sms_invitations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_sms_confirmations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_dk_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_no_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_se_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_se_authentication_to_sign" "DROP DEFAULT"
        , sqlAlterColumn "can_use_sms_pin_authentication_to_sign" "DROP DEFAULT"
        , sqlAlterColumn "can_use_no_authentication_to_sign" "DROP DEFAULT"
        , sqlAlterColumn "can_use_sms_pin_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_dk_authentication_to_sign" "DROP DEFAULT"
        , sqlAlterColumn "can_use_standard_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_standard_authentication_to_sign" "DROP DEFAULT"
        , sqlAlterColumn "can_use_email_invitations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_api_invitations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_pad_invitations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_fi_authentication_to_view" "DROP DEFAULT"
        , sqlAlterColumn "can_use_email_confirmations" "DROP DEFAULT"
        , sqlAlterColumn "can_use_shareable_links" "DROP DEFAULT"
        , sqlAlterColumn "can_use_forwarding" "DROP DEFAULT"
      ]
}

featureFlagsAddNotificationDeliveryMethod :: MonadDB m => Migration m
featureFlagsAddNotificationDeliveryMethod =
  let
    tableSpec = tableFeatureFlags
    tableName = tblName tableSpec
    columnName = "can_use_document_party_notifications"
  in
    Migration {
      mgrTableName = tableName
    , mgrFrom = 13
    , mgrAction = StandardMigration $ do
        runQuery_ $ sqlAlterTable tableName  [
          sqlAddColumn $ tblColumn { colName = columnName, colType = BoolT, colNullable = False, colDefault = Just "false" }
          ]
        runQuery_ $ sqlAlterTable tableName [ sqlAlterColumn columnName "DROP DEFAULT" ]
  }
