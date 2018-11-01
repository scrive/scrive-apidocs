module FeatureFlags.Model(
    Features(..)
  , FeatureFlags(..)
  , getFeaturesFor
  , updateFeaturesFor
  , firstAllowedAuthenticationToView
  , firstAllowedAuthenticationToSign
  , firstAllowedInvitationDelivery
  , firstAllowedConfirmationDelivery
) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Unjson

import DB
import Doc.Data.SignatoryLink
import UserGroup.Data

data Features = Features {
    fAdminUsers :: FeatureFlags
  , fRegularUsers :: FeatureFlags
} deriving (Eq, Ord, Show)

instance Unjson Features where
  unjsonDef = objectOf $ Features
    <$> field "admin_users" fAdminUsers "Feature flags for admin users"
    <*> field "regular_users" fRegularUsers "Feature flags for regular users"

data FeatureFlags = FeatureFlags {
    ffCanUseTemplates :: Bool
  , ffCanUseBranding :: Bool
  , ffCanUseAuthorAttachments :: Bool
  , ffCanUseSignatoryAttachments :: Bool
  , ffCanUseMassSendout :: Bool
  , ffCanUseSMSInvitations :: Bool
  , ffCanUseSMSConfirmations :: Bool
  , ffCanUseDKAuthenticationToView :: Bool
  , ffCanUseDKAuthenticationToSign :: Bool
  , ffCanUseFIAuthenticationToView :: Bool
  , ffCanUseNOAuthenticationToView :: Bool
  , ffCanUseNOAuthenticationToSign :: Bool
  , ffCanUseSEAuthenticationToView :: Bool
  , ffCanUseSEAuthenticationToSign :: Bool
  , ffCanUseSMSPinAuthenticationToView :: Bool
  , ffCanUseSMSPinAuthenticationToSign :: Bool
  , ffCanUseStandardAuthenticationToView :: Bool
  , ffCanUseStandardAuthenticationToSign :: Bool
  , ffCanUseEmailInvitations :: Bool
  , ffCanUseEmailConfirmations :: Bool
  , ffCanUseAPIInvitations :: Bool
  , ffCanUsePadInvitations :: Bool
  , ffCanUseShareableLinks :: Bool
} deriving (Eq, Ord, Show)

instance Unjson FeatureFlags where
  unjsonDef = objectOf $ FeatureFlags
    <$> field "can_use_templates" ffCanUseTemplates "Can use templates"
    <*> field "can_use_branding" ffCanUseBranding "Can use branding"
    <*> field "can_use_author_attachments" ffCanUseAuthorAttachments "Can use author attachments"
    <*> field "can_use_signatory_attachments" ffCanUseSignatoryAttachments "Can use signatory attachments"
    <*> field "can_use_mass_sendout" ffCanUseMassSendout "TODO desc"
    <*> field "can_use_sms_invitations" ffCanUseSMSInvitations "TODO desc"
    <*> field "can_use_sms_confirmations" ffCanUseSMSConfirmations "TODO desc"
    <*> field "can_use_dk_authentication_to_view" ffCanUseDKAuthenticationToView "TODO desc"
    <*> field "can_use_dk_authentication_to_sign" ffCanUseDKAuthenticationToSign "TODO desc"
    <*> field "can_use_fi_authentication_to_view" ffCanUseFIAuthenticationToView "TODO desc"
    <*> field "can_use_no_authentication_to_view" ffCanUseNOAuthenticationToView "TODO desc"
    <*> field "can_use_no_authentication_to_sign" ffCanUseNOAuthenticationToSign "TODO desc"
    <*> field "can_use_se_authentication_to_view" ffCanUseSEAuthenticationToView "TODO desc"
    <*> field "can_use_se_authentication_to_sign" ffCanUseSEAuthenticationToSign "TODO desc"
    <*> field "can_use_sms_pin_authentication_to_view" ffCanUseSMSPinAuthenticationToView "TODO desc"
    <*> field "can_use_sms_pin_authentication_to_sign" ffCanUseSMSPinAuthenticationToSign "TODO desc"
    <*> field "can_use_standard_authentication_to_view" ffCanUseStandardAuthenticationToView "TODO desc"
    <*> field "can_use_standard_authentication_to_sign" ffCanUseStandardAuthenticationToSign "TODO desc"
    <*> field "can_use_email_invitations" ffCanUseEmailInvitations "TODO desc"
    <*> field "can_use_email_confirmations" ffCanUseEmailConfirmations "TODO desc"
    <*> fieldDef "can_use_api_invitations" True ffCanUseAPIInvitations "TODO desc"
    <*> fieldDef "can_use_pad_invitations" True ffCanUsePadInvitations "TODO desc"
    <*> field "can_use_shareable_links" ffCanUseShareableLinks "TODO desc"

getFeaturesFor :: (MonadDB m, MonadThrow m) => UserGroupID -> m Features
getFeaturesFor ugid = do
  aff <- dbQuery (GetFeatureFlags ugid True)
  uff <- dbQuery (GetFeatureFlags ugid False)
  return Features { fAdminUsers = aff, fRegularUsers = uff }

updateFeaturesFor :: (MonadDB m, MonadThrow m) => UserGroupID -> Features -> m ()
updateFeaturesFor ugid fs = do
  _ <- dbUpdate (UpdateFeatureFlags ugid (fAdminUsers fs) True)
  _ <- dbUpdate (UpdateFeatureFlags ugid (fRegularUsers fs) False)
  return ()

firstAllowedAuthenticationToView :: FeatureFlags -> AuthenticationToViewMethod
firstAllowedAuthenticationToView ff
  | ffCanUseStandardAuthenticationToView ff = StandardAuthenticationToView
  | ffCanUseSEAuthenticationToView ff       = SEBankIDAuthenticationToView
  | ffCanUseDKAuthenticationToView ff       = DKNemIDAuthenticationToView
  | ffCanUseNOAuthenticationToView ff       = NOBankIDAuthenticationToView
  | ffCanUseFIAuthenticationToView ff       = FITupasAuthenticationToView
  | ffCanUseSMSPinAuthenticationToView ff   = SMSPinAuthenticationToView
  -- Someone can turn off all FFs, not recommended
  | otherwise = StandardAuthenticationToView

firstAllowedAuthenticationToSign :: FeatureFlags -> AuthenticationToSignMethod
firstAllowedAuthenticationToSign ff
  | ffCanUseStandardAuthenticationToSign ff = StandardAuthenticationToSign
  | ffCanUseSEAuthenticationToSign ff       = SEBankIDAuthenticationToSign
  | ffCanUseDKAuthenticationToSign ff       = DKNemIDAuthenticationToSign
  | ffCanUseNOAuthenticationToSign ff       = NOBankIDAuthenticationToSign
  | ffCanUseSMSPinAuthenticationToSign ff   = SMSPinAuthenticationToSign
  -- Someone can turn off all FFs, not recommended
  | otherwise = StandardAuthenticationToSign

firstAllowedInvitationDelivery :: FeatureFlags -> DeliveryMethod
firstAllowedInvitationDelivery ff
  | ffCanUseEmailInvitations ff = EmailDelivery
  | ffCanUseSMSInvitations ff   = MobileDelivery
  | ffCanUseAPIInvitations ff   = APIDelivery
  | ffCanUsePadInvitations ff   = PadDelivery
  -- Someone can turn off all FFs, not recommended
  | otherwise = EmailDelivery

firstAllowedConfirmationDelivery :: FeatureFlags -> ConfirmationDeliveryMethod
firstAllowedConfirmationDelivery ff
  | ffCanUseEmailConfirmations ff = EmailConfirmationDelivery
  | ffCanUseSMSConfirmations ff   = MobileConfirmationDelivery
  | otherwise = NoConfirmationDelivery

data GetFeatureFlags = GetFeatureFlags UserGroupID Bool
instance (MonadDB m,MonadThrow m) => DBQuery m GetFeatureFlags FeatureFlags where
  query (GetFeatureFlags ugid forAdmins) = do
    runQuery_ . sqlSelect "feature_flags" $ do
      sqlWhereEq "user_group_id" ugid
      sqlWhereEq "flags_for_admin" forAdmins
      selectFeatureFlagsSelectors
    fetchOne fetchFeatureFlags

data UpdateFeatureFlags = UpdateFeatureFlags UserGroupID FeatureFlags Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateFeatureFlags Bool where
  update (UpdateFeatureFlags ugid ff forAdmins) = do
    runQuery01 . sqlUpdate "feature_flags" $ do
      sqlSet "can_use_templates" $ ffCanUseTemplates ff
      sqlSet "can_use_branding" $ ffCanUseBranding ff
      sqlSet "can_use_author_attachments" $ ffCanUseAuthorAttachments ff
      sqlSet "can_use_signatory_attachments" $ ffCanUseSignatoryAttachments ff
      sqlSet "can_use_mass_sendout" $ ffCanUseMassSendout ff
      sqlSet "can_use_sms_invitations" $ ffCanUseSMSInvitations ff
      sqlSet "can_use_sms_confirmations" $ ffCanUseSMSConfirmations ff
      sqlSet "can_use_dk_authentication_to_view" $ ffCanUseDKAuthenticationToView ff
      sqlSet "can_use_dk_authentication_to_sign" $ ffCanUseDKAuthenticationToSign ff
      sqlSet "can_use_fi_authentication_to_view" $ ffCanUseFIAuthenticationToView ff
      sqlSet "can_use_no_authentication_to_view" $ ffCanUseNOAuthenticationToView ff
      sqlSet "can_use_no_authentication_to_sign" $ ffCanUseNOAuthenticationToSign ff
      sqlSet "can_use_se_authentication_to_view" $ ffCanUseSEAuthenticationToView ff
      sqlSet "can_use_se_authentication_to_sign" $ ffCanUseSEAuthenticationToSign ff
      sqlSet "can_use_sms_pin_authentication_to_view" $ ffCanUseSMSPinAuthenticationToView ff
      sqlSet "can_use_sms_pin_authentication_to_sign" $ ffCanUseSMSPinAuthenticationToSign ff
      sqlSet "can_use_standard_authentication_to_view" $ ffCanUseStandardAuthenticationToView ff
      sqlSet "can_use_standard_authentication_to_sign" $ ffCanUseStandardAuthenticationToSign ff
      sqlSet "can_use_email_invitations" $ ffCanUseEmailInvitations ff
      sqlSet "can_use_email_confirmations" $ ffCanUseEmailConfirmations ff
      sqlSet "can_use_api_invitations" $ ffCanUseAPIInvitations ff
      sqlSet "can_use_pad_invitations" $ ffCanUsePadInvitations ff
      sqlSet "can_use_shareable_links" $ ffCanUseShareableLinks ff
      sqlWhereEq "user_group_id" ugid
      sqlWhereEq "flags_for_admin" forAdmins

selectFeatureFlagsSelectors :: (SqlResult command) => State command ()
selectFeatureFlagsSelectors = do
  sqlResult "feature_flags.can_use_templates"
  sqlResult "feature_flags.can_use_branding"
  sqlResult "feature_flags.can_use_author_attachments"
  sqlResult "feature_flags.can_use_signatory_attachments"
  sqlResult "feature_flags.can_use_mass_sendout"
  sqlResult "feature_flags.can_use_sms_invitations"
  sqlResult "feature_flags.can_use_sms_confirmations"
  sqlResult "feature_flags.can_use_dk_authentication_to_view"
  sqlResult "feature_flags.can_use_dk_authentication_to_sign"
  sqlResult "feature_flags.can_use_fi_authentication_to_view"
  sqlResult "feature_flags.can_use_no_authentication_to_view"
  sqlResult "feature_flags.can_use_no_authentication_to_sign"
  sqlResult "feature_flags.can_use_se_authentication_to_view"
  sqlResult "feature_flags.can_use_se_authentication_to_sign"
  sqlResult "feature_flags.can_use_sms_pin_authentication_to_view"
  sqlResult "feature_flags.can_use_sms_pin_authentication_to_sign"
  sqlResult "feature_flags.can_use_standard_authentication_to_view"
  sqlResult "feature_flags.can_use_standard_authentication_to_sign"
  sqlResult "feature_flags.can_use_email_invitations"
  sqlResult "feature_flags.can_use_email_confirmations"
  sqlResult "feature_flags.can_use_api_invitations"
  sqlResult "feature_flags.can_use_pad_invitations"
  sqlResult "feature_flags.can_use_shareable_links"

fetchFeatureFlags :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool,
  Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> FeatureFlags
fetchFeatureFlags (
    can_use_templates
  , can_use_branding
  , can_use_author_attachments
  , can_use_signatory_attachments
  , can_use_mass_sendout
  , can_use_sms_invitations
  , can_use_sms_confirmations
  , can_use_dk_authentication_to_view
  , can_use_dk_authentication_to_sign
  , can_use_fi_authentication_to_view
  , can_use_no_authentication_to_view
  , can_use_no_authentication_to_sign
  , can_use_se_authentication_to_view
  , can_use_se_authentication_to_sign
  , can_use_sms_pin_authentication_to_view
  , can_use_sms_pin_authentication_to_sign
  , can_use_standard_authentication_to_view
  , can_use_standard_authentication_to_sign
  , can_use_email_invitations
  , can_use_email_confirmations
  , can_use_api_invitations
  , can_use_pad_invitations
  , can_use_shareable_links
  ) = FeatureFlags {
    ffCanUseTemplates = can_use_templates
  , ffCanUseBranding = can_use_branding
  , ffCanUseAuthorAttachments = can_use_author_attachments
  , ffCanUseSignatoryAttachments = can_use_signatory_attachments
  , ffCanUseMassSendout = can_use_mass_sendout
  , ffCanUseSMSInvitations = can_use_sms_invitations
  , ffCanUseSMSConfirmations = can_use_sms_confirmations
  , ffCanUseDKAuthenticationToView = can_use_dk_authentication_to_view
  , ffCanUseDKAuthenticationToSign = can_use_dk_authentication_to_sign
  , ffCanUseFIAuthenticationToView = can_use_fi_authentication_to_view
  , ffCanUseNOAuthenticationToView = can_use_no_authentication_to_view
  , ffCanUseNOAuthenticationToSign = can_use_no_authentication_to_sign
  , ffCanUseSEAuthenticationToView = can_use_se_authentication_to_view
  , ffCanUseSEAuthenticationToSign = can_use_se_authentication_to_sign
  , ffCanUseSMSPinAuthenticationToView = can_use_sms_pin_authentication_to_view
  , ffCanUseSMSPinAuthenticationToSign = can_use_sms_pin_authentication_to_sign
  , ffCanUseStandardAuthenticationToView = can_use_standard_authentication_to_view
  , ffCanUseStandardAuthenticationToSign = can_use_standard_authentication_to_sign
  , ffCanUseEmailInvitations = can_use_email_invitations
  , ffCanUseEmailConfirmations = can_use_email_confirmations
  , ffCanUseAPIInvitations = can_use_api_invitations
  , ffCanUsePadInvitations = can_use_pad_invitations
  , ffCanUseShareableLinks = can_use_shareable_links
  }
