module FeatureFlags.Model(
    FeatureFlags(..)
  , GetFeatureFlags(..)
  , UpdateFeatureFlags(..)
) where

import Control.Monad.Catch
import Control.Monad.State
import Crypto.RNG

import Company.CompanyID
import DB

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
  , ffCanUseNOAuthenticationToView :: Bool
  , ffCanUseNOAuthenticationToSign :: Bool
  , ffCanUseSEAuthenticationToView :: Bool
  , ffCanUseSEAuthenticationToSign :: Bool
  , ffCanUseSMSPinAuthenticationToView :: Bool
  , ffCanUseSMSPinAuthenticationToSign :: Bool
} deriving (Eq, Ord, Show)


data GetFeatureFlags = GetFeatureFlags CompanyID
instance (MonadDB m,MonadThrow m) => DBQuery m GetFeatureFlags FeatureFlags where
  query (GetFeatureFlags cid) = do
    runQuery_ . sqlSelect "feature_flags" $ do
      sqlWhereEq "company_id" cid
      selectFeatureFlagsSelectors
    fetchOne fetchFeatureFlags

data UpdateFeatureFlags = UpdateFeatureFlags CompanyID FeatureFlags
instance (MonadDB m, MonadThrow m, CryptoRNG m) => DBUpdate m UpdateFeatureFlags Bool where
  update (UpdateFeatureFlags cid ff) = do
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
      sqlSet "can_use_no_authentication_to_view" $ ffCanUseNOAuthenticationToView ff
      sqlSet "can_use_no_authentication_to_sign" $ ffCanUseNOAuthenticationToSign ff
      sqlSet "can_use_se_authentication_to_view" $ ffCanUseSEAuthenticationToView ff
      sqlSet "can_use_se_authentication_to_sign" $ ffCanUseSEAuthenticationToSign ff
      sqlSet "can_use_sms_pin_authentication_to_view" $ ffCanUseSMSPinAuthenticationToView ff
      sqlSet "can_use_sms_pin_authentication_to_sign" $ ffCanUseSMSPinAuthenticationToSign ff
      sqlWhereEq "company_id" cid

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
  sqlResult "feature_flags.can_use_no_authentication_to_view"
  sqlResult "feature_flags.can_use_no_authentication_to_sign"
  sqlResult "feature_flags.can_use_se_authentication_to_view"
  sqlResult "feature_flags.can_use_se_authentication_to_sign"
  sqlResult "feature_flags.can_use_sms_pin_authentication_to_view"
  sqlResult "feature_flags.can_use_sms_pin_authentication_to_sign"

fetchFeatureFlags :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> FeatureFlags
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
  , can_use_no_authentication_to_view
  , can_use_no_authentication_to_sign
  , can_use_se_authentication_to_view
  , can_use_se_authentication_to_sign
  , can_use_sms_pin_authentication_to_view
  , can_use_sms_pin_authentication_to_sign
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
  , ffCanUseNOAuthenticationToView = can_use_no_authentication_to_view
  , ffCanUseNOAuthenticationToSign = can_use_no_authentication_to_sign
  , ffCanUseSEAuthenticationToView = can_use_se_authentication_to_view
  , ffCanUseSEAuthenticationToSign = can_use_se_authentication_to_sign
  , ffCanUseSMSPinAuthenticationToView = can_use_sms_pin_authentication_to_view
  , ffCanUseSMSPinAuthenticationToSign = can_use_sms_pin_authentication_to_sign

  }
