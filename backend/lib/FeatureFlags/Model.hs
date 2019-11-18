module FeatureFlags.Model
  ( Features(..)
  , FeatureFlags(..)
  , defaultFeatures
  , firstAllowedAuthenticationToView
  , firstAllowedAuthenticationToSign
  , firstAllowedInvitationDelivery
  , firstAllowedConfirmationDelivery
  , setFeatureFlagsSql
  , selectFeatureFlagsSelectors
  ) where

import Control.Monad.State
import Data.Unjson
import Database.PostgreSQL.PQTypes.Model.CompositeType

import DB
import Doc.Types.SignatoryLink
import FeatureFlags.Tables
import UserGroup.Types.PaymentPlan

data Features = Features
  { fAdminUsers :: FeatureFlags
  , fRegularUsers :: FeatureFlags
  } deriving (Eq, Ord, Show)

instance Unjson Features where
  unjsonDef =
    objectOf
      $   Features
      <$> field "admin_users"   fAdminUsers   "Feature flags for admin users"
      <*> field "regular_users" fRegularUsers "Feature flags for regular users"

data FeatureFlags = FeatureFlags
  { ffCanUseTemplates :: Bool
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
  , ffCanUseVerimiAuthenticationToView :: Bool
  , ffCanUseIDINAuthenticationToView :: Bool
  , ffCanUseIDINAuthenticationToSign :: Bool
  , ffCanUseEmailInvitations :: Bool
  , ffCanUseEmailConfirmations :: Bool
  , ffCanUseAPIInvitations :: Bool
  , ffCanUsePadInvitations :: Bool
  , ffCanUseShareableLinks :: Bool
  , ffCanUseForwarding :: Bool
  , ffCanUseDocumentPartyNotifications :: Bool
  , ffCanUsePortal :: Bool
  } deriving (Eq, Ord, Show)

instance Unjson FeatureFlags where
  unjsonDef =
    objectOf
      $   FeatureFlags
      <$> field "can_use_templates" ffCanUseTemplates "Can use templates"
      <*> field "can_use_branding"  ffCanUseBranding  "Can use branding"
      <*> field "can_use_author_attachments"
                ffCanUseAuthorAttachments
                "Can use author attachments"
      <*> field "can_use_signatory_attachments"
                ffCanUseSignatoryAttachments
                "Can use signatory attachments"
      <*> field "can_use_mass_sendout"      ffCanUseMassSendout      "TODO desc"
      <*> field "can_use_sms_invitations"   ffCanUseSMSInvitations   "TODO desc"
      <*> field "can_use_sms_confirmations" ffCanUseSMSConfirmations "TODO desc"
      <*> field "can_use_dk_authentication_to_view"
                ffCanUseDKAuthenticationToView
                "TODO desc"
      <*> field "can_use_dk_authentication_to_sign"
                ffCanUseDKAuthenticationToSign
                "TODO desc"
      <*> field "can_use_fi_authentication_to_view"
                ffCanUseFIAuthenticationToView
                "TODO desc"
      <*> field "can_use_no_authentication_to_view"
                ffCanUseNOAuthenticationToView
                "TODO desc"
      <*> field "can_use_no_authentication_to_sign"
                ffCanUseNOAuthenticationToSign
                "TODO desc"
      <*> field "can_use_se_authentication_to_view"
                ffCanUseSEAuthenticationToView
                "TODO desc"
      <*> field "can_use_se_authentication_to_sign"
                ffCanUseSEAuthenticationToSign
                "TODO desc"
      <*> field "can_use_sms_pin_authentication_to_view"
                ffCanUseSMSPinAuthenticationToView
                "TODO desc"
      <*> field "can_use_sms_pin_authentication_to_sign"
                ffCanUseSMSPinAuthenticationToSign
                "TODO desc"
      <*> field "can_use_standard_authentication_to_view"
                ffCanUseStandardAuthenticationToView
                "TODO desc"
      <*> field "can_use_standard_authentication_to_sign"
                ffCanUseStandardAuthenticationToSign
                "TODO desc"
      <*> field "can_use_verimi_authentication_to_view"
                ffCanUseVerimiAuthenticationToView
                "TODO desc"
      <*> field "can_use_idin_authentication_to_view"
                ffCanUseIDINAuthenticationToView
                "TODO desc"
      <*> field "can_use_idin_authentication_to_sign"
                ffCanUseIDINAuthenticationToSign
                "TODO desc"
      <*> field "can_use_email_invitations"   ffCanUseEmailInvitations   "TODO desc"
      <*> field "can_use_email_confirmations" ffCanUseEmailConfirmations "TODO desc"
      <*> fieldDef "can_use_api_invitations" True ffCanUseAPIInvitations "TODO desc"
      <*> fieldDef "can_use_pad_invitations" True ffCanUsePadInvitations "TODO desc"
      <*> field "can_use_shareable_links" ffCanUseShareableLinks "TODO desc"
      <*> field "can_use_forwarding"      ffCanUseForwarding     "TODO desc"
      <*> field "can_use_document_party_notifications"
                ffCanUseDocumentPartyNotifications
                "Can use document notifications"
      <*> field "can_use_portal" ffCanUsePortal "TODO desc"



type instance CompositeRow FeatureFlags
  = ( Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    )

instance PQFormat FeatureFlags where
  pqFormat = compositeTypePqFormat ctFeatureFlags

instance CompositeFromSQL FeatureFlags where
  toComposite (ffCanUseTemplates, ffCanUseBranding, ffCanUseAuthorAttachments, ffCanUseSignatoryAttachments, ffCanUseMassSendout, ffCanUseSMSInvitations, ffCanUseSMSConfirmations, ffCanUseDKAuthenticationToView, ffCanUseDKAuthenticationToSign, ffCanUseFIAuthenticationToView, ffCanUseNOAuthenticationToView, ffCanUseNOAuthenticationToSign, ffCanUseSEAuthenticationToView, ffCanUseSEAuthenticationToSign, ffCanUseSMSPinAuthenticationToView, ffCanUseSMSPinAuthenticationToSign, ffCanUseStandardAuthenticationToView, ffCanUseStandardAuthenticationToSign, ffCanUseVerimiAuthenticationToView, ffCanUseIDINAuthenticationToView, ffCanUseIDINAuthenticationToSign, ffCanUseEmailInvitations, ffCanUseEmailConfirmations, ffCanUseAPIInvitations, ffCanUsePadInvitations, ffCanUseShareableLinks, ffCanUseForwarding, ffCanUseDocumentPartyNotifications, ffCanUsePortal)
    = FeatureFlags { .. }

firstAllowedAuthenticationToView :: FeatureFlags -> AuthenticationToViewMethod
firstAllowedAuthenticationToView ff
  | ffCanUseStandardAuthenticationToView ff = StandardAuthenticationToView
  | ffCanUseSEAuthenticationToView ff = SEBankIDAuthenticationToView
  | ffCanUseDKAuthenticationToView ff = DKNemIDAuthenticationToView
  | ffCanUseNOAuthenticationToView ff = NOBankIDAuthenticationToView
  | ffCanUseFIAuthenticationToView ff = FITupasAuthenticationToView
  | ffCanUseSMSPinAuthenticationToView ff = SMSPinAuthenticationToView
  | ffCanUseVerimiAuthenticationToView ff = VerimiAuthenticationToView
  | ffCanUseIDINAuthenticationToView ff = IDINAuthenticationToView
  |
  -- Someone can turn off all FFs, not recommended
    otherwise = StandardAuthenticationToView

firstAllowedAuthenticationToSign :: FeatureFlags -> AuthenticationToSignMethod
firstAllowedAuthenticationToSign ff
  | ffCanUseStandardAuthenticationToSign ff = StandardAuthenticationToSign
  | ffCanUseSEAuthenticationToSign ff = SEBankIDAuthenticationToSign
  | ffCanUseDKAuthenticationToSign ff = DKNemIDAuthenticationToSign
  | ffCanUseNOAuthenticationToSign ff = NOBankIDAuthenticationToSign
  | ffCanUseSMSPinAuthenticationToSign ff = SMSPinAuthenticationToSign
  | ffCanUseIDINAuthenticationToSign ff = IDINAuthenticationToSign
  |
  -- Someone can turn off all FFs, not recommended
    otherwise = StandardAuthenticationToSign

firstAllowedInvitationDelivery :: FeatureFlags -> DeliveryMethod
firstAllowedInvitationDelivery ff | ffCanUseEmailInvitations ff = EmailDelivery
                                  | ffCanUseSMSInvitations ff   = MobileDelivery
                                  | ffCanUseAPIInvitations ff   = APIDelivery
                                  | ffCanUsePadInvitations ff   = PadDelivery
                                  |
  -- Someone can turn off all FFs, not recommended
                                    otherwise                   = EmailDelivery

firstAllowedConfirmationDelivery :: FeatureFlags -> ConfirmationDeliveryMethod
firstAllowedConfirmationDelivery ff
  | ffCanUseEmailConfirmations ff = EmailConfirmationDelivery
  | ffCanUseSMSConfirmations ff   = MobileConfirmationDelivery
  | otherwise                     = NoConfirmationDelivery

defaultFeatures :: PaymentPlan -> Features
defaultFeatures paymentPlan = Features ff ff
  where
    defaultFF = FeatureFlags { ffCanUseTemplates                  = True
                             , ffCanUseBranding                   = True
                             , ffCanUseAuthorAttachments          = True
                             , ffCanUseSignatoryAttachments       = True
                             , ffCanUseMassSendout                = True
                             , ffCanUseSMSInvitations             = True
                             , ffCanUseSMSConfirmations           = True
                             , ffCanUseDKAuthenticationToView     = True
                             , ffCanUseDKAuthenticationToSign     = True
                             , ffCanUseFIAuthenticationToView     = True
                             , ffCanUseNOAuthenticationToView     = True
                             , ffCanUseNOAuthenticationToSign     = True
                             , ffCanUseSEAuthenticationToView     = True
                             , ffCanUseSEAuthenticationToSign     = True
                             , ffCanUseSMSPinAuthenticationToView = True
                             , ffCanUseSMSPinAuthenticationToSign = True
                             , ffCanUseStandardAuthenticationToView = True
                             , ffCanUseStandardAuthenticationToSign = True
                             , ffCanUseVerimiAuthenticationToView = True
                             , ffCanUseIDINAuthenticationToView   = True
                             , ffCanUseIDINAuthenticationToSign   = True
                             , ffCanUseEmailInvitations           = True
                             , ffCanUseEmailConfirmations         = True
                             , ffCanUseAPIInvitations             = True
                             , ffCanUsePadInvitations             = True
                             , ffCanUseShareableLinks             = False
                             , ffCanUseForwarding                 = True
                             , ffCanUseDocumentPartyNotifications = False
                             , ffCanUsePortal                     = False
                             }
    ff = case paymentPlan of
      FreePlan -> defaultFF { ffCanUseDKAuthenticationToView     = False
                            , ffCanUseDKAuthenticationToSign     = False
                            , ffCanUseFIAuthenticationToView     = False
                            , ffCanUseNOAuthenticationToView     = False
                            , ffCanUseNOAuthenticationToSign     = False
                            , ffCanUseSEAuthenticationToView     = False
                            , ffCanUseSEAuthenticationToSign     = False
                            , ffCanUseVerimiAuthenticationToView = False
                            , ffCanUseIDINAuthenticationToView   = False
                            , ffCanUseIDINAuthenticationToSign   = False
                            }
      _ -> defaultFF

setFeatureFlagsSql :: (SqlSet command) => FeatureFlags -> State command ()
setFeatureFlagsSql ff = do
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
  sqlSet "can_use_standard_authentication_to_view"
    $ ffCanUseStandardAuthenticationToView ff
  sqlSet "can_use_standard_authentication_to_sign"
    $ ffCanUseStandardAuthenticationToSign ff
  sqlSet "can_use_verimi_authentication_to_view" $ ffCanUseVerimiAuthenticationToView ff
  sqlSet "can_use_idin_authentication_to_view" $ ffCanUseIDINAuthenticationToView ff
  sqlSet "can_use_idin_authentication_to_sign" $ ffCanUseIDINAuthenticationToSign ff
  sqlSet "can_use_email_invitations" $ ffCanUseEmailInvitations ff
  sqlSet "can_use_email_confirmations" $ ffCanUseEmailConfirmations ff
  sqlSet "can_use_api_invitations" $ ffCanUseAPIInvitations ff
  sqlSet "can_use_pad_invitations" $ ffCanUsePadInvitations ff
  sqlSet "can_use_shareable_links" $ ffCanUseShareableLinks ff
  sqlSet "can_use_forwarding" $ ffCanUseForwarding ff
  sqlSet "can_use_document_party_notifications" $ ffCanUseDocumentPartyNotifications ff
  sqlSet "can_use_portal" $ ffCanUsePortal ff

selectFeatureFlagsSelectors :: [SQL]
selectFeatureFlagsSelectors =
  [ "feature_flags.can_use_templates"
  , "feature_flags.can_use_branding"
  , "feature_flags.can_use_author_attachments"
  , "feature_flags.can_use_signatory_attachments"
  , "feature_flags.can_use_mass_sendout"
  , "feature_flags.can_use_sms_invitations"
  , "feature_flags.can_use_sms_confirmations"
  , "feature_flags.can_use_dk_authentication_to_view"
  , "feature_flags.can_use_dk_authentication_to_sign"
  , "feature_flags.can_use_fi_authentication_to_view"
  , "feature_flags.can_use_no_authentication_to_view"
  , "feature_flags.can_use_no_authentication_to_sign"
  , "feature_flags.can_use_se_authentication_to_view"
  , "feature_flags.can_use_se_authentication_to_sign"
  , "feature_flags.can_use_sms_pin_authentication_to_view"
  , "feature_flags.can_use_sms_pin_authentication_to_sign"
  , "feature_flags.can_use_standard_authentication_to_view"
  , "feature_flags.can_use_standard_authentication_to_sign"
  , "feature_flags.can_use_verimi_authentication_to_view"
  , "feature_flags.can_use_idin_authentication_to_view"
  , "feature_flags.can_use_idin_authentication_to_sign"
  , "feature_flags.can_use_email_invitations"
  , "feature_flags.can_use_email_confirmations"
  , "feature_flags.can_use_api_invitations"
  , "feature_flags.can_use_pad_invitations"
  , "feature_flags.can_use_shareable_links"
  , "feature_flags.can_use_forwarding"
  , "feature_flags.can_use_document_party_notifications"
  , "feature_flags.can_use_portal"
  ]
