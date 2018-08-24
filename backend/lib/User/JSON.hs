{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module User.JSON (
    userJSON,
    companyJSON,
    subscriptionJSON,
    userStatsToJSON,
    companyStatsToJSON,
    paymentPlanFromText,
    userNotDeletableReasonToString
    ) where

import Data.String (IsString(..))
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplate.GenericStandard ()
import qualified Data.Text as T

import DataRetentionPolicy
import FeatureFlags.Model
import MinutesTime
import PadApplication.Data
import User.Model
import UserGroup.Data
import UserGroup.Data.PaymentPlan
import Util.HasSomeUserInfo

userJSON :: User -> UserGroup -> JSValue
userJSON user company = runJSONGen $ do
    value "id" $ show $ userid user
    value "fstname" $ getFirstName user
    value "sndname" $ getLastName user
    value "email" $ getEmail user
    value "twofactor_active" $ usertotpactive user
    value "personalnumber" $ getPersonalNumber user
    value "phone" $ userphone $ userinfo user
    value "companyadmin" $ useriscompanyadmin user
    value "companyposition" $ usercompanyposition $ userinfo user
    value "lang"   $ codeFromLang $ getLang user
    value "company" $ companyJSON False company

companyJSON :: Bool -> UserGroup -> JSValue
companyJSON forAdmin ug = runJSONGen $ do
    value "companyid" $ show $ get ugID ug
    value "address" $ unpack $ get (ugaAddress . ugAddress) ug
    value "city" $ unpack $ get (ugaCity  . ugAddress) ug
    value "country" $ unpack $ get (ugaCountry . ugAddress) ug
    value "zip" $ unpack $ get (ugaZip . ugAddress) ug
    value "companyname" $ unpack $ get ugName ug
    value "companynumber" $ unpack $ get (ugaCompanyNumber . ugAddress) ug
    value "cgidisplayname" $ unpack <$> get (ugsCGIDisplayName . ugSettings) ug
    value "cgiserviceid" $ unpack <$> get (ugsCGIServiceID . ugSettings) ug
    value "ipaddressmasklist" $ intercalate "," $ fmap show $  get (ugsIPAddressMaskList . ugSettings) ug
    value "idledoctimeoutpreparation" $ get (drpIdleDocTimeoutPreparation . ugsDataRetentionPolicy . ugSettings) ug
    value "idledoctimeoutclosed" $ get (drpIdleDocTimeoutClosed . ugsDataRetentionPolicy . ugSettings) ug
    value "idledoctimeoutcanceled" $ get (drpIdleDocTimeoutCanceled . ugsDataRetentionPolicy . ugSettings) ug
    value "idledoctimeouttimedout" $ get (drpIdleDocTimeoutTimedout . ugsDataRetentionPolicy . ugSettings) ug
    value "idledoctimeoutrejected" $ get (drpIdleDocTimeoutRejected . ugsDataRetentionPolicy . ugSettings) ug
    value "idledoctimeouterror" $ get (drpIdleDocTimeoutError . ugsDataRetentionPolicy . ugSettings) ug
    value "immediatetrash" $ get (drpImmediateTrash . ugsDataRetentionPolicy . ugSettings) ug
    value "smsprovider" $ show $ get (ugsSMSProvider . ugSettings) ug
    value "padappmode" $ unpack $ padAppModeText $ get (ugsPadAppMode . ugSettings) ug
    value "padearchiveenabled" $ get (ugsPadEarchiveEnabled . ugSettings) ug
    when forAdmin $ value "partnerid" $ show <$> (get ugParentGroupID ug)
  where
    unpack :: T.Text -> String
    unpack = T.unpack

paymentPlanText :: PaymentPlan -> String
paymentPlanText FreePlan       = "free"
paymentPlanText OnePlan        = "one"
paymentPlanText TeamPlan       = "team"
paymentPlanText EnterprisePlan = "enterprise"
paymentPlanText TrialPlan      = "trial"

paymentPlanFromText :: String -> Maybe PaymentPlan
paymentPlanFromText s = find (\p -> s == paymentPlanText p) allAvailablePlans
  where
    allAvailablePlans = [FreePlan, OnePlan, TeamPlan, EnterprisePlan, TrialPlan]

subscriptionJSON  :: UserGroup -> [User] -> Int -> FeatureFlags -> JSValue
subscriptionJSON ug users startedLastMonth ff = runJSONGen $ do
  value "payment_plan" . paymentPlanText . fromJust . ugPaymentPlan $ ug
  value "number_of_users" $ length users
  value "started_last_month" $ startedLastMonth
  value "can_use_templates" $ ffCanUseTemplates ff
  value "can_use_branding" $ ffCanUseBranding ff
  value "can_use_author_attachments" $ ffCanUseAuthorAttachments ff
  value "can_use_signatory_attachments" $ ffCanUseSignatoryAttachments ff
  value "can_use_mass_sendout" $ ffCanUseMassSendout ff
  value "can_use_sms_invitations" $ ffCanUseSMSInvitations ff
  value "can_use_sms_confirmations" $ ffCanUseSMSConfirmations ff
  value "can_use_dk_authentication_to_view" $ ffCanUseDKAuthenticationToView ff
  value "can_use_dk_authentication_to_sign" $ ffCanUseDKAuthenticationToSign ff
  value "can_use_no_authentication_to_view" $ ffCanUseNOAuthenticationToView ff
  value "can_use_no_authentication_to_sign" $ ffCanUseNOAuthenticationToSign ff
  value "can_use_se_authentication_to_view" $ ffCanUseSEAuthenticationToView ff
  value "can_use_se_authentication_to_sign" $ ffCanUseSEAuthenticationToSign ff
  value "can_use_sms_pin_authentication_to_view" $ ffCanUseSMSPinAuthenticationToView ff
  value "can_use_sms_pin_authentication_to_sign" $ ffCanUseSMSPinAuthenticationToSign ff

userStatsToJSON :: (UTCTime -> String) -> [UserUsageStats] -> JSValue
userStatsToJSON formatTime uuss = runJSONGen . objects "stats" . for uuss $ \uus -> do
  let DocumentStats{..} = uusDocumentStats uus
  value "date" . formatTime $ uusTimeWindowStart uus
  value "sent" dsDocumentsSent
  value "closed" dsDocumentsClosed
  value "signatures" dsSignaturesClosed

companyStatsToJSON :: (UTCTime -> String) -> String -> [UserUsageStats] -> JSValue
companyStatsToJSON formatTime textName uuss = runJSONGen . objects "stats" . for uussGrouped $ \uusGroup -> do
  let summary = foldMap uusDocumentStats uusGroup
  value "date" . formatTime . uusTimeWindowStart $ head uusGroup
  value "name" textName
  value "sent" $ dsDocumentsSent summary
  value "closed" $ dsDocumentsClosed summary
  value "signatures" $ dsSignaturesClosed summary
  objects "user_stats" . for uusGroup $ \uus -> do
    let DocumentStats{..} = uusDocumentStats uus
    value "date" . formatTime $ uusTimeWindowStart uus
    value "email" $ uusUserEmail uus
    value "name" $ uusUserName uus
    value "sent" dsDocumentsSent
    value "closed" dsDocumentsClosed
    value "signatures" dsSignaturesClosed
  where
    uussGrouped :: [[UserUsageStats]]
    uussGrouped = groupBy sameTimeWindow uuss
      where
        sameTimeWindow u1 u2 = uusTimeWindowStart u1 == uusTimeWindowStart u2

instance ToJSValue UserNotDeletableReason where
  toJSValue reason =
    let code = case reason of
          UserNotDeletableDueToPendingDocuments -> "pending_documents"
        msg = userNotDeletableReasonToString reason
    in JSObject $ toJSObject
         [ ("code",    JSString $ toJSString code)
         , ("message", JSString $ toJSString msg)
         ]

userNotDeletableReasonToString :: IsString s => UserNotDeletableReason -> s
userNotDeletableReasonToString = fromString . \case
  UserNotDeletableDueToPendingDocuments ->
    "Can't delete a user with pending documents."
