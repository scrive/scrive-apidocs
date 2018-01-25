{-# LANGUAGE ExtendedDefaultRules #-}
module User.JSON (
    userJSON,
    companyJSON,
    subscriptionJSON,
    userStatsToJSON,
    companyStatsToJSON,
    paymentPlanFromText
    ) where

import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplate.GenericStandard ()
import qualified Data.Text as T

import Company.Model
import FeatureFlags.Model
import KontraPrelude
import MinutesTime
import PadApplication.Data
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

userJSON :: User -> Company -> JSValue
userJSON user company = runJSONGen $ do
    value "id" $ show $ userid user
    value "fstname" $ getFirstName user
    value "sndname" $ getLastName user
    value "email" $ getEmail user
    value "personalnumber" $ getPersonalNumber user
    value "phone" $ userphone $ userinfo user
    value "companyadmin" $ useriscompanyadmin user
    value "companyposition" $ usercompanyposition $ userinfo user
    value "lang"   $ codeFromLang $ getLang user
    value "company" $ companyJSON company

companyJSON :: Company -> JSValue
companyJSON company = runJSONGen $ do
    value "companyid" $ show $ companyid company
    value "address" $ companyaddress $ companyinfo company
    value "city" $ companycity $ companyinfo company
    value "country" $ companycountry $ companyinfo company
    value "zip" $ companyzip $ companyinfo company
    value "companyname" $ getCompanyName company
    value "companynumber" $ getCompanyNumber company
    value "cgidisplayname" $ companycgidisplayname $ companyinfo company
    value "cgiserviceid" $ companycgiserviceid $ companyinfo company
    value "ipaddressmasklist" $ intercalate "," $ fmap show $ companyipaddressmasklist $ companyinfo company
    value "allowsavesafetycopy" $ companyallowsavesafetycopy (companyinfo company)
    value "idledoctimeout" $ companyidledoctimeout $ companyinfo company
    value "smsprovider" $ show . companysmsprovider . companyinfo $ company
    value "padappmode" $ T.unpack $ padAppModeText $ companypadappmode $ companyinfo company
    value "padearchiveenabled" . companypadearchiveenabled $ companyinfo company

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

subscriptionJSON  :: Company -> [User] -> Int -> FeatureFlags -> JSValue
subscriptionJSON company users startedLastMonth ff = runJSONGen $ do
  value "payment_plan" $ paymentPlanText $ companypaymentplan $ companyinfo company
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
  value "can_use_no_authentication_to_view" $ ffCanUseNOAuthenticationToView ff
  value "can_use_no_authentication_to_sign" $ ffCanUseNOAuthenticationToSign ff
  value "can_use_se_authentication_to_view" $ ffCanUseSEAuthenticationToView ff
  value "can_use_se_authentication_to_sign" $ ffCanUseSEAuthenticationToSign ff
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
