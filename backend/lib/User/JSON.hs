{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module User.JSON (
    userJSON,
    userJSONWithCallBackInfo,
    companyJSON,
    userStatsToJSON,
    companyStatsToJSON,
    userNotDeletableReasonToString
    ) where

import Data.String (IsString(..))
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplate.GenericStandard ()
import qualified Data.Text as T

import DataRetentionPolicy
import MinutesTime
import PadApplication.Data
import User.CallbackScheme.Model (UserCallbackScheme(..))
import User.Model
import UserGroup.Data
import Util.HasSomeUserInfo

userJSON :: User -> UserGroup -> JSValue
userJSON user company = runJSONGen $ userJSONUserDetails user company

userJSONWithCallBackInfo :: User -> UserGroup -> Maybe UserCallbackScheme -> JSValue
userJSONWithCallBackInfo user company callback = runJSONGen $ do
    userJSONUserDetails user company
    value "callback_is_editable" $ case callback of
      Nothing -> True
      Just (ConstantUrlSchemeV2 _) -> True
      Just _ -> False
    value "callbackurl" $ case callback of
      Nothing -> ("" :: String)
      Just (ConstantUrlSchemeV2 url) -> (url :: String)
      Just (ConstantUrlScheme _) -> "Existing ConstantUrlScheme"
      Just (SalesforceScheme _) -> "Existing SalesforceScheme"
      Just (BasicAuthScheme _ _) -> "Existing BasicAuthScheme"
      Just (OAuth2Scheme _ _ _ _) -> "Existing OAuth2Scheme"

userJSONUserDetails :: User -> UserGroup -> JSONGen ()
userJSONUserDetails user company = do
    value "id" $ show $ userid user
    value "fstname" $ getFirstName user
    value "sndname" $ getLastName user
    value "email" $ getEmail user
    value "twofactor_active" $ usertotpactive user
    value "personalnumber" $ getPersonalNumber user
    value "phone" $ userphone $ userinfo user
    value "companyadmin" $ useriscompanyadmin user
    value "companyposition" $ usercompanyposition $ userinfo user
    value "lang" $ codeFromLang $ getLang user
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
