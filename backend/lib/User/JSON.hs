{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module User.JSON (
    userJSONWithCompany,
    userJSONWithCallBackInfo,
    unjsonUser,
    unjsonUserWithPassword,
    companyJSON,
    companyJSONAdminOnly,
    userStatsToJSON,
    companyStatsToJSON,
    shareableLinkStatsToJSON,
    userNotDeletableReasonToString
    ) where

import Data.Functor.Identity
import Data.String (IsString(..))
import Data.Unjson
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplate.GenericStandard ()
import qualified Control.Applicative.Free as CAF (Ap)

import DataRetentionPolicy
import MinutesTime
import PadApplication.Types
import User.CallbackScheme.Model (UserCallbackScheme(..))
import User.Model
import UserGroup.Types
import Util.HasSomeUserInfo

userJSONWithCompany :: User -> UserGroupWithParents -> JSValue
userJSONWithCompany user ugwp = runJSONGen $ do
  userJSONUserDetails user
  value "company" $ companyJSON ugwp

userJSONWithCallBackInfo
  :: User -> UserGroupWithParents -> Maybe UserCallbackScheme -> JSValue
userJSONWithCallBackInfo user ugwp callback = runJSONGen $ do
  userJSONUserDetails user
  value "company" $ companyJSON ugwp
  value "callback_is_editable" $ case callback of
    Nothing -> True
    Just (ConstantUrlSchemeV2 _) -> True
    Just _  -> False
  value "callbackurl" $ case callback of
    Nothing -> ""
    Just (ConstantUrlSchemeV2 url) -> url
    Just (ConstantUrlScheme _) -> "Existing ConstantUrlScheme"
    Just (SalesforceScheme _) -> "Existing SalesforceScheme"
    Just (BasicAuthScheme _ _) -> "Existing BasicAuthScheme"
    Just (OAuth2Scheme _ _ _ _) -> "Existing OAuth2Scheme"
    Just (Hi3GScheme _ _ _ _) -> "Existing Hi3GScheme"

userJSONUserDetails :: User -> JSONGen ()
userJSONUserDetails user = do
  value "id" $ showt $ userid user
  value "fstname" $ getFirstName user
  value "sndname" $ getLastName user
  value "email" $ getEmail user
  value "home_folder_id" $ userhomefolderid user
  value "twofactor_active" $ usertotpactive user
  value "twofactor_is_mandatory" $ usertotpismandatory user
  value "personalnumber" $ getPersonalNumber user
  value "phone" $ userphone $ userinfo user
  value "companyadmin" $ useriscompanyadmin user
  value "companyposition" $ usercompanyposition $ userinfo user
  value "lang" $ codeFromLang $ getLang user

unjsonUser :: UnjsonDef User
unjsonUser = unjsonUserPartial identity

unjsonUserWithPassword :: Text -> UnjsonDef User
unjsonUserWithPassword password =
  unjsonUserPartial (<* (fieldReadonly "password" (const password) "User password"))

-- This is a duplicate def since the UserGroup API needs to support parsing Users
-- This JSON structure mimics userJSONUserDetails rather than the actual data structure
unjsonUserPartial
  :: (CAF.Ap (FieldDef User) User -> CAF.Ap (FieldDef User) User) -> UnjsonDef User
unjsonUserPartial passwordDef = objectOf $ passwordDef
  (    pure defaultUser
  <*   (fieldReadonly "id" userid "User ID")
  <**> (    fieldBy "fstname" getFirstName "User First Name" unjsonDef
       <**> (pure $ \fstname user ->
              user { userinfo = (userinfo user) { userfstname = fstname } }
            )
       )
  <**> (    fieldBy "sndname" getLastName "User Second Name" unjsonDef
       <**> (pure $ \sndname user ->
              user { userinfo = (userinfo user) { usersndname = sndname } }
            )
       )
  <**> (    fieldBy "email" (useremail . userinfo) "User Email" unjsonDef
       <**> (pure $ \email user -> user
              { userinfo = (userinfo user) { useremail = email }
              }
            )
       )
  <**> (    fieldBy "twofactor_active" usertotpactive "User Twofactor Active" unjsonDef
       <**> (pure $ \totpactive user -> user { usertotpactive = totpactive })
       )
  <**> (    fieldBy "twofactor_is_mandatory"
                    usertotpismandatory
                    "User Twofactor Is Mandatory"
                    unjsonDef
       <**> (pure $ \totpismandatory user -> user
              { usertotpismandatory = totpismandatory
              }
            )
       )
  <**> (    fieldBy "personalnumber" getPersonalNumber "User Personal Number" unjsonDef
       <**> (pure $ \personalnumber user -> user
              { userinfo = (userinfo user) { userpersonalnumber = personalnumber }
              }
            )
       )
  <**> (    fieldBy "phone" getMobile "User Phone Number" unjsonDef
       <**> (pure $ \phone user -> user
              { userinfo = (userinfo user) { userphone = phone }
              }
            )
       )
  <**> (    fieldBy "companyadmin" useriscompanyadmin "User Company Admin" unjsonDef
       <**> (pure $ \useriscompanyadmin user -> user
              { useriscompanyadmin = useriscompanyadmin
              }
            )
       )
  <**> (    fieldBy "companyposition"
                    (usercompanyposition . userinfo)
                    "User Company Position"
                    unjsonDef
       <**> (pure $ \companyposition user -> user
              { userinfo = (userinfo user) { usercompanyposition = companyposition }
              }
            )
       )
  <**> (    fieldBy "lang" getLang "User Language" unjsonUserLang
       <**> (pure $ \lang user -> user
              { usersettings = (usersettings user) { lang = lang }
              }
            )
       )
  )
  where
    unjsonUserLang :: UnjsonDef Lang
    unjsonUserLang = unjsonInvmapR
      ((maybe (fail "Can't parse Lang") return) . langFromCode)
      codeFromLang
      unjsonDef

companyJSON :: UserGroupWithParents -> JSValue
companyJSON ugwp = do
  runJSONGen $ do
    let ug = ugwpUG ugwp
    value "companyid" . show $ ug ^. #id
    value "companyname" $ ug ^. #name
    value "companyhomefolderid" $ ug ^. #homeFolderID
    companyAddressJson $ ugwpAddress ugwp
    companySettingsJson $ ugwpSettings ugwp

companyJSONAdminOnly :: UserGroupWithParents -> JSValue
companyJSONAdminOnly ugwp = do
  let ug                    = ugwpUG ugwp
      activeAddress         = ugwpAddress ugwp
      activeSettings        = ugwpSettings ugwp
      mInheritedAddress     = ugwpAddress <$> ugwpOnlyParents ugwp
      mInheritedSettings    = ugwpSettings <$> ugwpOnlyParents ugwp
      ugParentPath          = maybe [] ugwpToList . ugwpOnlyParents $ ugwp
      ugSettingsIsInherited = isNothing $ ug ^. #settings
      ugAddressIsInherited  = isNothing $ ug ^. #address
  runJSONGen $ do
    value "companyid" $ show $ ug ^. #id
    companyAddressJson activeAddress
    value "companyname" $ ug ^. #name
    companySettingsJson $ activeSettings

    whenJust (mInheritedAddress) $ object "companyinheritedaddress" . companyAddressJson
    value "companyaddressisinherited" ugAddressIsInherited

    whenJust (mInheritedSettings)
      $ object "companyinheritedsettings"
      . companySettingsJson
    value "companysettingsisinherited" ugSettingsIsInherited

    value "parentid" . fmap show $ ug ^. #parentGroupID
    objects "parentgrouppath" . for ugParentPath $ \parent -> do
      value "group_id" . show $ parent ^. #id
      value "group_name" $ parent ^. #name

companyAddressJson :: UserGroupAddress -> JSONGenT Identity ()
companyAddressJson uga = do
  value "address" $ uga ^. #ugaAddress
  value "city" $ uga ^. #ugaCity
  value "country" $ uga ^. #ugaCountry
  value "zip" $ uga ^. #ugaZip
  value "companynumber" $ uga ^. #ugaCompanyNumber
  value "entityname" $ uga ^. #ugaEntityName

companySettingsJson :: UserGroupSettings -> JSONGenT Identity ()
companySettingsJson ugs = do
  let drp = ugs ^. #ugsDataRetentionPolicy
  value "ipaddressmasklist" . intercalate "," . fmap show $ ugs ^. #ugsIPAddressMaskList
  value "cgidisplayname" $ ugs ^. #ugsCGIDisplayName
  value "cgiserviceid" $ ugs ^. #ugsCGIServiceID
  value "smsprovider" . show $ ugs ^. #ugsSMSProvider
  value "padappmode" . padAppModeText $ ugs ^. #ugsPadAppMode
  value "padearchiveenabled" $ ugs ^. #ugsPadEarchiveEnabled
  value "idledoctimeoutpreparation" $ drpIdleDocTimeoutPreparation drp
  value "idledoctimeoutclosed" $ drpIdleDocTimeoutClosed drp
  value "idledoctimeoutcanceled" $ drpIdleDocTimeoutCanceled drp
  value "idledoctimeouttimedout" $ drpIdleDocTimeoutTimedout drp
  value "idledoctimeoutrejected" $ drpIdleDocTimeoutRejected drp
  value "idledoctimeouterror" $ drpIdleDocTimeoutError drp
  value "immediatetrash" $ drpImmediateTrash drp
  value "sendtimeoutnotification" $ ugs ^. #ugsSendTimeoutNotification
  value "totpismandatory" $ ugs ^. #ugsTotpIsMandatory
  value "sessiontimeout" $ ugs ^. #ugsSessionTimeoutSecs
  value "portalurl" $ ugs ^. #ugsPortalUrl

userStatsToJSON :: (UTCTime -> Text) -> [UserUsageStats] -> JSValue
userStatsToJSON formatTime uuss = runJSONGen . objects "stats" . for uuss $ \uus -> do
  value "date" . formatTime $ uusTimeWindowStart uus
  documentStatsToJSON $ uusDocumentStats uus

documentStatsToJSON :: Monad m => DocumentStats -> JSONGenT m ()
documentStatsToJSON DocumentStats {..} = do
  value "sent"                  dsDocumentsSent
  value "closed"                dsDocumentsClosed
  value "signatures"            dsSignaturesClosed
  value "sms_sent"              dsSMSSent
  value "sms_sent_via_telia"    dsSMSSentViaTelia
  value "se_bankid_signatures"  dsSEBankIDSignatures
  value "se_bankid_authentications" dsSEBankIDAuthentications
  value "no_bankid_signatures"  dsNOBankIDSignatures
  value "no_bankid_authentications" dsNOBankIDAuthentications
  value "nemid_signatures"      dsNemIDSignatures
  value "nemid_authentications" dsNemIDAuthentications
  value "tupas_authentications" dsTupasAuthentications
  value "shareable_links"       dsShareableLinks

companyStatsToJSON :: (UTCTime -> Text) -> Text -> [UserUsageStats] -> JSValue
companyStatsToJSON formatTime textName uuss =
  runJSONGen . objects "stats" . for uussGrouped $ \uusGroup -> do
    let summary = foldMap uusDocumentStats uusGroup
    value "date" . formatTime . uusTimeWindowStart $ head uusGroup
    value "name" textName
    documentStatsToJSON summary

    objects "user_stats" . for uusGroup $ \uus -> do
      value "date" . formatTime $ uusTimeWindowStart uus
      value "email" $ uusUserEmail uus
      value "name" $ uusUserName uus
      documentStatsToJSON $ uusDocumentStats uus
  where
    uussGrouped :: [[UserUsageStats]]
    uussGrouped = groupBy sameTimeWindow uuss
      where sameTimeWindow u1 u2 = uusTimeWindowStart u1 == uusTimeWindowStart u2

shareableLinkStatsToJSON
  :: (UTCTime -> Text) -> Text -> [ShareableLinkUsageStats] -> JSValue
shareableLinkStatsToJSON formatTime textName sluss =
  runJSONGen . objects "stats" . for slussGrouped $ \slusGroup -> do
    let summary = foldMap slusDocumentStats slusGroup
    value "date" . formatTime . slusTimeWindowStart $ head slusGroup
    value "name" textName
    documentStatsToJSON summary
    objects "template_stats" . for slusGroup $ \slus -> do
      value "date" . formatTime $ slusTimeWindowStart slus
      value "id" . showt $ slusTemplateId slus
      value "title" $ slusTemplateTitle slus
      documentStatsToJSON $ slusDocumentStats slus
  where
    slussGrouped :: [[ShareableLinkUsageStats]]
    slussGrouped = groupBy sameTimeWindow sluss
      where sameTimeWindow sl1 sl2 = slusTimeWindowStart sl1 == slusTimeWindowStart sl2

instance ToJSValue UserNotDeletableReason where
  toJSValue reason =
    let code = case reason of
          UserNotDeletableDueToPendingDocuments -> "pending_documents"
        msg = userNotDeletableReasonToString reason
    in  JSObject $ toJSObject
          [("code", JSString $ toJSString code), ("message", JSString $ toJSString msg)]

userNotDeletableReasonToString :: IsString s => UserNotDeletableReason -> s
userNotDeletableReasonToString = fromString . \case
  UserNotDeletableDueToPendingDocuments -> "Can't delete a user with pending documents."
