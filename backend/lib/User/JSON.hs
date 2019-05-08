{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module User.JSON (
    userJSON,
    usersJSON,
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
import qualified Data.Text as T

import DataRetentionPolicy
import MinutesTime
import PadApplication.Types
import User.CallbackScheme.Model (UserCallbackScheme(..))
import User.Model
import UserGroup.Types
import Util.HasSomeUserInfo

userJSON :: User -> JSValue
userJSON user = runJSONGen $ userJSONUserDetails user

usersJSON :: [User] -> JSValue
usersJSON users = JSArray $ map (runJSONGen . userJSONUserDetails) users

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
      Just _ -> False
    value "callbackurl" $ case callback of
      Nothing -> ("" :: String)
      Just (ConstantUrlSchemeV2 url) -> (url :: String)
      Just (ConstantUrlScheme _) -> "Existing ConstantUrlScheme"
      Just (SalesforceScheme _) -> "Existing SalesforceScheme"
      Just (BasicAuthScheme _ _) -> "Existing BasicAuthScheme"
      Just (OAuth2Scheme _ _ _ _) -> "Existing OAuth2Scheme"

userJSONUserDetails :: User -> JSONGen ()
userJSONUserDetails user = do
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

unjsonUser :: UnjsonDef User
unjsonUser = unjsonUserPartial id

unjsonUserWithPassword :: String -> UnjsonDef User
unjsonUserWithPassword password = unjsonUserPartial
  (<* (fieldReadonly "password" (const password) "User password"))

-- This is a duplicate def since the UserGroup API needs to support parsing Users
-- This JSON structure mimics userJSONUserDetails rather than the actual data structure
unjsonUserPartial
  :: (CAF.Ap (FieldDef User) User -> CAF.Ap (FieldDef User) User)
  -> UnjsonDef User
unjsonUserPartial passwordDef = objectOf $ passwordDef (pure defaultUser
  <*   (fieldReadonly "id" userid "User ID")
  <**> (fieldBy "fstname" getFirstName "User First Name" unjsonDef
    <**> (pure $ \fstname user -> user { userinfo = (userinfo user) { userfstname = fstname } }))
  <**> (fieldBy "sndname" getLastName "User Second Name" unjsonDef
    <**> (pure $ \sndname user -> user { userinfo = (userinfo user) { usersndname = sndname } }))
  <**> (fieldBy "email" (useremail . userinfo) "User Email" unjsonDef
    <**> (pure $ \email user -> user { userinfo = (userinfo user) { useremail = email } }))
  <**> (fieldBy "twofactor_active" usertotpactive "User Twofactor Active" unjsonDef
    <**> (pure $ \totpactive user -> user { usertotpactive = totpactive }))
  <**> (fieldBy "personalnumber" getPersonalNumber "User Personal Number" unjsonDef
    <**> (pure $ \personalnumber user -> user { userinfo = (userinfo user) { userpersonalnumber = personalnumber } }))
  <**> (fieldBy "phone" getMobile "User Phone Number" unjsonDef
    <**> (pure $ \phone user -> user { userinfo = (userinfo user) { userphone = phone } }))
  <**> (fieldBy "companyadmin" useriscompanyadmin "User Company Admin" unjsonDef
    <**> (pure $ \useriscompanyadmin user -> user { useriscompanyadmin = useriscompanyadmin }))
  <**> (fieldBy "companyposition" (usercompanyposition . userinfo) "User Company Position" unjsonDef
    <**> (pure $ \companyposition user -> user { userinfo = (userinfo user) { usercompanyposition = companyposition } }))
  <**> (fieldBy "lang" getLang "User Language" unjsonUserLang
    <**> (pure $ \lang user -> user { usersettings = (usersettings user) { lang = lang } })))
  where
    unjsonUserLang :: UnjsonDef Lang
    unjsonUserLang = unjsonInvmapR
      ((maybe (fail "Can't parse Lang") return) . langFromCode)
      codeFromLang
      unjsonDef

companyJSON :: UserGroupWithParents -> JSValue
companyJSON ugwp = do
  runJSONGen $ do
    value "companyid" . show . get ugID $ ugwpUG ugwp
    value "companyname" . T.unpack $ get ugName $ ugwpUG ugwp
    companyAddressJson $ ugwpAddress ugwp
    companySettingsJson $ ugwpSettings ugwp

companyJSONAdminOnly :: UserGroupWithParents -> JSValue
companyJSONAdminOnly ugwp = do
  let ug = ugwpUG ugwp
      activeAddress = ugwpAddress ugwp
      activeSettings = ugwpSettings ugwp
      mInheritedAddress = ugwpAddress <$> ugwpOnlyParents ugwp
      mInheritedSettings = ugwpSettings <$> ugwpOnlyParents ugwp
      ugParentPath = maybe [] ugwpToList . ugwpOnlyParents $ ugwp
      ugSettingsIsInherited = isNothing . get ugSettings $ ug
      ugAddressIsInherited = isNothing . get ugAddress $ ug
  runJSONGen $ do
    value "companyid" $ show $ get ugID ug
    companyAddressJson activeAddress
    value "companyname" $ T.unpack $ get ugName ug
    companySettingsJson $ activeSettings

    whenJust (mInheritedAddress) $
      object "companyinheritedaddress" . companyAddressJson
    value "companyaddressisinherited" ugAddressIsInherited

    whenJust (mInheritedSettings) $
      object "companyinheritedsettings" . companySettingsJson
    value "companysettingsisinherited" ugSettingsIsInherited

    value "parentid" . fmap show $ get ugParentGroupID ug
    objects "parentgrouppath" . for ugParentPath $ \parent -> do
      value "group_id" . show . get ugID $ parent
      value "group_name" . T.unpack . get ugName $ parent

companyAddressJson :: UserGroupAddress -> JSONGenT Identity ()
companyAddressJson uga = do
  value "address"       . T.unpack $ get ugaAddress       uga
  value "city"          . T.unpack $ get ugaCity          uga
  value "country"       . T.unpack $ get ugaCountry       uga
  value "zip"           . T.unpack $ get ugaZip           uga
  value "companynumber" . T.unpack $ get ugaCompanyNumber uga

companySettingsJson :: UserGroupSettings -> JSONGenT Identity ()
companySettingsJson ugs = do
  let drp = get ugsDataRetentionPolicy ugs
  value "ipaddressmasklist" . intercalate "," . fmap show
    $ get ugsIPAddressMaskList ugs
  value "cgidisplayname" . fmap T.unpack $ get ugsCGIDisplayName ugs
  value "cgiserviceid" . fmap T.unpack $ get ugsCGIServiceID ugs
  value "smsprovider" . show $ get ugsSMSProvider ugs
  value "padappmode" . T.unpack . padAppModeText $ get ugsPadAppMode ugs
  value "padearchiveenabled" $ get ugsPadEarchiveEnabled ugs
  value "idledoctimeoutpreparation" $ get drpIdleDocTimeoutPreparation drp
  value "idledoctimeoutclosed" $ get drpIdleDocTimeoutClosed drp
  value "idledoctimeoutcanceled" $ get drpIdleDocTimeoutCanceled drp
  value "idledoctimeouttimedout" $ get drpIdleDocTimeoutTimedout drp
  value "idledoctimeoutrejected" $ get drpIdleDocTimeoutRejected drp
  value "idledoctimeouterror" $ get drpIdleDocTimeoutError drp
  value "immediatetrash" $ get drpImmediateTrash drp

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

shareableLinkStatsToJSON :: (UTCTime -> String) -> String -> [ShareableLinkUsageStats] -> JSValue
shareableLinkStatsToJSON formatTime textName sluss = runJSONGen . objects "stats" . for slussGrouped $ \slusGroup -> do
  let summary = foldMap slusTemplateStats slusGroup
  value "date" . formatTime . slusTimeWindowStart $ head slusGroup
  value "name" textName
  value "sent" $ tsDocumentsSent summary
  value "closed" $ tsDocumentsClosed summary
  objects "template_stats" . for slusGroup $ \slus -> do
    let TemplateStats{..} = slusTemplateStats slus
    value "date" . formatTime $ slusTimeWindowStart slus
    value "id" . show $ slusTemplateId slus
    value "title" $ slusTemplateTitle slus
    value "sent" tsDocumentsSent
    value "closed" tsDocumentsClosed
  where
    slussGrouped :: [[ShareableLinkUsageStats]]
    slussGrouped = groupBy sameTimeWindow sluss
      where
        sameTimeWindow sl1 sl2 = slusTimeWindowStart sl1 == slusTimeWindowStart sl2

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
