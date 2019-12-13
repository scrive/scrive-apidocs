-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Handlers for all administrations tasks
--
-----------------------------------------------------------------------------
module Administration.AdministrationControl (
            adminonlyRoutes
          , daveRoutes
          , jsonCompanies -- for tests
          , handleCompanyChange -- for tests
          , handleTriggerMigrateFolders -- for tests
          , handleTriggerMigrateDocuments -- for tests
          ) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Invariant
import Data.Int (Int16, Int32)
import Data.Time (diffUTCTime)
import Data.Unjson
import Happstack.Server hiding (dir, https, path, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir)
import Log
import Text.JSON
import Text.JSON.Gen hiding (object)
import Text.StringTemplates.Templates
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.ICU.Normalize as ICU
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Administration.AdministrationView
import API.V2.Errors
import API.V2.Parameters
import AppView (renderFromBody, simpleHtmlResponse)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import DataRetentionPolicy.Guards
import DB
import Doc.Action (postDocumentClosedActions, postDocumentPendingChange)
import Doc.API.V2.DocumentAccess
import Doc.API.V2.JSON.Document
import Doc.API.V2.JSON.List
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (withDocumentID)
import Doc.Model
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots (SignatoryScreenshots(..))
import EvidenceLog.Model
import File.File
import File.Model
import File.Storage
import Folder.Model
import Happstack.Fields
import InputValidation
import InspectXML
import InspectXMLInstances
import IPAddress ()
import Kontra
import KontraLink
import Log.Identifier
import Mails.Model
import MinutesTime
import PadApplication.Types (padAppModeFromText)
import Routing
import Session.Constant
import Session.Model
import Templates (renderTextTemplate)
import Theme.Control
import User.CallbackScheme.Model
import User.Email
import User.History.Model
import User.JSON
import User.UserControl
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import UserGroup.Types.Subscription
import UserGroupAccounts.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Monoid
import qualified API.V2 as V2
import qualified BrandedDomain.BrandedDomain.Internal as I
import qualified Company.CompanyControl as Company
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified UserGroup.Internal as I
import qualified UserGroupAccounts.UserGroupAccountsControl as UserGroupAccounts

adminonlyRoutes :: Route (Kontra Response)
adminonlyRoutes =
  fmap onlySalesOrAdmin
    $ choice
    $ [ hGet $ toK0 $ showAdminMainPage
      , dir "createuser" $ hPost $ toK0 $ handleCreateUser
      , dir "userslist" $ hGet $ toK0 $ jsonUsersList
      , dir "useradmin" $ hGet $ toK1 $ showAdminUsers
      , dir "useradmin" $ dir "details" $ hGet $ toK1 $ handleUserGetProfile
      , dir "useradmin" $ hPost $ toK1 $ handleUserChange
      , dir "useradmin" $ dir "changepassword" $ hPost $ toK1 $ handleUserPasswordChange
      , dir "useradmin" $ dir "deleteinvite" $ hPost $ toK2 $ handleDeleteInvite
      , dir "useradmin" $ dir "delete" $ hPost $ toK1 $ handleDeleteUser
      , dir "useradmin" $ dir "move" $ hPost $ toK1 $ handleMoveUserToDifferentCompany
      , dir "useradmin" $ dir "disable2fa" $ hPost $ toK1 $ handleDisable2FAForUser
      , dir "useradmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1
        handleAdminUserUsageStatsDays
      , dir "useradmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1
        handleAdminUserUsageStatsMonths
      , (dir "useradmin" . dir "shareablelinkstats" . dir "days" . hGet . toK1)
        $ handleAdminUserShareableLinksStats PartitionByDay
      , (dir "useradmin" . dir "shareablelinkstats" . dir "months" . hGet . toK1)
        $ handleAdminUserShareableLinksStats PartitionByMonth
      , dir "useradmin" $ dir "sendinviteagain" $ hPost $ toK0 $ sendInviteAgain
      , dir "companyadmin" $ hGet $ toK1 $ showAdminCompany
      , dir "companyadmin" $ dir "details" $ hGet $ toK1 $ handleCompanyGetProfile
      , dir "companyadmin" $ hPost $ toK1 $ handleCompanyChange
      , dir "companyadmin" $ dir "merge" $ hPost $ toK1 $ handleMergeToOtherCompany
      , dir "companyadmin" $ dir "branding" $ Company.adminRoutes
      , dir "companyadmin" $ dir "users" $ hPost $ toK1 $ handlePostAdminCompanyUsers
      , (dir "companyaccounts" . hGet . toK1)
        UserGroupAccounts.handleUserGroupAccountsForAdminOnly
      , dir "companyadmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1
        handleAdminCompanyUsageStatsDays
      , dir "companyadmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1
        handleAdminCompanyUsageStatsMonths
      , (dir "companyadmin" . dir "shareablelinkstats" . dir "days" . hGet . toK1)
        $ handleAdminCompanyShareableLinksStats PartitionByDay
      , (dir "companyadmin" . dir "shareablelinkstats" . dir "months" . hGet . toK1)
        $ handleAdminCompanyShareableLinksStats PartitionByMonth
      , (dir "companyadmin" . dir "getsubscription" . hGet . toK1)
        handleCompanyGetSubscription
      , (dir "companyadmin" . dir "updatesubscription" . hPost . toK1)
        handleCompanyUpdateSubscription
      , dir "companyadmin" $ dir "getstructure" $ hGet $ toK1 $ handleCompanyGetStructure
      , dir "documentslist" $ hGet $ toK0 $ jsonDocuments
      , dir "companies" $ hGet $ toK0 $ jsonCompanies
      , dir "brandeddomainslist" $ hGet $ toK0 $ jsonBrandedDomainsList
      , dir "brandeddomain" $ dir "create" $ hPost $ toK0 $ createBrandedDomain
      , dir "brandeddomain" $ dir "details" $ hGet $ toK1 $ jsonBrandedDomain
      , (dir "brandeddomain" . dir "details" . dir "change" . hPost . toK1)
        updateBrandedDomain
      , dir "brandeddomain" $ dir "themes" $ hGet $ toK1 $ handleGetThemesForDomain
      , dir "brandeddomain" $ dir "newtheme" $ hPost $ toK2 $ handleNewThemeForDomain
      , (dir "brandeddomain" . dir "updatetheme" . hPost . toK2)
        handleUpdateThemeForDomain
      , (dir "brandeddomain" . dir "deletetheme" . hPost . toK2)
        handleDeleteThemeForDomain

        -- migration trigging endpoints
      , dir "triggermigratefolders" $ hGet $ toK1 handleTriggerMigrateFolders
      , dir "triggermigratedocuments" $ hGet $ toK1 handleTriggerMigrateDocuments
      ]

daveRoutes :: Route (Kontra Response)
daveRoutes =
  fmap onlyAdmin
    $ choice
    $ [ dir "document" $ hGet $ toK1 $ daveDocument
      , dir "document" $ hGet $ toK2 $ daveSignatoryLink
      , dir "user" $ hGet $ toK1 $ daveUser
      , dir "userhistory" $ hGet $ toK1 $ daveUserHistory
      , dir "usergroup" $ hGet $ toK1 $ daveUserGroup
      , dir "reseal" $ hPost $ toK1 $ resealFile
      , dir "postpending" $ hPost $ toK1 $ triggerPostPending
      , dir "file" $ hGet $ toK2 $ daveFile
      , dir "backdoor" $ hGet $ handleBackdoorQuery
      , dir "randomscreenshot" $ hGet $ toK0 $ randomScreenshotForTest
      ]
{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m String
showAdminMainPage = onlySalesOrAdmin $ do
  ctx <- getContext
  adminMainPage ctx

{- | Process view for finding a user in basic administration -}
showAdminUsers :: Kontrakcja m => UserID -> m String
showAdminUsers uid = onlySalesOrAdmin $ do
  ctx <- getContext
  adminUserPage ctx uid

handleUserGetProfile :: Kontrakcja m => UserID -> m JSValue
handleUserGetProfile uid = onlySalesOrAdmin $ do
  user     <- guardJustM $ dbQuery $ GetUserByID uid
  callback <- dbQuery $ GetUserCallbackSchemeByUserID uid
  ugwp     <- dbQuery . UserGroupGetWithParentsByUserID $ uid
  return $ userJSONWithCallBackInfo user ugwp callback

handleCompanyGetProfile :: Kontrakcja m => UserGroupID -> m JSValue
handleCompanyGetProfile ugid =
  onlySalesOrAdmin
    $   companyJSONAdminOnly
    <$> (guardJustM . dbQuery . UserGroupGetWithParents $ ugid)

showAdminCompany :: Kontrakcja m => UserGroupID -> m Text
showAdminCompany ugid = onlySalesOrAdmin $ do
  ctx <- getContext
  T.pack <$> adminCompanyPage ctx ugid

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
  limit      <- guardJustM $ readField "limit"
  offset     <- guardJustM $ readField "offset"
  textFilter <- getField "text" >>= \case
    Nothing -> return []
    Just s  -> return [UGFilterByString s]
  usersFilter <- isFieldSet "allCompanies" >>= \case
    True  -> return []
    False -> return [UGManyUsers]
  pplanFilter <- isFieldSet "nonFree" >>= \case
    True  -> return [UGWithNonFreePricePlan]
    False -> return []
  ugs <- dbQuery $ UserGroupsGetFiltered (textFilter <> usersFilter <> pplanFilter)
                                         (Just (offset, limit))
  -- get address for those companies, which inherit it
  ugsWithAddress <- forM ugs $ \ug -> case ug ^. #address of
    Just uga -> return (ug, uga)
    Nothing ->
      ((ug, ) . ugwpAddress)
        <$> (guardJustM . dbQuery . UserGroupGetWithParents $ ug ^. #id)
  runJSONGenT $ do
    valueM "companies" $ forM ugsWithAddress $ \(ug, uga) -> runJSONGenT $ do
      value "id" . show $ ug ^. #id
      value "companyname" . T.unpack $ ug ^. #name
      value "companynumber" . T.unpack $ uga ^. #companyNumber
      value "companyentityname" . T.unpack $ uga ^. #entityName
      value "companyaddress" . T.unpack $ uga ^. #address
      value "companyzip" . T.unpack $ uga ^. #zipCode
      value "companycity" . T.unpack $ uga ^. #city
      value "companycountry" . T.unpack $ uga ^. #country

jsonUsersList :: Kontrakcja m => m JSValue
jsonUsersList = onlySalesOrAdmin $ do
  limit      <- guardJustM $ readField "limit"
  offset     <- guardJustM $ readField "offset"
  textFilter <- getField "text" >>= \case
    Nothing -> return []
    Just s  -> return [UserFilterByString $ T.unpack s]
  sorting <- getField "tosSorting" >>= \case
    Just "ascending"  -> return [Asc UserOrderByAccountCreationDate]
    Just "descending" -> return [Desc UserOrderByAccountCreationDate]
    _                 -> return [Asc UserOrderByName]
  allUsers <- dbQuery $ GetUsersWithUserGroupNames textFilter sorting (offset, limit)

  runJSONGenT $ do
    valueM "users" $ forM (allUsers) $ \(user, ugname) -> runJSONGenT $ do
      value "id" $ show $ user ^. #id
      value "username" $ T.unpack $ getFullName user
      value "email" $ T.unpack $ getEmail user
      value "companyposition" $ T.unpack $ user ^. #info % #companyPosition
      value "company" . T.unpack $ ugname
      value "phone" $ T.unpack $ user ^. #info % #phone
      value "tos" $ formatTimeISO <$> (user ^. #hasAcceptedTOS)
      value "twofactor_active" $ user ^. #totpActive


{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m JSValue
handleUserChange uid = onlySalesOrAdmin $ do
  ctx         <- getContext
  callback    <- dbQuery $ GetUserCallbackSchemeByUserID uid
  maybeNewUrl <- getField "usercallbackurl"
  let newUrl = T.strip $ fromJust maybeNewUrl
  case callback of
    Nothing -> if T.null newUrl
      then return () -- Don't add a callback with an empty URL
      else dbUpdate $ UpdateUserCallbackScheme uid (ConstantUrlSchemeV2 newUrl)
    Just (ConstantUrlSchemeV2 url) -> do
      case (T.null newUrl, newUrl == url) of
        (True , _   ) -> dbUpdate $ DeleteUserCallbackScheme uid -- Delete callback if textbox emptied
        (False, True) -> return () -- Don't update if no change
        (False, False) ->
          dbUpdate $ UpdateUserCallbackScheme uid (ConstantUrlSchemeV2 $ newUrl)
    Just _ -> return () -- Do not allow changing the callback if an existing other type is there

  -- Set whether 2FA is mandatory
  maybeNewTotpIsMandatory <- getField "usertotpismandatory"
  void . dbUpdate . SetUserTotpIsMandatory uid $ Just "true" == maybeNewTotpIsMandatory

  museraccounttype <- getField "useraccounttype"
  olduser          <- guardJustM $ dbQuery $ GetUserByID uid
  user             <- case (museraccounttype, olduser ^. #isCompanyAdmin) of
    (Just "companyadminaccount", False) -> do
      --then we just want to make this account an admin
      newuser <- guardJustM $ do
        void $ dbUpdate $ SetUserCompanyAdmin uid True
        void $ dbUpdate $ LogHistoryDetailsChanged
          uid
          (ctx ^. #ipAddr)
          (ctx ^. #time)
          [("is_company_admin", "false", "true")]
          (ctx ^? #maybeUser % _Just % #id)
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companystandardaccount", True) -> do
      --then we just want to downgrade this account to a standard
      newuser <- guardJustM $ do
        void $ dbUpdate $ SetUserCompanyAdmin uid False
        void $ dbUpdate $ LogHistoryDetailsChanged
          uid
          (ctx ^. #ipAddr)
          (ctx ^. #time)
          [("is_company_admin", "true", "false")]
          (ctx ^? #maybeUser % _Just % #id)
        dbQuery $ GetUserByID uid
      return newuser
    _ -> return olduser
  infoChange <- getUserInfoChange
  let applyChanges = do
        void $ dbUpdate $ SetUserInfo uid $ infoChange $ user ^. #info
        void $ dbUpdate $ LogHistoryUserInfoChanged uid
                                                    (ctx ^. #ipAddr)
                                                    (ctx ^. #time)
                                                    (user ^. #info)
                                                    (infoChange $ user ^. #info)
                                                    (ctx ^? #maybeUser % _Just % #id)
        settingsChange <- getUserSettingsChange
        void $ dbUpdate $ SetUserSettings uid $ settingsChange $ user ^. #settings
        return ()
  if infoChange (user ^. #info) ^. #email /= user ^. #info % #email
    then do
      -- email address changed, check if new one is not used
      mexistinguser <- dbQuery $ GetUserByEmail (infoChange (user ^. #info) ^. #email)
      case mexistinguser of
        Just _  -> runJSONGenT $ value "changed" False
        Nothing -> do
          applyChanges
          runJSONGenT $ value "changed" True
    else do
      applyChanges
      runJSONGenT $ value "changed" True


{- | Handling user password change. -}
handleUserPasswordChange :: Kontrakcja m => UserID -> m JSValue
handleUserPasswordChange uid = onlySalesOrAdmin $ do
  user         <- guardJustM $ dbQuery $ GetUserByID uid
  password     <- guardJustM $ getField "password"
  passwordhash <- createPassword password
  ctx          <- getContext
  let time     = ctx ^. #time
      ipnumber = ctx ^. #ipAddr
      admin    = ctx ^. #maybeUser
  void $ dbUpdate $ SetUserPassword (user ^. #id) passwordhash
  void $ dbUpdate $ LogHistoryPasswordSetup (user ^. #id)
                                            ipnumber
                                            time
                                            (view #id <$> admin)
  terminateAllUserSessionsExceptCurrent (user ^. #id)
  runJSONGenT $ value "changed" True

handleDeleteInvite :: Kontrakcja m => UserGroupID -> UserID -> m ()
handleDeleteInvite ugid uid = onlySalesOrAdmin $ do
  void $ dbUpdate $ RemoveUserGroupInvite [ugid] uid
  return ()

handleDeleteUser :: Kontrakcja m => UserID -> m ()
handleDeleteUser uid = onlySalesOrAdmin $ do
  void $ dbUpdate $ RemoveUserUserGroupInvites uid
  void $ dbUpdate $ DeleteUserCallbackScheme uid
  void $ dbUpdate $ DeleteUser uid
  return ()

handleDisable2FAForUser :: Kontrakcja m => UserID -> m ()
handleDisable2FAForUser uid = onlySalesOrAdmin $ do
  ctx  <- getContext
  user <- guardJustM $ dbQuery $ GetUserByID uid
  if user ^. #totpActive
    then do
      r <- dbUpdate $ DisableUserTOTP uid
      if r
        then do
          void $ dbUpdate $ LogHistoryTOTPDisable uid (ctx ^. #ipAddr) (ctx ^. #time)
          return ()
        else internalError
    else return ()

handleMoveUserToDifferentCompany :: Kontrakcja m => UserID -> m ()
handleMoveUserToDifferentCompany uid = onlySalesOrAdmin $ do
  newugid <- guardJustM $ readField "companyid"
  (view #id <$>) <$> (dbQuery $ FolderGetUserGroupHome newugid) >>= \case
    Nothing         -> internalError
    Just newugfdrid -> do
      void $ dbUpdate $ SetUserUserGroup uid newugid
      void $ dbUpdate $ SetUserCompanyAdmin uid False
      let newhomefdr = set #parentID (Just newugfdrid) defaultFolder
      newhomefdrid <- view #id <$> (dbUpdate $ FolderCreate newhomefdr)
      void $ dbUpdate . SetUserHomeFolder uid $ newhomefdrid

handleMergeToOtherCompany :: Kontrakcja m => UserGroupID -> m ()
handleMergeToOtherCompany ugid_source = onlySalesOrAdmin $ do
  hasChildren <- not . null <$> (dbQuery . UserGroupGetImmediateChildren $ ugid_source)
  if hasChildren
    then do
      V2.apiError
        .   conflictError
        $   "The user group has children;"
        <+> "merge aborted. Remove or merge these"
        <+> "and retry."
    else do
      ugid_target <- guardJustM $ readField "companyid"
      (view #id <$>) <$> (dbQuery $ FolderGetUserGroupHome ugid_target) >>= \case
        Nothing          -> internalError
        Just targetfdrid -> do
          users <- dbQuery $ UserGroupGetUsers ugid_source
          forM_ users $ \u -> do
            void $ dbUpdate $ SetUserUserGroup (u ^. #id) ugid_target
            let newhomefdr = set #parentID (Just targetfdrid) defaultFolder
            newhomefdrid <- view #id <$> (dbUpdate $ FolderCreate newhomefdr)
            void $ dbUpdate . SetUserHomeFolder (u ^. #id) $ newhomefdrid
          invites <- dbQuery $ UserGroupGetInvites ugid_source
          forM_ invites $ \i ->
            void . dbUpdate $ RemoveUserGroupInvite [ugid_source] (inviteduserid i)

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => UserGroupID -> m ()
handleCompanyChange ugid = onlySalesOrAdmin $ do
  ugwp                   <- guardJustM $ dbQuery $ UserGroupGetWithParents ugid
  mCompanyName           <- getField "companyname"
  mUGSettingsIsInherited <- fmap (== ("true" :: Text))
    <$> getField "companysettingsisinherited"
  ugSettingsChange      <- getUserGroupSettingsChange
  mUGAddressIsInherited <- fmap (== ("true" :: Text))
    <$> getField "companyaddressisinherited"
  ugAddressChange       <- getUserGroupAddressChange
  mTryParentUserGroupID <- getOptionalField asValidUserGroupID "companyparentid"

  let oldUG       = ugwpUG ugwp
      setSettings = if fromMaybe (isNothing $ oldUG ^. #settings) mUGSettingsIsInherited
        then set #settings Nothing
        else set #settings . Just . ugSettingsChange $ ugwpSettings ugwp
      setAddress = if fromMaybe (isNothing $ oldUG ^. #address) mUGAddressIsInherited
        then set #address Nothing
        else set #address . Just . ugAddressChange $ ugwpAddress ugwp
      newUG =
        set #parentGroupID mTryParentUserGroupID
          . maybe identity (set #name) mCompanyName
          . setSettings
          . setAddress
          $ ugwpUG ugwp

  newSettings <-
    guardJust
    . listToMaybe
    . catMaybes
    $ [newUG ^. #settings, ugwpSettings <$> ugwpOnlyParents ugwp]
  guardThatDataRetentionPolicyIsValid (newSettings ^. #dataRetentionPolicy) Nothing
  dbUpdate $ UserGroupUpdate newUG
  return $ ()

handleCreateUser :: Kontrakcja m => m JSValue
handleCreateUser = onlySalesOrAdmin $ do
  email    <- T.filter (/= ' ') <$> T.toLower <$> (guardJustM $ getField "email")
  fstname  <- guardJustM $ getField "fstname"
  sndname  <- guardJustM $ getField "sndname"
  lang     <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
  ugFolder <- dbUpdate . FolderCreate $ defaultFolder
  ug       <-
    dbUpdate
    . UserGroupCreate
    . set #homeFolderID (Just $ ugFolder ^. #id)
    $ defaultUserGroup
  muser <- createNewUserByAdmin email (fstname, sndname) (ug ^. #id, True) lang
  runJSONGenT $ case muser of
    Nothing -> do
      value "success" False
      valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
    Just _ -> do
      value "success"       True
      value "error_message" (Nothing :: Maybe String)

handlePostAdminCompanyUsers :: Kontrakcja m => UserGroupID -> m JSValue
handlePostAdminCompanyUsers ugid = onlySalesOrAdmin $ do
  email   <- getCriticalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  lang    <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
  admin   <- isFieldSet "iscompanyadmin"
  muser   <- createNewUserByAdmin email (fstname, sndname) (ugid, admin) lang
  runJSONGenT $ case muser of
    Nothing -> do
      value "success" False
      valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
    Just _ -> do
      value "success"       True
      value "error_message" (Nothing :: Maybe String)

{- | Reads params and returns function for conversion of user group info.  With no param leaves fields unchanged -}
getUserGroupSettingsChange
  :: forall  m . Kontrakcja m => m (UserGroupSettings -> UserGroupSettings)
getUserGroupSettingsChange = do
  mcompanyipaddressmasklist <- getOptionalField asValidIPAddressWithMaskList
                                                "companyipaddressmasklist"
  mcompanycgidisplayname <- fmap emptyToNothing <$> getField "companycgidisplayname"
  mcompanycgiserviceid <- fmap emptyToNothing <$> getField "companycgiserviceid"
  mcompanyidledoctimeoutpreparation <- getIdleDocTimeoutField
    "companyidledoctimeoutpreparation"
  mcompanyidledoctimeoutclosed <- getIdleDocTimeoutField "companyidledoctimeoutclosed"
  mcompanyidledoctimeoutcanceled <- getIdleDocTimeoutField "companyidledoctimeoutcanceled"
  mcompanyidledoctimeouttimedout <- getIdleDocTimeoutField "companyidledoctimeouttimedout"
  mcompanyidledoctimeoutrejected <- getIdleDocTimeoutField "companyidledoctimeoutrejected"
  mcompanyidledoctimeouterror <- getIdleDocTimeoutField "companyidledoctimeouterror"
  mcompanyimmediatetrash <- getField "companyimmediatetrash"
  mcompanysmsprovider <- fmap maybeRead <$> getField' $ "companysmsprovider"
  mcompanypadappmode <- fmap padAppModeFromText <$> getField' $ "companypadappmode"
  mcompanypadearchiveenabled <- getField "companypadearchiveenabled"
  mcompanysendtimeoutnotification <- getField "companysendtimeoutnotification"
  mcompanytotpismandatory <- getField "companytotpismandatory"
  mcompanysessiontimeout <- getSessionTimeoutField "companysessiontimeout"
  mcompanyportalurl <- fmap emptyToNothing <$> getField "companyportalurl"
  mcompanyeidservicetoken <- fmap emptyToNothing <$> getField "companyeidservicetoken"

  return
    $ maybe identity (set #ipAddressMaskList) mcompanyipaddressmasklist
    . maybe identity (set #cgiDisplayName)    mcompanycgidisplayname
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutPreparation))
            mcompanyidledoctimeoutpreparation
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutClosed))
            mcompanyidledoctimeoutclosed
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutCanceled))
            mcompanyidledoctimeoutcanceled
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutTimedout))
            mcompanyidledoctimeouttimedout
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutRejected))
            mcompanyidledoctimeoutrejected
    . maybe identity
            (set (#dataRetentionPolicy % #idleDocTimeoutError))
            mcompanyidledoctimeouterror
    . maybe identity
            (set (#dataRetentionPolicy % #immediateTrash) . (== "true"))
            mcompanyimmediatetrash
    . maybe identity (set #cgiServiceID) mcompanycgiserviceid
    . maybe identity (set #smsProvider)  mcompanysmsprovider
    . maybe identity (set #padAppMode)   mcompanypadappmode
    . maybe identity (set #padEarchiveEnabled . (== "true")) mcompanypadearchiveenabled
    . maybe identity
            (set #sendTimeoutNotification . (== "true"))
            mcompanysendtimeoutnotification
    . maybe identity (set #totpIsMandatory . (== "true")) mcompanytotpismandatory
    . maybe identity (set #sessionTimeoutSecs)            mcompanysessiontimeout
    . maybe identity (set #portalUrl)                     mcompanyportalurl
    . maybe identity (set #eidServiceToken)               mcompanyeidservicetoken

  where
    getIdleDocTimeoutField :: Kontrakcja m => Text -> m (Maybe (Maybe Int16))
    getIdleDocTimeoutField name = do
      ms <- getField name
      return $ do
        s <- ms
        if T.null s
          then return Nothing
          else do
            t <- maybeRead s
            guard $ t >= minUserGroupIdleDocTimeout
            guard $ t <= maxUserGroupIdleDocTimeout
            return $ Just t

    getSessionTimeoutField :: Text -> m (Maybe (Maybe Int32))
    getSessionTimeoutField fieldName = do
      mField :: Maybe Text <- getField fieldName
      case mField of
        Just timeoutStr -> return $ parseTimeout timeoutStr
        Nothing         -> return Nothing
      where
        parseTimeout :: Text -> Maybe (Maybe Int32)
        -- Use empty string to reset
        parseTimeout str | str == "" = Just Nothing

        -- At least 5 minutes for minimal usability
        -- At most 30 days to prevent any potential security vulnerability
        parseTimeout str =
          let mTimeout :: Maybe Int32 = maybeRead str
          in
            case mTimeout of
              Just timeout
                | timeout >= minSessionTimeoutSecs && timeout <= maxSessionTimeoutSecs
                -> Just (Just timeout)

              Just _  -> Nothing
              Nothing -> Nothing

{- | Reads params and returns function for conversion of user group address.  With no param leaves fields unchanged -}
getUserGroupAddressChange :: Kontrakcja m => m (UserGroupAddress -> UserGroupAddress)
getUserGroupAddressChange = do
  mcompanynumber  <- getField "companynumber"
  mentityname     <- getField "entityname"
  mcompanyaddress <- getField "companyaddress"
  mcompanyzip     <- getField "companyzip"
  mcompanycity    <- getField "companycity"
  mcompanycountry <- getField "companycountry"
  return
    $ maybe identity (set #companyNumber) mcompanynumber
    . maybe identity (set #entityName)    mentityname
    . maybe identity (set #address)       mcompanyaddress
    . maybe identity (set #zipCode)       mcompanyzip
    . maybe identity (set #city)          mcompanycity
    . maybe identity (set #country)       mcompanycountry

{- | Reads params and returns function for conversion of user settings.  No param leaves fields unchanged -}
getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange = do
  mlang <- join <$> fmap langFromCode <$> getField "userlang"
  return $ maybe identity (#lang .~) mlang

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
  muserfstname         <- getField "userfstname"
  musersndname         <- getField "usersndname"
  muserpersonalnumber  <- getField "userpersonalnumber"
  musercompanyposition <- getField "usercompanyposition"
  muserphone           <- getField "userphone"
  museremail           <- fmap Email <$> getField "useremail"
  return $ \userInfo ->
    userInfo
      & (maybe identity (#firstName .~) muserfstname)
      & (maybe identity (#lastName .~) musersndname)
      & (maybe identity (#personalNumber .~) muserpersonalnumber)
      & (maybe identity (#companyPosition .~) musercompanyposition)
      & (maybe identity (#phone .~) muserphone)
      & (maybe identity (#email .~) museremail)

jsonDocuments :: Kontrakcja m => m Response
jsonDocuments = onlyAdmin $ do
  adminUser        <- guardJustM $ view #maybeUser <$> getContext
  muid             <- readField "userid"
  mugid            <- readField "companyid"
  offset           <- guardJustM $ readField "offset"
  maxcount         <- guardJustM $ readField "max"

  requestedFilters <- getFieldBS "filter" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Right js -> case (Unjson.parse Unjson.unjsonDef js) of
        (Result res []) -> return $ join $ toDocumentFilter (adminUser ^. #id) <$> res
        _               -> internalError
      Left _ -> internalError
    Nothing -> return []

  requestedSorting <- getFieldBS "sorting" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Right js -> case (Unjson.parse Unjson.unjsonDef js) of
        (Result res []) -> return $ toDocumentSorting <$> res
        _               -> internalError
      Left _ -> internalError
    Nothing -> return []

  let
    (domain, filtering, sorting) = case (mugid, muid) of
      -- When fetching all documents, we don't allow any filtering, and only default sort is allowed
      (Nothing, Nothing) -> (DocumentsOfWholeUniverse, [], [Desc DocumentOrderByMTime])
      (Just ugid, Nothing) ->
        (DocumentsOfUserGroup ugid, requestedFilters, requestedSorting)
      (Nothing, Just uid) ->
        (DocumentsVisibleToUser uid, requestedFilters, requestedSorting)
      _ -> unexpectedError "Can't pass both user id and company id"
  (allDocsCount, allDocs) <- dbQuery
    $ GetDocumentsWithSoftLimit domain filtering sorting (offset, 1000, maxcount)
  let json =
        listToJSONBS (allDocsCount, (\d -> (documentAccessForAdminonly d, d)) <$> allDocs)
  return $ Response 200 Map.empty nullRsFlags json Nothing


handleBackdoorQuery :: Kontrakcja m => m Response
handleBackdoorQuery = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  emailAddress <- guardJustM $ getField "email_address"
  emailTitle   <- guardJustM $ getField "email_title"
  startDate    <-
    guardJustM
      $ (join . fmap (MinutesTime.parseTimeISO . T.unpack) <$> getField "start_date")
  memail <- dbQuery $ GetEmailForRecipient emailAddress emailTitle startDate
  case memail of
    Nothing    -> respond404
    Just email -> renderFromBody $ mailContent email

sendInviteAgain :: Kontrakcja m => m ()
sendInviteAgain = onlySalesOrAdmin $ do
  uid  <- guardJustM $ readField "userid"
  user <- guardJustM $ dbQuery $ GetUserByID uid
  sendNewUserMail user

-- This method can be used to reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ withDocumentID docid $ do
  logInfo_ "Trying to reseal document (only superadmin can do that)"
  ctx   <- getContext
  actor <- guardJust $ mkAdminActor ctx
  void $ dbUpdate $ InsertEvidenceEvent ResealedPDF (return ()) actor
  void $ postDocumentClosedActions False True
  return LoopBack

-- This method can be used to force postDocumentPendingChange on a doc
-- e.g. when everybody signed but doc is still pending
triggerPostPending :: Kontrakcja m => DocumentID -> m KontraLink
triggerPostPending did = onlyAdmin $ withDocumentID did $ do
  logInfo_
    "Trying to trigger postDocumentPendingChange on document (only superadmin can do that)"
  doc <- dbQuery $ GetDocumentByDocumentID did
  postDocumentPendingChange doc defaultSignatoryLink
  return LoopBack

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink Text)
daveDocument documentid = onlyAdmin $ do
    -- for dave, we want a slash at the end, so redirect if there is no slash
    -- we have a relative link for signatorylinkids, so we need a slash at the end
    -- of the dave/document links; I evaluated a few other ways (using javascript, etc)
    -- but I could not come up with a better one than this
    --  -Eric
  location <- rqUri <$> askRq
  logInfo "Logging location" $ object ["location" .= location]
  if "/" `isSuffixOf` location
    then do
      document        <- dbQuery $ GetDocumentForDave documentid
      mCallbackResult <- dbQuery $ GetDocumentAPICallbackResult documentid
      r               <- renderTextTemplate "daveDocument" $ do
        let everybodySignedAndStatusIn statuses =
              (documentstatus document `elem` statuses) && all
                (   isSignatory
                --> isSignatoryAndHasSigned
                &&  isApprover
                --> isApproverAndHasApproved
                )
                (documentsignatorylinks document)
            callbackResult = fromMaybe "Unknown" mCallbackResult
            extraDoc       = ExtraDocument callbackResult
        F.value "daveBody" $ inspectXML document
        F.value "extraDaveBody" $ inspectXML extraDoc
        F.value "id" $ show documentid
        F.value "couldBeResealed" $ everybodySignedAndStatusIn [Closed, DocumentError]
        F.value "couldBeClosed" $ everybodySignedAndStatusIn [DocumentError, Pending]
      return $ Right r
    else return $ Left $ LinkDaveDocument documentid

{- |
   Used by super users to inspect a particular signatory link.
-}
daveSignatoryLink :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Text
daveSignatoryLink documentid siglinkid = onlyAdmin $ do
  document <- dbQuery $ GetDocumentByDocumentID documentid
  siglink  <- guardJust $ getSigLinkFor siglinkid document
  renderTextTemplate "daveSignatoryLink" $ do
    F.value "daveBody" $ inspectXML siglink

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID -> m Text
daveUser userid = onlyAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID userid
  return $ inspectXML user

{- |
   Used by super users to inspect a particular user's history.
-}
daveUserHistory :: Kontrakcja m => UserID -> m Text
daveUserHistory userid = onlyAdmin $ do
  history <- dbQuery $ GetUserHistoryByUserID userid
  return $ inspectXML history

{- |
    Used by super users to inspect a user group in xml.
-}
daveUserGroup :: Kontrakcja m => UserGroupID -> m Text
daveUserGroup ugid = onlyAdmin $ do
  ug <- guardJustM . dbQuery . UserGroupGet $ ugid
  return $ inspectXML ug

daveFile :: Kontrakcja m => FileID -> Text -> m Response
daveFile fileid _title = onlyAdmin $ do
  now  <- currentTime
  user <- guardJust . contextUser =<< getContext
  logInfo "File accessed through dave"
    $ object [identifier fileid, identifier $ user ^. #id, "timestamp" .= now]
  file     <- dbQuery $ GetFileByFileID fileid
  contents <- getFileContents file
  if BS.null contents
    then internalError
    else do
      let -- Chrome does not like commas in this header
          fname  = T.filter (/= ',') $ filename file
          -- http2 doesnt like non-normalized utf8
          fname' = T.unpack $ ICU.normalize ICU.NFC fname
      return
        $ setHeader "Content-Disposition" ("attachment;filename=" <> fname')
        $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

randomScreenshotForTest :: Kontrakcja m => m Response
randomScreenshotForTest = do
  now <- currentTime
  let lastWeek = 7 `daysBefore` now
  slid <- guardJustM $ dbQuery $ GetRandomSignatoryLinkIDThatSignedRecently lastWeek
  screenshots <- map snd <$> dbQuery (GetSignatoryScreenshots [slid])
  doc         <- dbQuery $ GetDocumentBySignatoryLinkID slid
  elogEvents  <- dbQuery $ GetEvidenceLog $ documentid doc
  let sigElogEvents = filter ((== Just slid) . evSigLink) elogEvents
  content <- renderTextTemplate "screenshotReview" $ do
    F.value "userAgent" $ evClientName <$> find (isJust . evClientName) sigElogEvents
    F.value "signatoryid" $ show slid
    case screenshots of
      ((SignatoryScreenshots mfirst msigning _) : _) -> do
        let screenShowImageString (Screenshot _ img) =
              BS.toString $ RFC2397.encode "image/jpeg" img
        F.value "firstimage" $ screenShowImageString <$> mfirst
        F.value "signingimage" $ screenShowImageString <$> msigning
      _ -> return ()
  simpleHtmlResponse content

handleAdminUserUsageStatsDays :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsDays uid = onlySalesOrAdmin $ do
  user        <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (user ^. #isCompanyAdmin && withCompany)
    then getUsageStats PartitionByDay (UsageStatsForUserGroup $ user ^. #groupID)
    else getUsageStats PartitionByDay (UsageStatsForUser $ user ^. #id)


handleAdminUserUsageStatsMonths :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsMonths uid = onlySalesOrAdmin $ do
  user        <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (user ^. #isCompanyAdmin && withCompany)
    then getUsageStats PartitionByMonth (UsageStatsForUserGroup $ user ^. #groupID)
    else getUsageStats PartitionByMonth (UsageStatsForUser $ user ^. #id)

handleAdminCompanyUsageStatsDays :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsDays ugid =
  onlySalesOrAdmin $ getUsageStats PartitionByDay (UsageStatsForUserGroup ugid)

handleAdminCompanyUsageStatsMonths :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsMonths ugid =
  onlySalesOrAdmin $ getUsageStats PartitionByMonth (UsageStatsForUserGroup ugid)


handleAdminUserShareableLinksStats
  :: Kontrakcja m => StatsPartition -> UserID -> m JSValue
handleAdminUserShareableLinksStats statsPartition uid = onlySalesOrAdmin $ do
  getShareableLinksStats statsPartition (UsageStatsForUser uid)

handleAdminCompanyShareableLinksStats
  :: Kontrakcja m => StatsPartition -> UserGroupID -> m JSValue
handleAdminCompanyShareableLinksStats statsPartition ugid = onlySalesOrAdmin $ do
  getShareableLinksStats statsPartition (UsageStatsForUserGroup ugid)


handleCompanyGetSubscription :: Kontrakcja m => UserGroupID -> m Aeson.Value
handleCompanyGetSubscription ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  unjsonToJSON unjsonDef <$> getSubscription ugwp

handleCompanyUpdateSubscription :: Kontrakcja m => UserGroupID -> m Response
handleCompanyUpdateSubscription ugid = onlySalesOrAdmin . V2.api $ do
  ugwp         <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  subscription <- apiV2ParameterObligatory (ApiV2ParameterJSON "subscription" unjsonDef)

  let newInvoicing =
        case (ugSubInvoicingType subscription, ugSubPaymentPlan subscription) of
          (InvoicingTypeNone    , _      ) -> None
          (InvoicingTypeBillItem, mpp@_  ) -> BillItem mpp
          (InvoicingTypeInvoice , Just pp) -> Invoice pp
          (InvoicingTypeInvoice, Nothing) ->
            unexpectedError "payment plan missing for Invoice type"
      newFeaturesIsInherited = ugSubFeaturesIsInherited subscription
      mNewFeatures           = ugSubFeatures subscription
      mInheritedFeatures     = ugwpFeatures <$> ugwpOnlyParents ugwp
      setFeatures = case (newFeaturesIsInherited, mNewFeatures, mInheritedFeatures) of
        (True, _, Just _) -> set #features Nothing
        (False, Just newFeatures, _) -> set #features $ Just newFeatures
        _ -> unexpectedError "invalid combination of features and inheriting"

  dbUpdate . UserGroupUpdate . set #invoicing newInvoicing . setFeatures . ugwpUG $ ugwp


  case (ugSubInvoicingType subscription, ugSubPaymentPlan subscription) of
    (InvoicingTypeInvoice, Just FreePlan) -> do
      let fdts = freeDocumentTokensFromValues (ugSubValidDocTokensCount subscription)
                                              (ugSubDocTokensValidTill subscription)
      dbUpdate $ UserGroupFreeDocumentTokensUpdate ugid fdts
    _ -> return ()
  return $ V2.Accepted ()

handleCompanyGetStructure :: Kontrakcja m => UserGroupID -> m Aeson.Value
handleCompanyGetStructure ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  let root = ugwpRoot ugwp
  children <- dbQuery . UserGroupGetAllChildrenRecursive $ root ^. #id
  return $ object
    [ "user_group_structure"
        .= (ugWithChildrenToJson $ I.UserGroupWithChildren root children)
    ]
  where
    ugWithChildrenToJson (I.UserGroupWithChildren ug children) = object
      [ "group" .= object ["name" .= (ug ^. #name), identifier $ ug ^. #id]
      , "children" .= map ugWithChildrenToJson children
      ]

handleTriggerMigrateDocuments :: Kontrakcja m => Integer -> m Aeson.Value
handleTriggerMigrateDocuments limit = onlyAdmin $ do
  logInfo_ "Starting migration batch for documents"
  let limitWithUpperBound = minimum [limit, 50000]
  startTime     <- liftIO currentTime
  numberUpdated <- do
    runQuery . sqlUpdate "documents" $ do
      sqlWith "docs_to_update" . sqlSelect "documents d" $ do
        sqlJoinOn "users u" "u.id = d.author_user_id"
        sqlResult "d.id as doc_id"
        sqlResult "u.home_folder_id as folder_id"
        sqlWhereIsNULL "d.folder_id"
        sqlLimit limitWithUpperBound
      sqlSetCmd "folder_id" "docs_to_update.folder_id"
      sqlFrom "docs_to_update"
      sqlWhere "id = docs_to_update.doc_id"
  endTime <- liftIO currentTime
  return
    . object
    $ [ "documents_linked" .= numberUpdated
      , "limit_used" .= limitWithUpperBound
      , "elapsed_time" .= (realToFrac (diffUTCTime endTime startTime) :: Double)
      ]

-- `limit` is an `Integer` to not have to deal with overflows.
handleTriggerMigrateFolders :: Kontrakcja m => Integer -> m Aeson.Value
handleTriggerMigrateFolders limit = onlyAdmin $ do
  logInfo_ "Starting migration batch for folders"
  let limitWithUpperBound = minimum [limit, 10000]
  startTime <- liftIO currentTime
  (idsToUpdate :: [UserGroupID]) <- do
    runQuery_ . sqlSelect "user_groups ug" $ do
      sqlResult "ug.id as ug_id"
      sqlWhereIsNULL "ug.home_folder_id"
      sqlLimit limitWithUpperBound
    fetchMany runIdentity
  numberDone <- dbUpdate . AddFoldersToUserGroups $ idsToUpdate
  endTime    <- liftIO currentTime
  return
    . object
    $ [ "home_folders_created" .= numberDone
      , "limit_used" .= limitWithUpperBound
      , "elapsed_time" .= (realToFrac (diffUTCTime endTime startTime) :: Double)
      ]

jsonBrandedDomainsList :: Kontrakcja m => m Aeson.Value
jsonBrandedDomainsList = onlySalesOrAdmin $ do
  murlpart          <- getField "text"
  allBrandedDomains <- dbQuery $ GetBrandedDomains murlpart
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True })
                                unjsonBrandedDomainsList
                                allBrandedDomains

jsonBrandedDomain :: Kontrakcja m => BrandedDomainID -> m Aeson.Value
jsonBrandedDomain bdID = onlySalesOrAdmin $ do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True })
                                unjsonBrandedDomain
                                bd

updateBrandedDomain :: Kontrakcja m => BrandedDomainID -> m ()
updateBrandedDomain xbdid = onlySalesOrAdmin $ do
  obd <- dbQuery $ GetBrandedDomainByID xbdid
  when (obd ^. #mainDomain) $ do
    logInfo_ "Main domain can't be changed"
    internalError
  -- keep this 1to1 consistent with fields in the database
  domainJSON <- guardJustM $ getFieldBS "domain"
  case Aeson.eitherDecode $ domainJSON of
    Left err -> do
      logInfo "Error while parsing branding for adminonly" $ object ["error" .= err]
      internalError
    Right js -> case (Unjson.parse unjsonBrandedDomain js) of
      (Result newDomain []) -> do
        void
          $ dbUpdate
          $ UpdateBrandedDomain
          $ copy #id         obd
          $ copy #mainDomain obd
          $ newDomain
        return ()
      _ -> internalError

unjsonBrandedDomain :: UnjsonDef BrandedDomain
unjsonBrandedDomain =
  objectOf
    $   pure I.BrandedDomain
    <*> field "id"              (^. #id)              "Id of a branded domain (unique)"
    <*> field "mainDomain"      (^. #mainDomain)      "Is this a main domain"
    <*> field "url"             (^. #url)             "URL that will match this domain"
    <*> field "smsOriginator"   (^. #smsOriginator)   "Originator for text messages"
    <*> field "emailOriginator" (^. #emailOriginator) "Originator for email messages"
    <*> field "mailTheme"       (^. #mailTheme)       "Email theme"
    <*> field "signviewTheme"   (^. #signviewTheme)   "Signview theme"
    <*> field "serviceTheme"    (^. #serviceTheme)    "Service theme"
    <*> field "loginTheme"      (^. #loginTheme)      "Login theme"
    <*> field "browserTitle"    (^. #browserTitle)    "Browser title"
    <*> fieldBy
          "favicon"
          (^. #favicon)
          "Favicon"
          (invmap
            (\l -> B64.decodeLenient $ BSC8.pack $ drop 1 $ dropWhile ((/=) ',') l)
            (\l ->
              BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l
            )
            unjsonDef
          )
    <*> field "participantColor1" (^. #participantColor1) "Participant 1 color"
    <*> field "participantColor2" (^. #participantColor2) "Participant 2 color"
    <*> field "participantColor3" (^. #participantColor3) "Participant 3 color"
    <*> field "participantColor4" (^. #participantColor4) "Participant 4 color"
    <*> field "participantColor5" (^. #participantColor5) "Participant 5 color"
    <*> field "participantColor6" (^. #participantColor6) "Participant 6 color"
    <*> field "draftColor"        (^. #draftColor)        "Draft color"
    <*> field "cancelledColor"    (^. #cancelledColor)    "Cancelled color"
    <*> field "initatedColor"     (^. #initatedColor)     "Initated color"
    <*> field "sentColor"         (^. #sentColor)         "Sent color"
    <*> field "deliveredColor"    (^. #deliveredColor)    "Delivered color"
    <*> field "openedColor"       (^. #openedColor)       "Opened color"
    <*> field "reviewedColor"     (^. #reviewedColor)     "Reviewed color"
    <*> field "signedColor"       (^. #signedColor)       "Signed color"

unjsonBrandedDomainsList :: UnjsonDef [BrandedDomain]
unjsonBrandedDomainsList = objectOf
  $ fieldBy "domains" identity "List of branded domains" (arrayOf unjsonBrandedDomain)


createBrandedDomain :: Kontrakcja m => m JSValue
createBrandedDomain = do
  bdID <- dbUpdate $ NewBrandedDomain
  runJSONGenT $ do
    value "id" (show bdID)
