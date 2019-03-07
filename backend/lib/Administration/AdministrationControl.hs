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
module Administration.AdministrationControl(
            adminonlyRoutes
          , daveRoutes
          , jsonCompanies -- for tests
          , handleCompanyChange -- for tests
          , handleTriggerMigrateFolders -- for tests
          , handleTriggerMigrateDocuments -- for tests
          ) where

import Control.Monad.Catch (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.Either (rights)
import Data.Functor.Invariant
import Data.Time (diffUTCTime)
import Data.Unjson
import GHC.Int (Int16)
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
import qualified Data.String.Utils as SU (strip)
import qualified Data.Text as T
import qualified Data.Text.ICU.Normalize as ICU
import qualified Data.Unjson as Unjson
import qualified Text.StringTemplates.Fields as F

import Administration.AdministrationView
import API.V2.Parameters
import AppView (renderFromBody, simpleHtmlResponse)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import DataRetentionPolicy
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
import InternalResponse
import IPAddress ()
import Kontra
import KontraLink
import Log.Identifier
import Mails.Model
import MinutesTime
import PadApplication.Types (padAppModeFromText)
import Routing
import Theme.Control
import User.CallbackScheme.Model
import User.Email
import User.History.Model
import User.JSON
import User.UserControl
import User.UserView
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.Subscription
import UserGroupAccounts.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Utils.Monoid
import qualified API.V2 as V2
import qualified Company.CompanyControl as Company
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified UserGroupAccounts.UserGroupAccountsControl as UserGroupAccounts

adminonlyRoutes :: Route (Kontra Response)
adminonlyRoutes =
  fmap onlySalesOrAdmin $ choice $ [
          hGet $ toK0 $ showAdminMainPage
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

        , dir "useradmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminUserUsageStatsDays
        , dir "useradmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminUserUsageStatsMonths

        , dir "useradmin" $ dir "shareablelinkstats" $ dir "days" $ hGet
          $ toK1 $ handleAdminUserShareableLinksStats PartitionByDay
        , dir "useradmin" $ dir "shareablelinkstats" $ dir "months" $ hGet
          $ toK1 $ handleAdminUserShareableLinksStats PartitionByMonth

        , dir "useradmin" $ dir "sendinviteagain" $ hPost $ toK0 $ sendInviteAgain

        , dir "companyadmin" $ hGet $ toK1 $ showAdminCompany
        , dir "companyadmin" $ dir "details" $ hGet $ toK1 $ handleCompanyGetProfile

        , dir "companyadmin" $ hPost $ toK1 $ handleCompanyChange
        , dir "companyadmin" $ dir "merge" $ hPost $ toK1 $ handleMergeToOtherCompany

        , dir "companyadmin" $ dir "branding" $ Company.adminRoutes
        , dir "companyadmin" $ dir "users" $ hPost $ toK1 $ handlePostAdminCompanyUsers

        , dir "companyaccounts" $ hGet  $ toK1 $ UserGroupAccounts.handleUserGroupAccountsForAdminOnly
        , dir "companyadmin" $ dir "usagestats" $ dir "days" $ hGet $ toK1 handleAdminCompanyUsageStatsDays
        , dir "companyadmin" $ dir "usagestats" $ dir "months" $ hGet $ toK1 handleAdminCompanyUsageStatsMonths

        , dir "companyadmin" $ dir "shareablelinkstats" $ dir "days" $ hGet
          $ toK1 $ handleAdminCompanyShareableLinksStats PartitionByDay
        , dir "companyadmin" $ dir "shareablelinkstats" $ dir "months" $ hGet
          $ toK1 $ handleAdminCompanyShareableLinksStats PartitionByMonth

        , dir "companyadmin" $ dir "getsubscription" $ hGet $ toK1 $ handleCompanyGetSubscription
        , dir "companyadmin" $ dir "updatesubscription" $ hPost $ toK1 $ handleCompanyUpdateSubscription

        , dir "companyadmin" $ dir "getstructure" $ hGet $ toK1 $ handleCompanyGetStructure

        , dir "documentslist" $ hGet $ toK0 $ jsonDocuments

        , dir "companies" $ hGet $ toK0 $ jsonCompanies

        , dir "brandeddomainslist" $ hGet $ toK0 $ jsonBrandedDomainsList
        , dir "brandeddomain" $ dir "create" $ hPost $ toK0 $ createBrandedDomain
        , dir "brandeddomain" $ dir "details" $ hGet $ toK1 $ jsonBrandedDomain
        , dir "brandeddomain" $ dir "details" $ dir "change" $ hPost $ toK1 $ updateBrandedDomain
        , dir "brandeddomain" $ dir "themes" $ hGet $ toK1 $ handleGetThemesForDomain
        , dir "brandeddomain" $ dir "newtheme" $ hPost $ toK2 $ handleNewThemeForDomain
        , dir "brandeddomain" $ dir "updatetheme" $ hPost $ toK2 $ handleUpdateThemeForDomain
        , dir "brandeddomain" $ dir "deletetheme" $ hPost $ toK2$ handleDeleteThemeForDomain

        -- migration trigging endpoints       
        , dir "triggermigratefolders" $ hGet $ toK1 handleTriggerMigrateFolders
        , dir "triggermigratedocuments" $ hGet $ toK1 handleTriggerMigrateDocuments

  ]

daveRoutes :: Route (Kontra Response)
daveRoutes =
  fmap onlyAdmin $ choice $ [
       dir "document"      $ hGet $ toK1 $ daveDocument
     , dir "document"      $ hGet $ toK2 $ daveSignatoryLink
     , dir "user"          $ hGet $ toK1 $ daveUser
     , dir "userhistory"   $ hGet $ toK1 $ daveUserHistory
     , dir "usergroup"       $ hGet $ toK1 $ daveUserGroup
     , dir "reseal" $ hPost $ toK1 $ resealFile
     , dir "postpending" $ hPost $ toK1 $ triggerPostPending
     , dir "file"   $ hGet  $ toK2 $ daveFile
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

handleUserGetProfile:: Kontrakcja m => UserID -> m JSValue
handleUserGetProfile uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  callback <- dbQuery $ GetUserCallbackSchemeByUserID uid
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ uid
  return $ userJSONWithCallBackInfo user ugwp callback

handleCompanyGetProfile:: Kontrakcja m => UserGroupID -> m JSValue
handleCompanyGetProfile ugid = onlySalesOrAdmin $
  companyJSONAdminOnly <$> (guardJustM . dbQuery . UserGroupGetWithParents $ ugid)

showAdminCompany :: Kontrakcja m => UserGroupID -> m String
showAdminCompany ugid = onlySalesOrAdmin $ do
  ctx <- getContext
  adminCompanyPage ctx ugid

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
    limit    <- guardJustM $ readField "limit"
    offset   <- guardJustM $ readField "offset"
    textFilter <- getField "text" >>= \case
                     Nothing -> return []
                     Just s -> return [UGFilterByString s]
    usersFilter <- isFieldSet "allCompanies" >>= \case
                     True ->  return []
                     False -> return [UGManyUsers]
    pplanFilter <- isFieldSet "nonFree" >>= \case
                     True ->  return [UGWithNonFreePricePlan]
                     False -> return []
    ugs <- dbQuery $ UserGroupsGetFiltered (textFilter ++ usersFilter ++ pplanFilter) (Just (offset, limit))
    -- get address for those companies, which inherit it
    ugsWithAddress <- forM ugs $ \ug -> case get ugAddress ug of
      Just uga -> return (ug, uga)
      Nothing -> ((ug,) . ugwpAddress)
        <$> (guardJustM . dbQuery . UserGroupGetWithParents $ get ugID ug)
    runJSONGenT $ do
      valueM "companies" $ forM ugsWithAddress $ \(ug, uga) ->
        runJSONGenT $ do
          value "id"             . show . get ugID $ ug
          value "companyname"    . T.unpack . get ugName $ ug
          value "companynumber"  . T.unpack . get ugaCompanyNumber $ uga
          value "companyaddress" . T.unpack . get ugaAddress       $ uga
          value "companyzip"     . T.unpack . get ugaZip           $ uga
          value "companycity"    . T.unpack . get ugaCity          $ uga
          value "companycountry" . T.unpack . get ugaCountry       $ uga

jsonUsersList ::Kontrakcja m => m JSValue
jsonUsersList = onlySalesOrAdmin $ do
    limit    <- guardJustM $ readField "limit"
    offset   <- guardJustM $ readField "offset"
    textFilter <- getField "text" >>= \case
                     Nothing -> return []
                     Just s -> return [UserFilterByString s]
    sorting <- getField "tosSorting" >>= \case
                     Just "ascending"   -> return [Asc UserOrderByAccountCreationDate]
                     Just "descending" -> return [Desc UserOrderByAccountCreationDate]
                     _ -> return [Asc UserOrderByName]
    allUsers <- dbQuery $ GetUsersWithUserGroupNames textFilter sorting (offset,limit)

    runJSONGenT $ do
      valueM "users" $ forM (allUsers) $ \(user,ugname) -> runJSONGenT $ do
        value "id" $ show $ userid user
        value "username" $ getFullName user
        value "email"    $ getEmail user
        value "companyposition" $ usercompanyposition $ userinfo user
        value "company"  . T.unpack $ ugname
        value "phone"    $ userphone $ userinfo user
        value "tos"      $ formatTimeISO <$> (userhasacceptedtermsofservice user)
        value "twofactor_active" $ usertotpactive user


{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m JSValue
handleUserChange uid = onlySalesOrAdmin $ do
  ctx <- getContext
  callback <- dbQuery $ GetUserCallbackSchemeByUserID uid
  maybeNewUrl <- getField "usercallbackurl"
  let newUrl = SU.strip $ fromJust maybeNewUrl
  case callback of
    Nothing -> if null newUrl
      then return () -- Don't add a callback with an empty URL
      else dbUpdate $ UpdateUserCallbackScheme uid (ConstantUrlSchemeV2 newUrl)
    Just (ConstantUrlSchemeV2 url) -> do
      case (null newUrl, newUrl == url) of
        (True, _) -> dbUpdate $ DeleteUserCallbackScheme uid -- Delete callback if textbox emptied
        (False, True) -> return () -- Don't update if no change
        (False, False) -> dbUpdate $ UpdateUserCallbackScheme uid (ConstantUrlSchemeV2 newUrl)
    Just _ -> return () -- Do not allow changing the callback if an existing other type is there
  museraccounttype <- getField "useraccounttype"
  olduser <- guardJustM $ dbQuery $ GetUserByID uid
  user <- case (museraccounttype,useriscompanyadmin olduser) of
    (Just "companyadminaccount",  False) -> do
      --then we just want to make this account an admin
      newuser <- guardJustM $ do
        void $ dbUpdate $ SetUserCompanyAdmin uid True
        void $ dbUpdate $ LogHistoryDetailsChanged uid (get ctxipnumber ctx) (get ctxtime ctx)
             [("is_company_admin", "false", "true")]
             (userid <$> get ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    (Just "companystandardaccount", True) -> do
      --then we just want to downgrade this account to a standard
      newuser <- guardJustM $ do
        void $ dbUpdate $ SetUserCompanyAdmin uid False
        void $ dbUpdate
                 $ LogHistoryDetailsChanged uid (get ctxipnumber ctx) (get ctxtime ctx)
                                            [("is_company_admin", "true", "false")]
                                            (userid <$> get ctxmaybeuser ctx)
        dbQuery $ GetUserByID uid
      return newuser
    _ -> return olduser
  infoChange <- getUserInfoChange
  let applyChanges = do
        void $ dbUpdate $ SetUserInfo uid $ infoChange $ userinfo user
        void $ dbUpdate
              $ LogHistoryUserInfoChanged uid (get ctxipnumber ctx) (get ctxtime ctx)
                    (userinfo user) (infoChange $ userinfo user)
                    (userid <$> get ctxmaybeuser ctx)
        settingsChange <- getUserSettingsChange
        void $ dbUpdate $ SetUserSettings uid $ settingsChange $ usersettings user
        return ()
  if (useremail (infoChange $ userinfo user) /= useremail (userinfo user))
    then do
      -- email address changed, check if new one is not used
      mexistinguser <- dbQuery $ GetUserByEmail $ useremail $ infoChange $ userinfo user
      case mexistinguser of
        Just _ -> runJSONGenT $ value "changed" False
        Nothing -> do
          applyChanges
          runJSONGenT $ value "changed" True
    else do
      applyChanges
      runJSONGenT $ value "changed" True


{- | Handling user password change. -}
handleUserPasswordChange :: Kontrakcja m => UserID -> m JSValue
handleUserPasswordChange uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  password <- guardJustM $ getField "password"
  passwordhash <- createPassword password
  ctx <- getContext
  let time     = get ctxtime ctx
      ipnumber = get ctxipnumber ctx
      admin    = get ctxmaybeuser ctx
  void $ dbUpdate $ SetUserPassword (userid user) passwordhash
  void $ dbUpdate $ LogHistoryPasswordSetup (userid user) ipnumber time (userid <$> admin)
  runJSONGenT $ value "changed" True

handleDeleteInvite :: Kontrakcja m => UserGroupID -> UserID -> m ()
handleDeleteInvite ugid uid = onlySalesOrAdmin $ do
  void $ dbUpdate $ RemoveUserGroupInvite ugid uid
  return ()

handleDeleteUser :: Kontrakcja m => UserID -> m ()
handleDeleteUser uid = onlySalesOrAdmin $ do
  void $ dbUpdate $ RemoveUserUserGroupInvites uid
  void $ dbUpdate $ DeleteUserCallbackScheme uid
  void $ dbUpdate $ DeleteUser uid
  return ()

handleDisable2FAForUser :: Kontrakcja m => UserID -> m ()
handleDisable2FAForUser uid = onlySalesOrAdmin $ do
  ctx <- getContext
  user <- guardJustM $ dbQuery $ GetUserByID uid
  if usertotpactive user
     then do
       r <- dbUpdate $ DisableUserTOTP uid
       if r
          then do
            void $ dbUpdate $ LogHistoryTOTPDisable uid (get ctxipnumber ctx) (get ctxtime ctx)
            return ()
          else
            internalError
     else return ()

handleMoveUserToDifferentCompany :: Kontrakcja m => UserID -> m ()
handleMoveUserToDifferentCompany uid = onlySalesOrAdmin $ do
  ugid <- guardJustM $ readField "companyid"
  void $ dbUpdate $ SetUserUserGroup uid ugid
  void $ dbUpdate $ SetUserCompanyAdmin uid False
  return ()


handleMergeToOtherCompany :: Kontrakcja m => UserGroupID -> m ()
handleMergeToOtherCompany ugid_source = onlySalesOrAdmin $ do
  ugid_target <- guardJustM $ readField "companyid"
  users <- dbQuery $ UserGroupGetUsers ugid_source
  forM_ users $ \u -> do
      void $ dbUpdate $ SetUserUserGroup (userid u) ugid_target
      return ()
  invites <- dbQuery $ UserGroupGetInvites ugid_source
  forM_ invites $ \i-> do
      void $ dbUpdate $ RemoveUserGroupInvite ugid_source (inviteduserid i)
      return ()

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => UserGroupID -> m ()
handleCompanyChange ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM $ dbQuery $ UserGroupGetWithParents ugid
  mCompanyName <- getField "companyname"
  mUGSettingsIsInherited <- fmap (==("true"::String))
    <$> getField "companysettingsisinherited"
  ugSettingsChange <- getUserGroupSettingsChange
  mUGAddressIsInherited <- fmap (==("true"::String))
    <$> getField "companyaddressisinherited"
  ugAddressChange <- getUserGroupAddressChange
  mTryParentUserGroupID <- getOptionalField asValidUserGroupID "companyparentid"

  let oldUG = ugwpUG ugwp
      setSettings =
        if   fromMaybe (isNothing $ get ugSettings oldUG) mUGSettingsIsInherited
        then set ugSettings Nothing
        else set ugSettings . Just . ugSettingsChange $ ugwpSettings ugwp
      setAddress =
        if   fromMaybe (isNothing $ get ugAddress oldUG) mUGAddressIsInherited
        then set ugAddress Nothing
        else set ugAddress . Just . ugAddressChange $ ugwpAddress ugwp
      newUG =
          set ugParentGroupID mTryParentUserGroupID
        . maybe id (set ugName . T.pack) mCompanyName
        . setSettings
        . setAddress
        $ ugwpUG ugwp

  newSettings <- guardJust . listToMaybe . catMaybes $
    [ get ugSettings newUG
    , ugwpSettings <$> ugwpOnlyParents ugwp ]
  guardThatDataRetentionPolicyIsValid
    (get ugsDataRetentionPolicy newSettings) Nothing
  dbUpdate $ UserGroupUpdate newUG
  return $ ()

handleCreateUser :: Kontrakcja m => m JSValue
handleCreateUser = onlySalesOrAdmin $ do
    email <- filter (/=' ') <$> map toLower <$> (guardJustM $ getField "email")
    fstname <- guardJustM $ getField "fstname"
    sndname <- guardJustM $ getField "sndname"
    lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
    ugFolder <- dbUpdate . FolderCreate $ defaultFolder
    ug <- dbUpdate . UserGroupCreate . set ugHomeFolderID (Just $ get folderID ugFolder) $ defaultUserGroup
    muser <- createNewUserByAdmin email (fstname, sndname) (get ugID ug, True) lang
    runJSONGenT $ case muser of
      Nothing -> do
        value "success" False
        valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
      Just _ -> do
        value "success" True
        value "error_message" (Nothing :: Maybe String)

handlePostAdminCompanyUsers :: Kontrakcja m => UserGroupID -> m JSValue
handlePostAdminCompanyUsers ugid = onlySalesOrAdmin $ do
  email <- getCriticalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  lang <- guardJustM $ join <$> fmap langFromCode <$> getField "lang"
  admin <- isFieldSet "iscompanyadmin"
  muser <- createNewUserByAdmin email (fstname,sndname) (ugid, admin) lang
  runJSONGenT $ case muser of
    Nothing -> do
      value "success" False
      valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
    Just _ -> do
      value "success" True
      value "error_message" (Nothing :: Maybe String)

{- | Reads params and returns function for conversion of user group info.  With no param leaves fields unchanged -}
getUserGroupSettingsChange :: Kontrakcja m => m (UserGroupSettings -> UserGroupSettings)
getUserGroupSettingsChange = do
    mcompanyipaddressmasklist <- getOptionalField asValidIPAddressWithMaskList "companyipaddressmasklist"
    mcompanycgidisplayname <- fmap emptyToNothing <$> getField "companycgidisplayname"
    mcompanycgiserviceid <- fmap emptyToNothing <$> getField "companycgiserviceid"
    mcompanyidledoctimeoutpreparation <- getIdleDocTimeoutField "companyidledoctimeoutpreparation"
    mcompanyidledoctimeoutclosed <- getIdleDocTimeoutField "companyidledoctimeoutclosed"
    mcompanyidledoctimeoutcanceled <- getIdleDocTimeoutField "companyidledoctimeoutcanceled"
    mcompanyidledoctimeouttimedout <- getIdleDocTimeoutField "companyidledoctimeouttimedout"
    mcompanyidledoctimeoutrejected <- getIdleDocTimeoutField "companyidledoctimeoutrejected"
    mcompanyidledoctimeouterror <- getIdleDocTimeoutField "companyidledoctimeouterror"
    mcompanyimmediatetrash <- getField "companyimmediatetrash"
    mcompanysmsprovider <- fmap maybeRead <$> getField' $ "companysmsprovider"
    mcompanypadappmode <- fmap (padAppModeFromText . T.pack) <$> getField' $ "companypadappmode"
    mcompanypadearchiveenabled <- getField "companypadearchiveenabled"

    return $
        maybe id (set ugsIPAddressMaskList) mcompanyipaddressmasklist
      . maybe id (set ugsCGIDisplayName . fmap T.pack) mcompanycgidisplayname
      . maybe id (set (drpIdleDocTimeoutPreparation . ugsDataRetentionPolicy))
              mcompanyidledoctimeoutpreparation
      . maybe id (set (drpIdleDocTimeoutClosed . ugsDataRetentionPolicy))
              mcompanyidledoctimeoutclosed
      . maybe id (set (drpIdleDocTimeoutCanceled . ugsDataRetentionPolicy))
              mcompanyidledoctimeoutcanceled
      . maybe id (set (drpIdleDocTimeoutTimedout . ugsDataRetentionPolicy))
              mcompanyidledoctimeouttimedout
      . maybe id (set (drpIdleDocTimeoutRejected . ugsDataRetentionPolicy))
              mcompanyidledoctimeoutrejected
      . maybe id (set (drpIdleDocTimeoutError . ugsDataRetentionPolicy))
              mcompanyidledoctimeouterror
      . maybe id (set (drpImmediateTrash . ugsDataRetentionPolicy)
                  . (=="true"))
              mcompanyimmediatetrash
      . maybe id (set ugsCGIServiceID . fmap T.pack) mcompanycgiserviceid
      . maybe id (set ugsSMSProvider) mcompanysmsprovider
      . maybe id (set ugsPadAppMode) mcompanypadappmode
      . maybe id (set ugsPadEarchiveEnabled . (=="true")) mcompanypadearchiveenabled

  where
    getIdleDocTimeoutField :: Kontrakcja m => String -> m (Maybe (Maybe Int16))
    getIdleDocTimeoutField name = do
      ms <- getField name
      return $ do
        s <- ms
        if null s
          then return Nothing
          else do
            t <- maybeRead s
            guard $ t >= minUserGroupIdleDocTimeout
            guard $ t <= maxUserGroupIdleDocTimeout
            return $ Just t

{- | Reads params and returns function for conversion of user group address.  With no param leaves fields unchanged -}
getUserGroupAddressChange :: Kontrakcja m => m (UserGroupAddress -> UserGroupAddress)
getUserGroupAddressChange = do
  mcompanynumber  <- getField "companynumber"
  mcompanyaddress <- getField "companyaddress"
  mcompanyzip     <- getField "companyzip"
  mcompanycity    <- getField "companycity"
  mcompanycountry <- getField "companycountry"
  return $
      maybe id (set ugaCompanyNumber . T.pack) mcompanynumber
    . maybe id (set ugaAddress . T.pack) mcompanyaddress
    . maybe id (set ugaZip . T.pack) mcompanyzip
    . maybe id (set ugaCity . T.pack) mcompanycity
    . maybe id (set ugaCountry . T.pack) mcompanycountry

{- | Reads params and returns function for conversion of user settings.  No param leaves fields unchanged -}
getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange = do
  mlang <- join <$> fmap langFromCode <$> getField "userlang"
  return $ \settings -> settings {
     lang = fromMaybe (lang settings) mlang
  }

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
  muserfstname         <- getField "userfstname"
  musersndname         <- getField "usersndname"
  muserpersonalnumber  <- getField "userpersonalnumber"
  musercompanyposition <- getField "usercompanyposition"
  muserphone           <- getField "userphone"
  museremail           <- fmap Email <$> getField "useremail"
  return $ \UserInfo{..} -> UserInfo {
        userfstname         = fromMaybe userfstname muserfstname
      , usersndname         = fromMaybe usersndname musersndname
      , userpersonalnumber  = fromMaybe userpersonalnumber muserpersonalnumber
      , usercompanyposition = fromMaybe usercompanyposition musercompanyposition
      , userphone           = fromMaybe userphone muserphone
      , useremail           = fromMaybe useremail museremail
    }

jsonDocuments :: Kontrakcja m => m Response
jsonDocuments = onlyAdmin $ do
  adminUser <- guardJustM $ get ctxmaybeuser <$> getContext
  muid <- readField "userid"
  mugid <- readField "companyid"
  offset   <- guardJustM $ readField "offset"
  maxcount <- guardJustM $ readField  "max"

  requestedFilters <- getFieldBS "filter" >>= \case
      Just paramValue -> case Aeson.eitherDecode paramValue of
         Right js -> case (Unjson.parse Unjson.unjsonDef js) of
            (Result res []) -> return $ join $ toDocumentFilter (userid adminUser) <$> res
            _ -> internalError
         Left _ -> internalError
      Nothing -> return []

  requestedSorting <- getFieldBS "sorting" >>= \case
      Just paramValue -> case Aeson.eitherDecode paramValue of
         Right js -> case (Unjson.parse Unjson.unjsonDef js) of
            (Result res []) -> return $ toDocumentSorting <$> res
            _ -> internalError
         Left _ -> internalError
      Nothing -> return []

  let (domain,filtering, sorting)     = case (mugid, muid) of
        -- When fetching all documents, we don't allow any filtering, and only default sort is allowed
        (Nothing, Nothing)   -> (DocumentsOfWholeUniverse,[],[Desc DocumentOrderByMTime])
        (Just ugid, Nothing) -> (DocumentsOfUserGroup ugid,requestedFilters,requestedSorting)
        (Nothing, Just uid)  -> (DocumentsVisibleToUser uid, requestedFilters,requestedSorting)
        _                    -> unexpectedError "Can't pass both user id and company id"
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit domain filtering sorting (offset, 1000, maxcount)
  let json = listToJSONBS (allDocsCount,(\d -> (documentAccessForAdminonly d,d)) <$> allDocs)
  return $ Response 200 Map.empty nullRsFlags json Nothing


handleBackdoorQuery :: Kontrakcja m => m Response
handleBackdoorQuery = onlySalesOrAdmin $ onlyBackdoorOpen $ do
  emailAddress <- guardJustM $ getField "email_address"
  emailTitle   <- guardJustM $ getField "email_title"
  startDate    <- guardJustM $
                  (join . fmap MinutesTime.parseTimeISO <$> getField "start_date")
  memail       <- dbQuery $ GetEmailForRecipient emailAddress emailTitle startDate
  case memail of
    Nothing -> respond404
    Just email -> renderFromBody $ mailContent email

sendInviteAgain :: Kontrakcja m => m InternalKontraResponse
sendInviteAgain = onlySalesOrAdmin $ do
  uid <- guardJustM $ readField "userid"
  user <- guardJustM $ dbQuery $ GetUserByID uid
  sendNewUserMail user
  flashmessage <- flashMessageNewActivationLinkSend
  return $ internalResponseWithFlash flashmessage LoopBack

-- This method can be used to reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin $ withDocumentID docid $ do
  logInfo_ "Trying to reseal document (only superadmin can do that)"
  ctx <- getContext
  actor <- guardJust $ mkAdminActor ctx
  void $ dbUpdate $ InsertEvidenceEvent
          ResealedPDF
          (return ())
          actor
  void $ postDocumentClosedActions False True
  return LoopBack

-- This method can be used to force postDocumentPendingChange on a doc
-- e.g. when everybody signed but doc is still pending
triggerPostPending :: Kontrakcja m => DocumentID -> m KontraLink
triggerPostPending did = onlyAdmin $ withDocumentID did $ do
  logInfo_ "Trying to trigger postDocumentPendingChange on document (only superadmin can do that)"
  doc <- dbQuery $ GetDocumentByDocumentID did
  postDocumentPendingChange doc defaultSignatoryLink
  return LoopBack

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: Kontrakcja m => DocumentID -> m (Either KontraLink String)
daveDocument documentid = onlyAdmin $ do
    -- for dave, we want a slash at the end, so redirect if there is no slash
    -- we have a relative link for signatorylinkids, so we need a slash at the end
    -- of the dave/document links; I evaluated a few other ways (using javascript, etc)
    -- but I could not come up with a better one than this
    --  -Eric
    location <- rqUri <$> askRq
    logInfo "Logging location" $ object [
        "location" .= location
      ]
    if "/" `isSuffixOf` location
     then do
      document <- dbQuery $ GetDocumentForDave documentid
      mCallbackResult <- dbQuery $ GetDocumentAPICallbackResult documentid
      r <- renderTemplate "daveDocument" $ do
        let everybodySignedAndStatusIn statuses =
              (documentstatus document `elem` statuses) &&
                all (isSignatory --> isSignatoryAndHasSigned
                  && isApprover  --> isApproverAndHasApproved)
                (documentsignatorylinks document)
            callbackResult = fromMaybe "Unknown" mCallbackResult
            extraDoc = ExtraDocument callbackResult
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
daveSignatoryLink :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m  String
daveSignatoryLink documentid siglinkid = onlyAdmin $ do
    document <- dbQuery $ GetDocumentByDocumentID documentid
    siglink <- guardJust $ getSigLinkFor siglinkid document
    renderTemplate  "daveSignatoryLink" $ do
        F.value "daveBody" $ inspectXML siglink

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID ->  m String
daveUser userid = onlyAdmin $ do
    user <- guardJustM $ dbQuery $ GetUserByID userid
    return $ inspectXML user

{- |
   Used by super users to inspect a particular user's history.
-}
daveUserHistory :: Kontrakcja m => UserID -> m String
daveUserHistory userid = onlyAdmin $ do
    history <- dbQuery $ GetUserHistoryByUserID userid
    return $ inspectXML history

{- |
    Used by super users to inspect a user group in xml.
-}
daveUserGroup :: Kontrakcja m => UserGroupID -> m String
daveUserGroup ugid = onlyAdmin $ do
  ug <- guardJustM . dbQuery . UserGroupGet $ ugid
  return $ inspectXML ug

daveFile :: Kontrakcja m => FileID -> String -> m Response
daveFile fileid _title = onlyAdmin $ do
  now <- currentTime
  user <- guardJust . getContextUser =<< getContext
  logInfo "File accessed through dave" $ object
    [ identifier fileid
    , identifier $ userid user
    , "timestamp" .= now
    ]
  file <- dbQuery $ GetFileByFileID fileid
  contents <- getFileContents file
  if BS.null contents
    then internalError
    else do
      let -- Chrome does not like commas in this header
          fname = filter (/=',') $ filename file
          -- http2 doesnt like non-normalized utf8
          fname' = T.unpack $ ICU.normalize ICU.NFC $ T.pack fname
      return
        $ setHeader "Content-Disposition" ("attachment;filename=" ++ fname')
        $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

randomScreenshotForTest :: Kontrakcja m => m Response
randomScreenshotForTest = do
  now <- currentTime
  let lastWeek = 7 `daysBefore` now
  slid <- guardJustM $ dbQuery $ GetRandomSignatoryLinkIDThatSignedRecently lastWeek
  screenshots <- map snd <$> dbQuery (GetSignatoryScreenshots [slid])
  doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
  elogEvents <- dbQuery $ GetEvidenceLog $ documentid doc
  let sigElogEvents = filter ((== Just slid) . evSigLink) elogEvents
  content <- renderTemplate "screenshotReview" $ do
    F.value "userAgent" $ evClientName <$> find (isJust . evClientName) sigElogEvents
    F.value "signatoryid" $ show slid
    case screenshots of
      ((SignatoryScreenshots mfirst msigning _):_) -> do
        let screenShowImageString (Screenshot _ img) = BS.toString $ RFC2397.encode "image/jpeg" img
        F.value "firstimage" $ screenShowImageString <$> mfirst
        F.value "signingimage" $ screenShowImageString <$> msigning
      _ -> return ()
  simpleHtmlResponse content

handleAdminUserUsageStatsDays :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsDays uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getUsageStats PartitionByDay (UsageStatsForUserGroup $ usergroupid user)
    else getUsageStats PartitionByDay (UsageStatsForUser $ userid user)


handleAdminUserUsageStatsMonths :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsMonths uid = onlySalesOrAdmin $ do
  user <- guardJustM $ dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if (useriscompanyadmin user && withCompany)
    then getUsageStats PartitionByMonth (UsageStatsForUserGroup $ usergroupid user)
    else getUsageStats PartitionByMonth (UsageStatsForUser $ userid user)

handleAdminCompanyUsageStatsDays :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsDays ugid = onlySalesOrAdmin $
  getUsageStats PartitionByDay (UsageStatsForUserGroup ugid)

handleAdminCompanyUsageStatsMonths :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsMonths ugid = onlySalesOrAdmin $
  getUsageStats PartitionByMonth (UsageStatsForUserGroup ugid)


handleAdminUserShareableLinksStats :: Kontrakcja m => StatsPartition -> UserID -> m JSValue
handleAdminUserShareableLinksStats statsPartition uid = onlySalesOrAdmin $ do
  getShareableLinksStats statsPartition (UsageStatsForUser uid)

handleAdminCompanyShareableLinksStats :: Kontrakcja m
                                      => StatsPartition -> UserGroupID
                                      -> m JSValue
handleAdminCompanyShareableLinksStats statsPartition ugid = onlySalesOrAdmin $ do
  getShareableLinksStats statsPartition (UsageStatsForUserGroup ugid)


handleCompanyGetSubscription :: Kontrakcja m => UserGroupID -> m Aeson.Value
handleCompanyGetSubscription ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  unjsonToJSON unjsonDef <$> getSubscription ugwp

handleCompanyUpdateSubscription :: Kontrakcja m => UserGroupID -> m Response
handleCompanyUpdateSubscription ugid = onlySalesOrAdmin . V2.api $ do
  ugwp <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  subscription <- apiV2ParameterObligatory (ApiV2ParameterJSON "subscription" unjsonDef)

  let newInvoicing =
        case (ugSubInvoicingType subscription, ugSubPaymentPlan subscription) of
          (InvoicingTypeNone, _) -> None
          (InvoicingTypeBillItem, mpp@_) -> BillItem mpp
          (InvoicingTypeInvoice, Just pp) -> Invoice pp
          (InvoicingTypeInvoice, Nothing) -> unexpectedError "payment plan missing for Invoice type"
      newFeaturesIsInherited = ugSubFeaturesIsInherited subscription
      mNewFeatures = ugSubFeatures subscription
      mInheritedFeatures = ugwpFeatures <$> ugwpOnlyParents ugwp
      setFeatures = case (newFeaturesIsInherited, mNewFeatures, mInheritedFeatures) of
        (True , _, Just _) -> set ugFeatures Nothing
        (False, Just newFeatures, _) -> set ugFeatures $ Just newFeatures
        _ -> unexpectedError "invalid combination of features and inheriting"

  dbUpdate . UserGroupUpdate . set ugInvoicing newInvoicing . setFeatures . ugwpUG $ ugwp
  return $ V2.Accepted ()

handleCompanyGetStructure :: Kontrakcja m => UserGroupID -> m Aeson.Value
handleCompanyGetStructure ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM . dbQuery . UserGroupGetWithParents $ ugid
  let root = ugwpRoot ugwp
  children <- dbQuery . UserGroupGetAllChildrenRecursive $ get ugID root
  return $ object
    [ "user_group_structure" .= (ugWithChildrenToJson $ UserGroupWithChildren root children) ]
  where
    ugWithChildrenToJson (UserGroupWithChildren ug children) = object [
        "group" .= object
          [ "name" .= get ugName ug
          , identifier $ get ugID ug
          ]
      , "children" .= map ugWithChildrenToJson children
      ]

handleTriggerMigrateDocuments :: Kontrakcja m => Integer -> m Aeson.Value
handleTriggerMigrateDocuments limit = onlyAdmin $ do
  logInfo_ "Starting migration batch for documents"
  let limitWithUpperBound = minimum [limit, 10000]
  startTime <- liftIO currentTime
  docAndFdrs :: [(DocumentID, FolderID)] <- do
    runQuery_ . sqlSelect "documents d" $ do
      sqlJoinOn "users u" "u.id = d.author_user_id"
      sqlResult "d.id"
      sqlResult "u.home_folder_id"
      sqlWhereIsNULL "d.folder_id"
      sqlLimit limitWithUpperBound
    fetchMany id

  (results :: [Either SomeException ()]) <- do
   forM docAndFdrs $ \(did, fdrid) ->
     try $ (withDocumentID did (dbUpdate $ AddDocumentToFolder fdrid))
  endTime <- liftIO currentTime
  return . object $
    [ "documents_linked" .= (length . rights $ results)
    , "limit_used" .= limitWithUpperBound
    , "elapsed_time" .= (realToFrac (diffUTCTime endTime startTime) :: Double)]

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
  endTime <- liftIO currentTime
  return . object $
    [ "home_folders_created" .= numberDone
    , "limit_used" .= limitWithUpperBound
    , "elapsed_time" .= (realToFrac (diffUTCTime endTime startTime) :: Double)]

jsonBrandedDomainsList ::Kontrakcja m => m Aeson.Value
jsonBrandedDomainsList = onlySalesOrAdmin $ do
    allBrandedDomains <- dbQuery $ GetBrandedDomains
    return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonBrandedDomainsList allBrandedDomains

jsonBrandedDomain :: Kontrakcja m => BrandedDomainID -> m Aeson.Value
jsonBrandedDomain bdID = onlySalesOrAdmin $ do
  bd <- dbQuery $ GetBrandedDomainByID bdID
  return $ Unjson.unjsonToJSON' (Options { pretty = True, indent = 2, nulls = True }) unjsonBrandedDomain bd

updateBrandedDomain :: Kontrakcja m => BrandedDomainID -> m ()
updateBrandedDomain xbdid = onlySalesOrAdmin $ do
    obd <- dbQuery $ GetBrandedDomainByID xbdid
    when (get bdMainDomain obd) $ do
      logInfo_ "Main domain can't be changed"
      internalError
    -- keep this 1to1 consistent with fields in the database
    domainJSON <- guardJustM $ getFieldBS "domain"
    case Aeson.eitherDecode $ domainJSON of
     Left err -> do
      logInfo "Error while parsing branding for adminonly" $ object [
          "error" .= err
        ]
      internalError
     Right js -> case (Unjson.parse unjsonBrandedDomain js) of
        (Result newDomain []) -> do
          void $ dbUpdate $ UpdateBrandedDomain $
                 copy bdid obd $
                 copy bdMainDomain obd $
                 newDomain
          return ()
        _ -> internalError

unjsonBrandedDomain :: UnjsonDef BrandedDomain
unjsonBrandedDomain = objectOf $ pure BrandedDomain
  <*> field "id"
      (get bdid)
      "Id of a branded domain (unique)"
  <*> field "mainDomain"
      (get bdMainDomain)
      "Is this a main domain"
  <*> field "url"
      (get bdUrl)
      "URL that will match this domain"
  <*> field "smsOriginator"
      (get bdSmsOriginator)
      "Originator for text messages"
  <*> field "emailOriginator"
      (get bdEmailOriginator)
      "Originator for email messages"
  <*> field "mailTheme"
      (get bdMailTheme)
      "Email theme"
  <*> field "signviewTheme"
      (get bdSignviewTheme)
      "Signview theme"
  <*> field "serviceTheme"
      (get bdServiceTheme)
      "Service theme"
  <*> field "loginTheme"
      (get bdLoginTheme)
      "Login theme"
  <*> field "browserTitle"
      (get bdBrowserTitle)
      "Browser title"
  <*> fieldBy "favicon"
      (get bdFavicon)
      "Favicon"
       (invmap
          (\l -> B64.decodeLenient $ BSC8.pack $  drop 1 $ dropWhile ((/=) ',') l)
          (\l -> BSC8.unpack $ BS.append (BSC8.pack "data:image/png;base64,") $ B64.encode l)
          unjsonDef
       )
   <*> field "participantColor1"
      (get bdParticipantColor1)
      "Participant 1 color"
   <*> field "participantColor2"
      (get bdParticipantColor2)
      "Participant 2 color"
   <*> field "participantColor3"
      (get bdParticipantColor3)
      "Participant 3 color"
   <*> field "participantColor4"
      (get bdParticipantColor4)
      "Participant 4 color"
   <*> field "participantColor5"
      (get bdParticipantColor5)
      "Participant 5 color"
   <*> field "participantColor6"
      (get bdParticipantColor6)
      "Participant 6 color"
   <*> field "draftColor"
      (get bdDraftColor)
      "Draft color"
   <*> field "cancelledColor"
      (get bdCancelledColor)
      "Cancelled color"
   <*> field "initatedColor"
      (get bdInitatedColor)
      "Initated color"
   <*> field "sentColor"
      (get bdSentColor)
      "Sent color"
   <*> field "deliveredColor"
      (get bdDeliveredColor)
      "Delivered color"
   <*> field "openedColor"
      (get bdOpenedColor)
      "Opened color"
   <*> field "reviewedColor"
      (get bdReviewedColor)
      "Reviewed color"
   <*> field "signedColor"
      (get bdSignedColor)
      "Signed color"

unjsonBrandedDomainsList :: UnjsonDef [BrandedDomain]
unjsonBrandedDomainsList = objectOf $
  fieldBy "domains"
  id
  "List of branded domains"
  (arrayOf unjsonBrandedDomain)


createBrandedDomain :: Kontrakcja m => m JSValue
createBrandedDomain = do
    bdID <- dbUpdate $ NewBrandedDomain
    runJSONGenT $ do
      value "id" (show bdID)
