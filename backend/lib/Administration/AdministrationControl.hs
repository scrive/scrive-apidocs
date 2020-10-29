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
          , adminonlyOldRoutes
          , daveRoutes
          , showAdminMainPage
          , jsonCompanies -- for tests
          , handleCompanyChange -- for tests
          , handleCompanyCreate -- for tests
          , CompanyCreateResult(..) -- for tests
          ) where

import Data.Aeson.Types
import Data.Functor.Invariant
import Data.Int (Int16, Int32)
import Data.Unjson
import Happstack.Server hiding (badRequest, dir, https, path, simpleHTTP)
import Happstack.StaticRouting (Route, choice, dir, param, remainingPath)
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
import AppView (entryPointFields, renderFromBody, simpleHtmlResponse)
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import DataRetentionPolicy.Guards
import DB
import DigitalSignatureMethod
import Doc.Action (postDocumentClosedActions, postDocumentPendingChange)
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
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
import FeatureFlags.Model (defaultFeatures)
import File.Model
import File.Storage
import File.Types
import Folder.Model
import Happstack.Fields
import InputValidation
import InspectXML
import InspectXMLInstances
import IPAddress ()
import Kontra
import KontraLink
import Log.Identifier
import LoginAuth.LoginAuthMethod
import Mails.Model
import MinutesTime
import PadApplication.Types (padAppModeFromText)
import Routing
import Session.Constant
import Session.Model
import Tag
import Templates (renderTextTemplate)
import Theme.Control
import User.CallbackScheme.Model
import User.Email
import User.History.Model
import User.JSON
import User.UserControl
import User.Utils
import UserGroup.DeletionRequest.Model
import UserGroup.DeletionRequest.Types
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import UserGroup.Types.Subscription
import UserGroupAccounts.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.PDFUtil
import Util.SignatoryLinkUtils
import Utils.Monoid
import qualified API.V2 as V2
import qualified Company.CompanyControl as Company
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified UserGroupAccounts.UserGroupAccountsControl as UserGroupAccounts

adminonlyRoutes :: Route (Kontra Response)
adminonlyRoutes = onlySalesOrAdmin <$> choice [htmlRoutes, jsonRoutes]
  where
    htmlRoutes = choice
      [hGet showAdminElmMainPage, (dir "page" . remainingPath GET) showAdminElmMainPage]
    jsonRoutes = choice
      [ (dir "userslist" . hGet . toApiV2K0 V2.Ok) jsonUsersList
      , (dir "useradmin" . dir "details" . hGet . toApiV2K1 V2.Ok) handleUserGetProfile
      , (dir "useradmin" . hPost . toApiV2K1 V2.Ok) handleUserChange
      , (dir "useradmin" . dir "changepassword" . hPost . toApiV2K1 V2.Ok)
        handleUserPasswordChange
      , (dir "useradmin" . dir "deleteinvite" . hPost . toApiV2K2 V2.Ok)
        handleDeleteInvite
      , (dir "useradmin" . dir "delete" . hPost . toApiV2K1 V2.Ok) handleDeleteUser
      , (dir "useradmin" . dir "move" . hPost . toApiV2K1 V2.Ok)
        handleMoveUserToDifferentCompany
      , (dir "useradmin" . dir "disable2fa" . hPost . toApiV2K1 V2.Ok)
        handleDisable2FAForUser
      , (dir "useradmin" . dir "usagestats" . dir "days" . hGet . toApiV2K1 V2.Ok)
        handleAdminUserUsageStatsDays
      , (dir "useradmin" . dir "usagestats" . dir "months" . hGet . toApiV2K1 V2.Ok)
        handleAdminUserUsageStatsMonths
      , (dir "useradmin" . dir "shareablelinkstats" . dir "days" . hGet . toApiV2K1 V2.Ok)
        $ handleAdminUserShareableLinksStats PartitionByDay
      , ( dir "useradmin"
        . dir "shareablelinkstats"
        . dir "months"
        . hGet
        . toApiV2K1 V2.Ok
        )
        $ handleAdminUserShareableLinksStats PartitionByMonth
      , (dir "useradmin" . dir "sendinviteagain" . hPost . toK0 . toApiV2K0 V2.Ok)
        sendInviteAgain
      , (dir "companyadmin" . hPost . toK0 . toApiV2K0 V2.Created) handleCompanyCreate
      , (dir "companyadmin" . hPost . toApiV2K1 V2.Ok) handleCompanyChange
      , (dir "companyadmin" . dir "details" . hGet . toApiV2K1 V2.Ok)
        handleCompanyGetProfile
      , (dir "companyadmin" . dir "merge" . hPost . toApiV2K1 V2.Ok)
        handleMergeToOtherCompany
      , (dir "companyadmin" . dir "branding") Company.adminRoutes
      , (dir "companyadmin" . dir "users" . hPost . toApiV2K1 V2.Created)
        handleCreateCompanyUser
      , (dir "companyaccounts" . hGet . toApiV2K1 V2.Ok)
        UserGroupAccounts.handleUserGroupAccountsForAdminOnly
      , (dir "companyadmin" . dir "usagestats" . dir "days" . hGet . toApiV2K1 V2.Ok)
        handleAdminCompanyUsageStatsDays
      , (dir "companyadmin" . dir "usagestats" . dir "months" . hGet . toApiV2K1 V2.Ok)
        handleAdminCompanyUsageStatsMonths
      , ( dir "companyadmin"
        . dir "shareablelinkstats"
        . dir "days"
        . hGet
        . toApiV2K1 V2.Ok
        )
        $ handleAdminCompanyShareableLinksStats PartitionByDay
      , ( dir "companyadmin"
        . dir "shareablelinkstats"
        . dir "months"
        . hGet
        . toApiV2K1 V2.Ok
        )
        $ handleAdminCompanyShareableLinksStats PartitionByMonth
      , (dir "companyadmin" . dir "getsubscription" . hGet . toApiV2K1 V2.Ok)
        handleCompanyGetSubscription
      , (dir "companyadmin" . dir "updatesubscription" . hPost . toApiV2K1 V2.Ok)
        handleCompanyUpdateSubscription
      , (dir "companyadmin" . dir "getstructure" . hGet . toApiV2K1 V2.Ok)
        handleCompanyGetStructure
      , (dir "companyadmin" . dir "requestdeletion" . hPost . toApiV2K1 V2.Created)
        handleRequestUserGroupDeletion
      , (dir "companyadmin" . dir "signoffdeletion" . hPost . toApiV2K1 V2.Ok)
        handleSignOffUserGroupDeletion
      , (dir "companyadmin" . dir "canceldeletion" . hPost . toApiV2K1 V2.Ok)
        handleCancelUserGroupDeletion
      , (dir "documentslist" . hGet . toK0 . toApiV2K0 V2.Ok) jsonDocuments
      , (dir "companies" . hGet . toK0 . toApiV2K0 V2.Ok) jsonCompanies
      , (dir "brandeddomainslist" . hGet . toK0 . toApiV2K0 V2.Ok) jsonBrandedDomainsList
      , (dir "brandeddomain" . dir "create" . hPost . toK0 . toApiV2K0 V2.Ok)
        createBrandedDomain
      , (dir "brandeddomain" . dir "details" . hGet . toApiV2K1 V2.Ok) jsonBrandedDomain
      , (dir "brandeddomain" . dir "details" . dir "change" . hPost . toApiV2K1 V2.Ok)
        updateBrandedDomain
      , (dir "brandeddomain" . dir "themes" . hGet . toApiV2K1 V2.Ok)
        handleGetThemesForDomain
      , (dir "brandeddomain" . dir "newtheme" . hPost . toApiV2K2 V2.Created)
        handleNewThemeForDomain
      , (dir "brandeddomain" . dir "updatetheme" . hPost . toApiV2K2 V2.Ok)
        handleUpdateThemeForDomain
      , (dir "brandeddomain" . dir "deletetheme" . hPost . toApiV2K2 V2.Ok)
        handleDeleteThemeForDomain
      ]

    toApiV2K0
      :: (Kontrakcja m, V2.ToAPIResponse a)
      => (a -> V2.APIResponse a)
      -> m a
      -> m Response
    toApiV2K0 apiResponse handler = V2.api (apiResponse <$> handler)

    toApiV2K1
      :: (Kontrakcja m, V2.ToAPIResponse b)
      => (b -> V2.APIResponse b)
      -> (a -> m b)
      -> a
      -> m Response
    toApiV2K1 apiResponse handler a = V2.api (apiResponse <$> handler a)

    toApiV2K2
      :: (Kontrakcja m, V2.ToAPIResponse c)
      => (c -> V2.APIResponse c)
      -> (a -> b -> m c)
      -> a
      -> b
      -> m Response
    toApiV2K2 apiResponse handler a b = V2.api (apiResponse <$> handler a b)

adminonlyOldRoutes :: Route (Kontra Response)
adminonlyOldRoutes = onlySalesOrAdmin <$> choice
  [ hGet $ toK0 showAdminMainPage
  , dir "useradmin" . hGet $ toK1 showAdminUsers
  , dir "companyadmin" . hGet $ toK1 showAdminCompany
  ]

daveRoutes :: Route (Kontra Response)
daveRoutes = onlyAdmin <$> choice
  [ dir "document" . hGet $ toK1 daveDocument
  , dir "document" . hGet $ toK2 daveSignatoryLink
  , dir "document" . param $ dir "transfer" (hPost $ toK1 handleTransferDocument)
  , dir "user" . hGet $ toK1 daveUser
  , dir "userhistory" . hGet $ toK1 daveUserHistory
  , dir "usergroup" . hGet $ toK1 daveUserGroup
  , dir "reseal" . hPost $ toK1 resealFile
  , dir "reflatten" . hPost $ toK1 reflattenFile
  , dir "postpending" . hPost $ toK1 triggerPostPending
  , dir "file" . hGet $ toK2 daveFile
  , dir "backdoor" $ hGet handleBackdoorQuery
  , dir "randomscreenshot" . hGet $ toK0 randomScreenshotForTest
  ]
{- | Main page. Redirects users to other admin panels -}

showAdminMainPage :: Kontrakcja m => m String
showAdminMainPage = onlySalesOrAdmin $ do
  ctx <- getContext
  adminMainPage ctx

{- | Main page. Redirects users to other admin panels -}
showAdminElmMainPage :: Kontrakcja m => m Response
showAdminElmMainPage = onlySalesOrAdmin $ do
  ctx  <- getContext
  page <- adminElmMainPage ctx
  simpleHtmlResponse $ T.pack page

{- | Process view for finding a user in basic administration -}
showAdminUsers :: Kontrakcja m => UserID -> m String
showAdminUsers uid = onlySalesOrAdmin $ do
  ctx <- getContext
  adminUserPage ctx uid

handleUserGetProfile :: Kontrakcja m => UserID -> m JSValue
handleUserGetProfile uid = onlySalesOrAdmin $ do
  user     <- guardJustM . dbQuery $ GetUserByID uid
  callback <- dbQuery $ GetUserCallbackSchemeByUserID uid
  ugwp     <- dbQuery . UserGroupGetWithParentsByUserID $ uid
  return $ userJSONWithCallBackInfo user ugwp callback

handleCompanyGetProfile :: Kontrakcja m => UserGroupID -> m JSValue
handleCompanyGetProfile ugid = onlySalesOrAdmin $ do
  ugwp             <- guardJustM . dbQuery $ UserGroupGetWithParents ugid
  mDeletionRequest <- dbQuery $ GetUserGroupDeletionRequest ugid
  return $ companyJSONAdminOnly mDeletionRequest ugwp

-- | Full scrive admins (not mere sales admins) can request the deletion of
-- arbitrary user groups. The request expires after 1 day unless signed off by
-- another scrive admin.
handleRequestUserGroupDeletion :: Kontrakcja m => UserGroupID -> m JSValue
handleRequestUserGroupDeletion requestedFor = onlyAdmin $ do
  requestedBy           <- guardJustM $ preview (#maybeUser % _Just % #id) <$> getContext
  requestedDeletionDate <- guardJustM $ readField "deletion_date"
  expires               <- Just . (1 `daysAfter`) <$> currentTime
  let signedOffBy = Nothing
  toJSValue <$> dbUpdate (UserGroupCreateDeletionRequest UserGroupDeletionRequest { .. })

-- | Full scrive admins (not mere sales admins) can remove existing deletion
-- requests for any user group; they needn't have requested the deletion
-- themselves in the first place.
handleCancelUserGroupDeletion :: Kontrakcja m => UserGroupID -> m ()
handleCancelUserGroupDeletion requestedFor = onlyAdmin $ do
  dbUpdate $ DeleteUserGroupDeletionRequest requestedFor

-- | A second scrive admin (not mere sales admin) needs to sign off on a
-- deletion request. A signed off deletion request will cause the user group and
-- any contained users to be deleted on the requested deletion time, unless the
-- request is withdrawn ('unrequested') before that time.
handleSignOffUserGroupDeletion :: Kontrakcja m => UserGroupID -> m JSValue
handleSignOffUserGroupDeletion requestedFor = onlyAdmin $ do
  signedOffBy <- guardJustM $ preview (#maybeUser % _Just % #id) <$> getContext
  toJSValue <$> dbUpdate (UserGroupSignOffDeletion requestedFor signedOffBy)

showAdminCompany :: Kontrakcja m => UserGroupID -> m Text
showAdminCompany ugid = onlySalesOrAdmin $ do
  ctx <- getContext
  T.pack <$> adminCompanyPage ctx ugid

jsonCompanies :: Kontrakcja m => m JSValue
jsonCompanies = onlySalesOrAdmin $ do
  softLimit  <- guardJustM $ readField "limit"
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
  (totalMatching, ugs) <- dbQuery $ UserGroupsGetFiltered
    (textFilter <> usersFilter <> pplanFilter)
    (Just (offset, 10000, softLimit))
  -- get address for those companies, which inherit it
  ugsWithAddress <- forM ugs $ \ug -> case ug ^. #address of
    Just uga -> return (ug, uga)
    Nothing ->
      (ug, )
        .   ugwpAddress
        <$> (guardJustM . dbQuery . UserGroupGetWithParents $ ug ^. #id)
  runJSONGenT $ do
    value "total_matching" totalMatching
    valueM "companies" . forM ugsWithAddress $ \(ug, uga) -> runJSONGenT $ do
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
  softLimit  <- guardJustM $ readField "limit"
  offset     <- guardJustM $ readField "offset"
  textFilter <- getField "text" >>= \case
    Nothing -> return []
    Just s  -> return [UserFilterByString $ T.unpack s]
  sorting <- getField "tosSorting" >>= \case
    Just "ascending"  -> return [Asc UserOrderByAccountCreationDate]
    Just "descending" -> return [Desc UserOrderByAccountCreationDate]
    _                 -> return [Asc UserOrderByName]
  (totalMatching, users) <- dbQuery
    $ GetUsersWithUserGroupNames textFilter sorting (offset, 10000, softLimit)

  runJSONGenT $ do
    value "total_matching" totalMatching
    valueM "users" . forM users $ \(user, ugname, entityName) -> runJSONGenT $ do
      value "id" . show $ user ^. #id
      value "username" . T.unpack $ getFullName user
      value "email" . T.unpack $ getEmail user
      value "companyposition" . T.unpack $ user ^. #info % #companyPosition
      value "company" . T.unpack $ entityName
      value "usergroup" . T.unpack $ ugname
      value "phone" . T.unpack $ user ^. #info % #phone
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
          dbUpdate $ UpdateUserCallbackScheme uid (ConstantUrlSchemeV2 newUrl)
    Just _ -> return () -- Do not allow changing the callback if an existing other type is there

  -- Set whether 2FA is mandatory
  maybeNewTotpIsMandatory <- getField "usertotpismandatory"
  void . dbUpdate . SetUserTotpIsMandatory uid $ Just "true" == maybeNewTotpIsMandatory


  -- Set the user authentication
  maybeNewAuthentication <- getAuth "userauth"
  whenJust maybeNewAuthentication $ void . dbUpdate . SetLoginAuth uid

  museraccounttype <- getField "useraccounttype"
  olduser          <- guardJustM . dbQuery $ GetUserByID uid
  user             <- case (museraccounttype, olduser ^. #isCompanyAdmin) of
    (Just "companyadminaccount", False) -> do
      --then we just want to make this account an admin
      guardJustM $ do
        void . dbUpdate $ SetUserCompanyAdmin uid True
        void . dbUpdate $ LogHistoryDetailsChanged
          uid
          (ctx ^. #ipAddr)
          (ctx ^. #time)
          [("is_company_admin", "false", "true")]
          (ctx ^? #maybeUser % _Just % #id)
        dbQuery $ GetUserByID uid
    (Just "companystandardaccount", True) -> do
      --then we just want to downgrade this account to a standard
      guardJustM $ do
        void . dbUpdate $ SetUserCompanyAdmin uid False
        void . dbUpdate $ LogHistoryDetailsChanged
          uid
          (ctx ^. #ipAddr)
          (ctx ^. #time)
          [("is_company_admin", "true", "false")]
          (ctx ^? #maybeUser % _Just % #id)
        dbQuery $ GetUserByID uid
    _ -> return olduser
  infoChange     <- getUserInfoChange
  internalTagOps <- fromMaybe [] <$> getFieldTags "userinternaltags"
  externalTagOps <- fromMaybe [] <$> getFieldTags "companyexternaltags"
  let applyChanges = do
        void . dbUpdate $ SetUserInfo uid (infoChange $ user ^. #info)
        void . dbUpdate $ LogHistoryUserInfoChanged uid
                                                    (ctx ^. #ipAddr)
                                                    (ctx ^. #time)
                                                    (user ^. #info)
                                                    (infoChange $ user ^. #info)
                                                    (ctx ^? #maybeUser % _Just % #id)
        settingsChange <- getUserSettingsChange
        void . dbUpdate $ SetUserSettings uid (settingsChange $ user ^. #settings)
        void $ dbUpdate SetUserTags
          { userID       = user ^. #id
          , internalTags = updateTags (user ^. #internalTags) internalTagOps
          , externalTags = updateTags (user ^. #externalTags) externalTagOps
          }
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
  user         <- guardJustM . dbQuery $ GetUserByID uid
  password     <- guardJustM $ getField "password"
  passwordhash <- createPassword password
  ctx          <- getContext
  let time     = ctx ^. #time
      ipnumber = ctx ^. #ipAddr
      admin    = ctx ^. #maybeUser
  void . dbUpdate $ SetUserPassword (user ^. #id) passwordhash
  void . dbUpdate $ LogHistoryPasswordSetup (user ^. #id)
                                            ipnumber
                                            time
                                            (view #id <$> admin)
  terminateAllUserSessionsExceptCurrent (user ^. #id)
  runJSONGenT $ value "changed" True

handleDeleteInvite :: Kontrakcja m => UserGroupID -> UserID -> m ()
handleDeleteInvite ugid uid = onlySalesOrAdmin $ do
  void . dbUpdate $ RemoveUserGroupInvite [ugid] uid

handleDeleteUser :: Kontrakcja m => UserID -> m ()
handleDeleteUser uid = onlySalesOrAdmin $ do
  void . dbUpdate $ RemoveUserUserGroupInvites uid
  void . dbUpdate $ DeleteUserCallbackScheme uid
  void . dbUpdate $ DeleteUser uid

handleDisable2FAForUser :: Kontrakcja m => UserID -> m ()
handleDisable2FAForUser uid = onlySalesOrAdmin $ do
  ctx  <- getContext
  user <- guardJustM . dbQuery $ GetUserByID uid
  when (user ^. #totpActive) $ do
    r <- dbUpdate $ DisableUserTOTP uid
    if r
      then void . dbUpdate $ LogHistoryTOTPDisable uid (ctx ^. #ipAddr) (ctx ^. #time)
      else internalError

handleMoveUserToDifferentCompany :: Kontrakcja m => UserID -> m ()
handleMoveUserToDifferentCompany userID = onlySalesOrAdmin $ do
  targetUserGroupID <- guardJustM $ readField "companyid"
  user              <- guardJustM . dbQuery $ GetUserByID userID
  void $ moveUserToUserGroupWithDocuments user targetUserGroupID

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
      (view #id <$>) <$> dbQuery (FolderGetUserGroupHome ugid_target) >>= \case
        Nothing          -> internalError
        Just targetfdrid -> do
          users <- dbQuery $ UserGroupGetUsers ugid_source
          forM_ users $ \u -> do
            void . dbUpdate $ SetUserUserGroup (u ^. #id) ugid_target
            let newhomefdr = set #parentID (Just targetfdrid) defaultFolder
            newhomefdrid <- view #id <$> dbUpdate (FolderCreate newhomefdr)
            logInfo "Changing user home folder id" $ object
              [ "user_id" .= (u ^. #id)
              , "folder_id_new" .= newhomefdrid
              , "folder_id_old" .= (u ^. #homeFolderID)
              , "change_source" .= ("handleMergeToOtherCompany" :: Text)
              ]
            void . dbUpdate . SetUserHomeFolder (u ^. #id) $ newhomefdrid
            case u ^. #homeFolderID of
              Just sourceFolderId -> do
                void . dbUpdate $ MoveAuthorDocuments (u ^. #id)
                                                      sourceFolderId
                                                      newhomefdrid
                logInfo "Moving user's authored documents" $ object
                  [ "user_id" .= (u ^. #id)
                  , "folder_id_new" .= newhomefdrid
                  , "folder_id_old" .= sourceFolderId
                  , "change_source" .= ("handleMergeToOtherCompany" :: Text)
                  ]
              Nothing -> do
                logInfo "Not moving user's authored documents"
                  $ object
                      [ "user_id" .= (u ^. #id)
                      , "change_source" .= ("handleMergeToOtherCompany" :: Text)
                      ]
                return ()
          invites <- dbQuery $ UserGroupGetInvites ugid_source
          forM_ invites $ \i ->
            void . dbUpdate $ RemoveUserGroupInvite [ugid_source] (inviteduserid i)

{- | Handling company details change. It reads user info change -}
handleCompanyChange :: Kontrakcja m => UserGroupID -> m ()
handleCompanyChange ugid = onlySalesOrAdmin $ do
  ugwp <- guardJustM . dbQuery $ UserGroupGetWithParents ugid
  logInfo "ugwp" $ object ["ugwp" .= showt ugwp]
  mCompanyName           <- getField "companyname"
  mUGSettingsIsInherited <- fmap (== ("true" :: Text))
    <$> getField "companysettingsisinherited"
  ugSettingsChange      <- getUserGroupSettingsChange
  mUGAddressIsInherited <- fmap (== ("true" :: Text))
    <$> getField "companyaddressisinherited"
  ugAddressChange       <- getUserGroupAddressChange
  mInternalTagOps       <- getFieldTags "companyinternaltags"
  mExternalTagOps       <- getFieldTags "companyexternaltags"
  mTryParentUserGroupID <- getOptionalField asValidUserGroupID "companyparentid"

  let
    oldUg       = ugwpUG ugwp
    setSettings = if fromMaybe (isNothing $ oldUg ^. #settings) mUGSettingsIsInherited
      then set #settings Nothing
      else set #settings . Just . ugSettingsChange $ ugwpSettings ugwp
    setAddress = if fromMaybe (isNothing $ oldUg ^. #address) mUGAddressIsInherited
      then set #address Nothing
      else set #address . Just . ugAddressChange $ ugwpAddress ugwp
    -- Set invoicing to None and payment plan to inherit when moving
    -- root user group (legacy user groups without billable flag).
    setInvoicing = if isNothing (oldUg ^. #parentGroupID) && isJust mTryParentUserGroupID
      then set #invoicing None
      else identity
    updateInternalTags = set #internalTags . updateTags (oldUg ^. #internalTags)
    updateExternalTags = set #externalTags . updateTags (oldUg ^. #externalTags)
    newUg =
      set #parentGroupID mTryParentUserGroupID
        . maybe identity (set #name)        mCompanyName
        . maybe identity updateInternalTags mInternalTagOps
        . maybe identity updateExternalTags mExternalTagOps
        . setSettings
        . setAddress
        . setInvoicing
        $ ugwpUG ugwp

  guardUserGroupIsBillable oldUg newUg

  newSettings <-
    guardJust
    . listToMaybe
    . catMaybes
    $ [newUg ^. #settings, ugwpSettings <$> ugwpOnlyParents ugwp]
  logInfo "newsettings" $ object ["newsettings" .= showt newSettings]
  guardThatDataRetentionPolicyIsValid (newSettings ^. #dataRetentionPolicy) Nothing
  dbUpdate $ UserGroupUpdate newUg
  return ()

getFieldTags :: Kontrakcja m => Text -> m (Maybe [TagUpdate])
getFieldTags fieldName = do
  mValue <- getFieldBS fieldName
  case mValue of
    Nothing  -> return Nothing
    Just val -> case Aeson.eitherDecode val of
      Right res -> return $ Just res
      Left  err -> do
        logInfo "Error while parsing tags" $ object ["field" .= fieldName, "error" .= err]
        internalError

data CompanyCreateResult = CompanyCreateSuccess
  { groupId :: UserGroupID
  , mRootGroupId :: Maybe UserGroupID
  }
  deriving (Show)

instance ToJSON CompanyCreateResult where
  toJSON CompanyCreateSuccess {..} = object
    [ "success" .= True
    , "user_group_id" .= groupId
    , "root_user_group_id" .= mRootGroupId
    , "error_message" .= (Nothing :: Maybe Text)
    ]

instance V2.ToAPIResponse CompanyCreateResult

-- | User group creation handler
-- There are following cases which are allowed:
--
-- 1. Neither "user_group_parent_id" nor "user_group_child_id" are set and "payment_plan" is set to `free`.
--    - We're creating only single user group with free payment plan without billable flag.
-- 2. Neither "user_group_parent_id" nor "user_group_child_id" are set and "payment_plan" is set to anything else than `free`.
--    - In this case, we are creating subscription user group structure.
--      Root user group has billable flag, paid subscription type, invoicing set to Invoice
--      and single child user group which is place for users.
--      Child group has it's own feature flags (does not inherit) set same as for free account,
--      inherits parent's subscription and invoicing is set to None.
-- 3. Only "user_group_parent_id" is set.
--    - We are just creating child user group. In case it's direct child of user group with
--      billable flag, same restriction applies as in first case in regards to features.
-- 4. Only "user_group_child_id" is set and "payment_plan" is set to anything else than `free`.
--    - We are trying to add root user group with billable flag (upgrade of legacy/free user groups).
--      Root user group is created and assigned as parent user group.
--      Child user group's subscription is set to inherit and invoicing is set to None.
--      Though in this case, we don't set feature flags as the child group already has explicit ones
--      end we don't want to mess with those.
handleCompanyCreate :: Kontrakcja m => m CompanyCreateResult
handleCompanyCreate = onlySalesOrAdmin $ do
  ugName       <- apiV2ParameterObligatory $ ApiV2ParameterText "user_group_name"
  mParentUgId  <- apiV2ParameterOptional $ ApiV2ParameterRead "user_group_parent_id"
  mChildUgId   <- apiV2ParameterOptional $ ApiV2ParameterRead "user_group_child_id"
  mPaymentPlan <- apiV2ParameterOptional
    (ApiV2ParameterTextUnjson "payment_plan" unjsonDef)
  (mRootUgId, ugId, logMsg) <- case (mParentUgId, mChildUgId, mPaymentPlan) of
    (Nothing, Nothing, Nothing) ->
      V2.apiError
        $ requestFailed
            "Either \"payment_plan\", \"user_group_parent_id\" or \"user_group_child_id\" must be present."
    (Just _, Just _, _) ->
      V2.apiError
        $ requestFailed
            "Both \"user_group_parent_id\" and \"user_group_child_id\" fields can't be present at the same time."
    -- Case #1 of description - new free group
    (Nothing, Nothing, Just FreePlan) -> do
      freeUg <- createFreeUserGroup ugName
      pure (Nothing, freeUg ^. #id, "Creating user group with free plan")
    -- Case #2 of description - new paid group
    (Nothing, Nothing, Just paymentPlan) -> do
      parentUg <- createRootUserGroup ("ROOT - " <> ugName) paymentPlan
      childUg  <- createChildGroup ugName parentUg
      pure (Just $ parentUg ^. #id, childUg ^. #id, "Creating user group with paid plan")
    -- Case #3 of description - new child group
    (Just parentUgId, Nothing, Nothing) -> do
      parentUg <- getUserGroup parentUgId
      childUg  <- createChildGroup ugName parentUg
      pure (Nothing, childUg ^. #id, "Creating child user group")
    (Just _, Nothing, Just _) -> do
      V2.apiError $ requestFailed
        "\"payment_plan\" can't be present when \"user_group_parent_id\" field is set."
    -- Case #4 of description - new paid root group for free child group
    (Nothing, Just _, Just FreePlan) -> V2.apiError $ requestFailed
      "\"payment_plan\" can't be \"free\" when \"user_group_child_id\" field is set."
    (Nothing, Just childUgId, Just paymentPlan) -> do
      parentUg <- createRootUserGroup ugName paymentPlan
      childUg  <- updateChildGroup parentUg childUgId
      pure (Just $ parentUg ^. #id, childUg ^. #id, "Upgrading to paid user group")
    (Nothing, Just _, Nothing) ->
      V2.apiError $ requestFailed "\"payment_plan\" field is missing."
  logInfo logMsg $ object
    [ "user_group_name" .= ugName
    , "user_group_id" .= ugId
    , "parent_user_group_id" .= mParentUgId
    , "root_user_group_id" .= mRootUgId
    , "payment_plan" .= fmap showt mPaymentPlan
    ]
  pure CompanyCreateSuccess { groupId = ugId, mRootGroupId = mRootUgId }
  where
    getUserGroup :: Kontrakcja m => UserGroupID -> m UserGroup
    getUserGroup = guardJustM . dbQuery . UserGroupGet

    getUserGroupWithParents :: Kontrakcja m => UserGroupID -> m UserGroupWithParents
    getUserGroupWithParents = guardJustM . dbQuery . UserGroupGetWithParents

    createRootUserGroup :: Kontrakcja m => Text -> PaymentPlan -> m UserGroup
    createRootUserGroup ugName paymentPlan = do
      ugFolder <- dbUpdate . FolderCreate $ defaultFolder
      dbUpdate
        . UserGroupCreate
        . set #name         ugName
        . set #homeFolderID (Just $ ugFolder ^. #id)
        . set #isBillable   True
        . set #invoicing    (Invoice paymentPlan)
        . set #features (Just $ defaultFeatures paymentPlan)
        $ defaultUserGroup

    createFreeUserGroup :: Kontrakcja m => Text -> m UserGroup
    createFreeUserGroup ugName = do
      ugFolder <- dbUpdate . FolderCreate $ defaultFolder
      ug       <-
        dbUpdate
        . UserGroupCreate
        . set #name         ugName
        . set #homeFolderID (Just $ ugFolder ^. #id)
        $ defaultUserGroup
      freeDocumentsValidity <- (31 `daysAfter`) <$> currentTime
      let freeDocumentsCount = 3
          freeDocuments =
            freeDocumentTokensFromValues freeDocumentsCount freeDocumentsValidity
      dbUpdate $ UserGroupFreeDocumentTokensUpdate (ug ^. #id) freeDocuments
      pure ug

    createChildGroup :: Kontrakcja m => Text -> UserGroup -> m UserGroup
    createChildGroup ugName parentUg = do
      rootUg <- fmap ugwpRoot . getUserGroupWithParents $ parentUg ^. #id
      unless (rootUg ^. #isBillable) . V2.apiError $ requestFailed
        "Can't create child user group, unless root user group is billable."
      let setChildFeatures = if parentUg ^. #isBillable
            then set #features (Just $ defaultFeatures FreePlan)
            else identity
      ugFolder <- dbUpdate . FolderCreate $ defaultFolder
      dbUpdate
        . UserGroupCreate
        . set #name          ugName
        . set #homeFolderID  (Just $ ugFolder ^. #id)
        . set #parentGroupID (Just $ parentUg ^. #id)
        . setChildFeatures
        $ defaultChildUserGroup

    updateChildGroup :: Kontrakcja m => UserGroup -> UserGroupID -> m UserGroup
    updateChildGroup parentUg childUgId = do
      childUg <- getUserGroup childUgId
      when (childUg ^. #isBillable) . V2.apiError $ requestFailed
        "Can't upgrade billable user group."

      let newChildUg =
            childUg & set #parentGroupID (Just $ parentUg ^. #id) & set #invoicing None
      dbUpdate $ UserGroupUpdate newChildUg
      pure newChildUg

handleCreateCompanyUser :: Kontrakcja m => UserGroupID -> m JSValue
handleCreateCompanyUser ugid = onlySalesOrAdmin $ do
  email   <- getCriticalField asValidEmail "email"
  fstname <- fromMaybe "" <$> getOptionalField asValidName "fstname"
  sndname <- fromMaybe "" <$> getOptionalField asValidName "sndname"
  lang    <- guardJustM $ (langFromCode =<<) <$> getField "lang"
  admin   <- isFieldSet "iscompanyadmin"
  auth    <- fromMaybe LoginAuthNative <$> getAuth "sysauth"
  muser   <- createNewUserByAdmin email (fstname, sndname) (ugid, admin) lang auth
  runJSONGenT $ case muser of
    Nothing -> do
      value "success" False
      valueM "error_message" $ renderTemplate_ "flashMessageUserWithSameEmailExists"
    Just _ -> do
      value "success"       True
      value "error_message" (Nothing :: Maybe String)

getAuth :: (HasRqData m, ServerMonad m) => Text -> m (Maybe LoginAuthMethod)
getAuth name = (loginAuthMethodFromText =<<) <$> getField name

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
  mcompanyidledoctimeoutclosed    <- getIdleDocTimeoutField "companyidledoctimeoutclosed"
  mcompanyidledoctimeoutcanceled <- getIdleDocTimeoutField "companyidledoctimeoutcanceled"
  mcompanyidledoctimeouttimedout <- getIdleDocTimeoutField "companyidledoctimeouttimedout"
  mcompanyidledoctimeoutrejected <- getIdleDocTimeoutField "companyidledoctimeoutrejected"
  mcompanyidledoctimeouterror     <- getIdleDocTimeoutField "companyidledoctimeouterror"
  mcompanyimmediatetrash          <- getField "companyimmediatetrash"
  mcompanysmsprovider             <- fmap maybeRead <$> getField' $ "companysmsprovider"
  mcompanypadappmode <- fmap padAppModeFromText <$> getField' $ "companypadappmode"
  mcompanypadearchiveenabled      <- getField "companypadearchiveenabled"
  mcompanyforcehidepn             <- getField "companyforcehidepn"
  mcompanyusefolderlistcalls      <- getField "companyusefolderlistcalls"
  mcompanysendtimeoutnotification <- getField "companysendtimeoutnotification"
  mcompanytotpismandatory         <- getField "companytotpismandatory"
  mcompanysessiontimeout          <- getSessionTimeoutField "companysessiontimeout"
  mcompanyportalurl               <- fmap emptyToNothing <$> getField "companyportalurl"
  mcompanyeidservicetoken <- fmap emptyToNothing <$> getField "companyeidservicetoken"
  mcompanysealingmethod           <-
    fmap digitalSignatureMethodFromText <$> getField' $ "companysealingmethod"
  mcompanydocumentsessiontimeout <- getSessionTimeoutField "companydocumentsessiontimeout"
  mcompanyhaspostsignview         <- getField "companyhaspostsignview"
  mcompanyeiduseforseview         <- getField "companyeiduseforseview"
  mcompanyappfrontend             <- getField "companyappfrontend"
  mcompanysebankidsigningoverride <- do
    mstr <- getField "companysebankidsigningoverride"
    return $ case mstr of
      Just "force_cgi"    -> Just $ Just ForceCGIForSEBankIDSigning
      Just "force_eidhub" -> Just $ Just ForceEIDHubForSEBankIDSigning
      Just "no_override"  -> Just Nothing
      _                   -> Nothing
  mcompanypadescredentialslabel <- fmap emptyToNothing
    <$> getField "companypadescredentialslabel"

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
    . maybe identity (set #cgiServiceID)              mcompanycgiserviceid
    . maybe identity (set #smsProvider)               mcompanysmsprovider
    . maybe identity (set #padAppMode)                mcompanypadappmode
    . maybe identity (set #forceHidePN . (== "true")) mcompanyforcehidepn
    . maybe identity (set #useFolderListCalls . (== "true")) mcompanyusefolderlistcalls
    . maybe identity (set #padEarchiveEnabled . (== "true")) mcompanypadearchiveenabled
    . maybe identity
            (set #sendTimeoutNotification . (== "true"))
            mcompanysendtimeoutnotification
    . maybe identity (set #totpIsMandatory . (== "true")) mcompanytotpismandatory
    . maybe identity (set #sessionTimeoutSecs)            mcompanysessiontimeout
    . maybe identity (set #portalUrl)                     mcompanyportalurl
    . maybe identity (set #eidServiceToken)               mcompanyeidservicetoken
    . maybe identity (set #digitalSignatureMethod)        mcompanysealingmethod
    . maybe identity (set #documentSessionTimeoutSecs)    mcompanydocumentsessiontimeout
    . maybe identity (set #hasPostSignview . (== "true")) mcompanyhaspostsignview
    . maybe identity (set #eidUseForSEView . (== "true")) mcompanyeiduseforseview
    . maybe identity (set #appFrontend . (== "true"))     mcompanyappfrontend
    . maybe identity (set #seBankIDSigningOverride)       mcompanysebankidsigningoverride
    . maybe identity (set #padesCredentialsLabel)         mcompanypadescredentialslabel

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
  mlang <- (langFromCode =<<) <$> getField "userlang"
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
      & maybe identity (#firstName .~)       muserfstname
      & maybe identity (#lastName .~)        musersndname
      & maybe identity (#personalNumber .~)  muserpersonalnumber
      & maybe identity (#companyPosition .~) musercompanyposition
      & maybe identity (#phone .~)           muserphone
      & maybe identity (#email .~)           museremail

jsonDocuments :: Kontrakcja m => m Response
jsonDocuments = onlyAdmin $ do
  adminUser        <- guardJustM $ view #maybeUser <$> getContext
  muid             <- readField "userid"
  mugid            <- readField "companyid"
  offset           <- guardJustM $ readField "offset"
  maxcount         <- guardJustM $ readField "max"

  requestedFilters <- getFieldBS "filter" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Right js -> case Unjson.parse Unjson.unjsonDef js of
        (Result res []) -> return $ toDocumentFilter (adminUser ^. #id) =<< res
        _               -> internalError
      Left _ -> internalError
    Nothing -> return []

  requestedSorting <- getFieldBS "sorting" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Right js -> case Unjson.parse Unjson.unjsonDef js of
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
handleBackdoorQuery = onlySalesOrAdmin . onlyBackdoorOpen $ do
  emailAddress <- guardJustM $ getField "email_address"
  emailTitle   <- guardJustM $ getField "email_title"
  startDate    <- guardJustM
    (((MinutesTime.parseTimeISO . T.unpack) =<<) <$> getField "start_date")
  memail <- dbQuery $ GetEmailForRecipient emailAddress emailTitle startDate
  case memail of
    Nothing    -> respond404
    Just email -> renderFromBody $ mailContent email

sendInviteAgain :: Kontrakcja m => m ()
sendInviteAgain = onlySalesOrAdmin $ do
  uid  <- guardJustM $ readField "userid"
  user <- guardJustM . dbQuery $ GetUserByID uid
  sendNewUserMail user

-- This method can be used to reseal a document
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlyAdmin . withDocumentID docid $ do
  logInfo_ "Trying to reseal document (only superadmin can do that)"
  ctx      <- getContext
  actor    <- guardJust $ mkAdminActor ctx
  document <- dbQuery $ GetDocumentByDocumentID docid
  guardNoPades document
  void . dbUpdate $ InsertEvidenceEvent ResealedPDF (return ()) actor
  void $ postDocumentClosedActions False True
  return LoopBack

-- This method can be used to reseal a document
reflattenFile :: Kontrakcja m => DocumentID -> m KontraLink
reflattenFile docid = onlyAdmin . withDocumentID docid $ do
  logInfo_ "Reflattening by superadmin, related to issue on 10.06.2020"
  document               <- dbQuery $ GetDocumentByDocumentID docid
  baseFile               <- guardJust $ documentinputfile document
  eReflattenedPdfContent <- preCheckPDF =<< getFileContents baseFile
  case eReflattenedPdfContent of
    Left  _                     -> internalError
    Right reflattenedPdfContent -> do
      reflattenedFile <- saveNewFile (filename baseFile) reflattenedPdfContent
      dbUpdate . AppendReflattenedInputFile docid $ fileid reflattenedFile
      return LoopBack

guardNoPades :: Kontrakcja m => Document -> m ()
guardNoPades doc = do
  let digitalSignatureMethod = documentdigitalsignaturemethod doc
  when (digitalSignatureMethod == Pades) $ do
    logInfo "Cannot reseal a document with PAdES" $ logObject_ doc
    internalError

-- This method can be used to force postDocumentPendingChange on a doc
-- e.g. when everybody signed but doc is still pending
triggerPostPending :: Kontrakcja m => DocumentID -> m KontraLink
triggerPostPending did = onlyAdmin . withDocumentID did $ do
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
  ctx      <- getContext
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
            couldBeResealed =
              everybodySignedAndStatusIn [Closed, DocumentError]
                && documentdigitalsignaturemethod document
                == Guardtime
            couldDoPostPending = documentstatus document == Pending
            callbackResult     = fromMaybe "Unknown" mCallbackResult
            extraDoc           = ExtraDocument callbackResult
        F.value "daveBody" $ inspectXML document
        F.value "extraDaveBody" $ inspectXML extraDoc
        F.value "id" $ show documentid
        F.value "couldBeResealed" couldBeResealed
        F.value "couldBeReflatten" $ documentstatus document /= Closed && isJust
          (documentinputfile document)
        F.value "couldDoPostPending" couldDoPostPending
        F.value "istemplate" $ documenttype document == Template
        entryPointFields ctx
      return $ Right r
    else return . Left $ LinkDaveDocument documentid

{- |
   Used by super users to inspect a particular signatory link.
-}
daveSignatoryLink :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Text
daveSignatoryLink documentid siglinkid = onlyAdmin $ do
  -- No GetDocumentByDocumentIDSignatoryLinkID here, allow getting deleted
  -- signatory links.
  document <- dbQuery $ GetDocumentByDocumentID documentid
  siglink  <- guardJust $ getSigLinkFor siglinkid document
  renderTextTemplate "daveSignatoryLink" $ do
    F.value "daveBody" $ inspectXML siglink

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID -> m Text
daveUser userid = onlyAdmin $ do
  user <- guardJustM . dbQuery $ GetUserByID userid
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
        . setHeader "Content-Disposition" ("attachment;filename=" <> fname')
        $ Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing

randomScreenshotForTest :: Kontrakcja m => m Response
randomScreenshotForTest = do
  now <- currentTime
  let lastWeek = 7 `daysBefore` now
  slid <- guardJustM . dbQuery $ GetRandomSignatoryLinkIDThatSignedRecently lastWeek
  screenshots <- map snd <$> dbQuery (GetSignatoryScreenshots [slid])
  doc         <- dbQuery $ GetDocumentBySignatoryLinkID slid
  elogEvents  <- dbQuery . GetEvidenceLog $ documentid doc
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
  user        <- guardJustM . dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if user ^. #isCompanyAdmin && withCompany
    then getUsageStats PartitionByDay False (UsageStatsForUserGroup $ user ^. #groupID)
    else getUsageStats PartitionByDay False (UsageStatsForUser $ user ^. #id)


handleAdminUserUsageStatsMonths :: Kontrakcja m => UserID -> m JSValue
handleAdminUserUsageStatsMonths uid = onlySalesOrAdmin $ do
  user        <- guardJustM . dbQuery $ GetUserByID uid
  withCompany <- isFieldSet "withCompany"
  if user ^. #isCompanyAdmin && withCompany
    then getUsageStats PartitionByMonth False (UsageStatsForUserGroup $ user ^. #groupID)
    else getUsageStats PartitionByMonth False (UsageStatsForUser $ user ^. #id)

handleAdminCompanyUsageStatsDays :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsDays ugid =
  onlySalesOrAdmin $ getUsageStats PartitionByDay False (UsageStatsForUserGroup ugid)

handleAdminCompanyUsageStatsMonths :: Kontrakcja m => UserGroupID -> m JSValue
handleAdminCompanyUsageStatsMonths ugid =
  onlySalesOrAdmin $ getUsageStats PartitionByMonth False (UsageStatsForUserGroup ugid)


handleAdminUserShareableLinksStats
  :: Kontrakcja m => StatsPartition -> UserID -> m JSValue
handleAdminUserShareableLinksStats statsPartition uid =
  onlySalesOrAdmin $ getShareableLinksStats statsPartition False (UsageStatsForUser uid)

handleAdminCompanyShareableLinksStats
  :: Kontrakcja m => StatsPartition -> UserGroupID -> m JSValue
handleAdminCompanyShareableLinksStats statsPartition ugid =
  onlySalesOrAdmin
    $ getShareableLinksStats statsPartition False (UsageStatsForUserGroup ugid)


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
          (InvoicingTypeBillItem, mpp    ) -> BillItem mpp
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
    ["user_group_structure" .= ugWithChildrenToJson (UserGroupWithChildren root children)]
  where
    ugWithChildrenToJson (UserGroupWithChildren ug children) = object
      [ "group" .= object
        [ "name" .= (ug ^. #name)
        , "is_billable" .= (ug ^. #isBillable)
        , identifier $ ug ^. #id
        ]
      , "children" .= map ugWithChildrenToJson children
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
  case Aeson.eitherDecode domainJSON of
    Left err -> do
      logInfo "Error while parsing branding for adminonly" $ object ["error" .= err]
      internalError
    Right js -> case Unjson.parse unjsonBrandedDomain js of
      (Result newDomain []) -> void . dbUpdate $ UpdateBrandedDomain
        (copy #id obd $ copy #mainDomain obd newDomain)
      _ -> internalError

unjsonBrandedDomain :: UnjsonDef BrandedDomain
unjsonBrandedDomain =
  objectOf
    $   BrandedDomain
    <$> field "id"              (^. #id)              "Id of a branded domain (unique)"
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
            (B64.decodeLenient . BSC8.pack . drop 1 . dropWhile (',' /=))
            (BSC8.unpack . BS.append (BSC8.pack "data:image/png;base64,") . B64.encode)
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
  bdID <- dbUpdate NewBrandedDomain
  runJSONGenT $ do
    value "id" (show bdID)

handleTransferDocument :: Kontrakcja m => DocumentID -> m ()
handleTransferDocument did = onlySalesOrAdmin $ do
  newuid <- guardJustM $ readField "userid"
  doc    <- dbQuery $ GetDocumentByDocumentID did
  user   <- guardJustM . dbQuery $ GetUserByID newuid
  case documenttype doc of
    Template -> do
      let userInfo = (getFirstName user, getLastName user, getEmail user)
      dbUpdate $ TransferDocument did newuid userInfo
    _ -> internalError
