module Partner.API
( partnerAPI
-- Exported for tests
, partnerApiCallV1CompaniesGet
, partnerApiCallV1CompanyCreate
, partnerApiCallV1CompanyUpdate
, partnerApiCallV1CompanyGet
, partnerApiCallV1CompanyUsersGet
, partnerApiCallV1UserCreate
, partnerApiCallV1UserGet
, partnerApiCallV1UserUpdate
, partnerApiCallV1UserGetPersonalToken
) where

import Control.Monad.Catch (MonadCatch, SomeException(..), try)
import Data.Either (isLeft)
import Data.Unjson as Unjson
import Happstack.Server.Types
import Happstack.StaticRouting
import Log
import qualified Data.Text as T

import API.V2
import API.V2.Errors
import API.V2.Parameters
import DB
import InputValidation (Result(..), asValidEmail)
import Kontra
import OAuth.Model
import Partner.JSON
import Partner.Logging
import Partner.Model
import Routing
import User.Action (createUser)
import User.Email (Email(..))
import User.Model
import UserGroup.Data
import UserGroup.Data.PaymentPlan
import UserGroup.Model

partnerAPI :: Route (Kontra Response)
partnerAPI = dir "api" $ choice
  [
    dir "v1" $ partnerAPIV1
  ]

partnerAPIV1 :: Route (Kontra Response)
partnerAPIV1 = dir "partner" $ choice
  [
    param $ dir "companies" $ hGet $ toK1 $ partnerApiCallV1CompaniesGet
  , param $ dir "company" $ dir "new" $ hPost $ toK1 $ partnerApiCallV1CompanyCreate
  , param $ dir "company" $ param $ dir "update" $ hPost $ toK2 $ partnerApiCallV1CompanyUpdate
  , param $ dir "company" $ param $ hGet $ toK2 $ partnerApiCallV1CompanyGet
  , param $ dir "company" $ param $ dir "user" $ dir "new" $ hPost $ toK2 $ partnerApiCallV1UserCreate
  , param $ dir "company" $ param $ dir "users" $ hGet $ toK2 $ partnerApiCallV1CompanyUsersGet
  , param $ dir "user" $ param $ hGet $ toK2 $ partnerApiCallV1UserGet
  , param $ dir "user" $ param $ dir "update" $ hPost $ toK2 $ partnerApiCallV1UserUpdate
  , param $ dir "user" $ param $ dir "getpersonalcredentials" $ hGet $ toK2 $ partnerApiCallV1UserGetPersonalToken
  ]

-- | Create a user group as a child of the partner's user group (root of the tree).
partnerApiCallV1CompanyCreate :: Kontrakcja m => PartnerID -> m Response
partnerApiCallV1CompanyCreate pid = logPartner pid . api $ do
  (apiuser, _actor) <- getAPIUser APIPersonal
  guardThatUserCanAdministerPartnerID apiuser pid
  (ptUserGroupID <$> (dbQuery $ GetPartnerByID pid)) >>= \case

    Nothing -> do
      let errMsg = "No root user group could be found for given partner identifier."
      logInfo "Partner API" $ object [ "error_message" .= errMsg ]
      apiError $ serverError errMsg

    Just partnerUserGroupID -> do
      let ugBase = set ugParentGroupID (Just partnerUserGroupID)
                 . set ugInvoicing     (BillItem $ Just FreePlan)
                 $ def
      ugu <- apiV2ParameterObligatory $ ApiV2ParameterJSON
                                            "json"
                                            unjsonUserGroupForUpdate
      let ug = updateUserGroupWithUserGroupForUpdate ugBase ugu
      (eUserGroup :: Either SomeException UserGroup) <- try . dbUpdate . UserGroupCreate $ ug
      case eUserGroup of
        Left exc -> do
            let errMsg = "The user group could not be created; " <> (T.pack . show $ exc)
            apiError . serverError $ errMsg
        Right ug' ->
            Created <$> return (unjsonUserGroupForUpdate,
                                userGroupToUserGroupForUpdate ug')

partnerApiCallV1CompanyUpdate :: ( MonadCatch m
                                 , Kontrakcja m)
                              => PartnerID
                              -> UserGroupID
                              -> m Response
partnerApiCallV1CompanyUpdate pid ugid =
  logPartnerAndUserGroup pid ugid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid
    ug <- guardThatPartnerIDCanAdministerUserGroupID pid ugid
    uguJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
    ugu <- case (Unjson.update (userGroupToUserGroupForUpdate ug)
                               unjsonUserGroupForUpdate uguJSON) of
        (Unjson.Result value []) ->
          return value
        (Unjson.Result _ errs) ->
          let errMsg = "Errors while parsing user group data:" <+>
                       (T.pack . show $ errs)
          in apiError . requestParameterParseError "json" $ errMsg
    let ug' = updateUserGroupWithUserGroupForUpdate ug ugu
    -- `UserGroupUpdate` has no boolean return status so we `try` instead.
    (eDidUpdate :: Either SomeException ()) <- try . dbUpdate $ UserGroupUpdate ug'
    when (isLeft eDidUpdate) $
      apiError $ serverError "The user group details could not be updated."
    updatedUserGroup <- apiGuardJustM
      (serverError "Was not able to retrieve updated company")
      (dbQuery . UserGroupGet $ get ugID ug)
    return . Ok $ (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate updatedUserGroup)

partnerApiCallV1CompanyGet :: Kontrakcja m => PartnerID -> UserGroupID -> m Response
partnerApiCallV1CompanyGet pid ugid =
  logPartnerAndUserGroup pid ugid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid
    ug <- guardThatPartnerIDCanAdministerUserGroupID pid ugid
    Ok <$> return (unjsonUserGroupForUpdate,
                   userGroupToUserGroupForUpdate ug)

partnerApiCallV1CompaniesGet :: Kontrakcja m => PartnerID -> m Response
partnerApiCallV1CompaniesGet pid =
  logPartner pid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid

    (ptUserGroupID <$> (dbQuery $ GetPartnerByID pid)) >>= \case
      Nothing -> do
        let errMsg = "No root user group could be found for given partner identifier."
        logInfo "Partner API" $ object [ "error_message" .= errMsg ]
        apiError $ serverError errMsg
      Just partnerUserGroupID -> do
        userGroups <- dbQuery $ UserGroupGetAllChildren partnerUserGroupID
        Ok <$> return (unjsonUserGroupsForUpdate,
                       userGroupToUserGroupForUpdate <$> userGroups)

partnerApiCallV1UserCreate :: Kontrakcja m => PartnerID -> UserGroupID -> m Response
partnerApiCallV1UserCreate pid ugid =
  logPartnerAndUserGroup pid ugid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid
    _ug <- guardThatPartnerIDCanAdministerUserGroupID pid ugid
    (userInfo, hasAcceptedTOS, lang) <- do
      userForUpdate <- apiV2ParameterObligatory $ ApiV2ParameterJSON "json" unjsonUserForUpdate
      return ( userInfoFromUserForUpdate userForUpdate
             , ufuHasAcceptedTOS userForUpdate
             , ufuLang userForUpdate
             )
    guardValidEmailAndNoExistingUser (useremail userInfo) Nothing
    when (not hasAcceptedTOS) $
      apiError $ requestParameterInvalid
                   "has_accepted_tos"
                   ("The user must accept the Scrive Terms of Service" <+>
                    "to create an account")
    newUser <- apiGuardJustM
      (serverError "The user could not be created")
      (createUser (useremail userInfo)
                  (userfstname userInfo, usersndname userInfo)
                  (ugid, False)
                  lang
                  PartnerInvitation)

    let uid = userid newUser
    didUpdate <- dbUpdate $ SetUserInfo uid userInfo
    when (not didUpdate) $
      apiError $ serverError "Could not update user details"
    when hasAcceptedTOS $
      currentTime >>= void . dbUpdate . AcceptTermsOfService uid
    -- re-get from DB to go to the source of truth
    userRefreshed <- apiGuardJustM
      (serverError "Could not retrieve updated user details")
      (dbQuery $ GetUserByID uid)
    Created <$> return (unjsonUserForUpdate, userToUserForUpdate userRefreshed)

partnerApiCallV1UserGet :: Kontrakcja m => PartnerID -> UserID -> m Response
partnerApiCallV1UserGet pid uid =
  logPartnerAndUser pid uid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid
    user <- guardThatPartnerIDCanAdministerUserID pid uid
    Ok <$> return (unjsonUserForUpdate, userToUserForUpdate user)

partnerApiCallV1CompanyUsersGet :: Kontrakcja m => PartnerID -> UserGroupID -> m Response
partnerApiCallV1CompanyUsersGet pid ugid =
  logPartnerAndUserGroup pid ugid . api $ do
    (apiuser, _actor) <- getAPIUser APIPersonal
    guardThatUserCanAdministerPartnerID apiuser pid
    _ug <- guardThatPartnerIDCanAdministerUserGroupID pid ugid
    users <- dbQuery $ UserGroupGetUsers ugid
    -- Result
    Ok <$> return (unjsonUsersForUpdate, userToUserForUpdate <$> users)

partnerApiCallV1UserUpdate :: Kontrakcja m => PartnerID -> UserID -> m Response
partnerApiCallV1UserUpdate pid userID =
  logPartnerAndUser pid userID . api $ do
    -- Permissions
    (apiuser, _actor) <- getAPIUser APIPersonal
    -- Guards
    guardThatUserCanAdministerPartnerID apiuser pid
    user <- guardThatPartnerIDCanAdministerUserID pid userID
    -- Parameters
    ufuJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
    ufu <- case (Unjson.update (userToUserForUpdate user) unjsonUserForUpdate ufuJSON) of
        (Result value []) ->
          return value
        (Result _ errs) ->
          apiError $ requestParameterParseError "json" $ "Errors while parsing user data:" <+> T.pack (show errs)
    let userInfo = userInfoFromUserForUpdate ufu

    -- More guards
    guardValidEmailAndNoExistingUser (useremail userInfo) (Just userID)
    when (not $ ufuHasAcceptedTOS ufu) $
      apiError $ requestParameterInvalid "has_accepted_tos" "The user must accept the Scrive Terms of Service to use it."
    -- API call actions
    didUpdateInfo     <- dbUpdate $ SetUserInfo userID userInfo
    didUpdateSettings <- dbUpdate $ SetUserSettings userID (UserSettings $ ufuLang ufu)
    when (not $ didUpdateInfo && didUpdateSettings) $
      apiError $ serverError "Could not update user"
    -- re-fetch original to get what's really in the DB.
    userFromDB <- apiGuardJustM
      (serverError "The updated user could not be fetched from the database.")
      (dbQuery $ GetUserByID userID)
    -- Result
    Ok <$> return (unjsonUserForUpdate, userToUserForUpdate userFromDB)

partnerApiCallV1UserGetPersonalToken :: Kontrakcja m => PartnerID -> UserID -> m Response
partnerApiCallV1UserGetPersonalToken pid userID =
  logPartnerAndUser pid userID . api $ do
    -- Permissions
    (apiuser, _actor) <- getAPIUser APIPersonal
    -- Guards
    guardThatUserCanAdministerPartnerID apiuser pid
    user <- guardThatPartnerIDCanAdministerUserID pid userID
    -- API call actions
    _ <- dbUpdate $ CreatePersonalToken (userid user)
    token <- apiGuardJustM
      (serverError "Could not get user personal token")
      (dbQuery $ GetPersonalToken (userid user))
    -- Result
    --return $ Ok $ J.JSObject (J.toJSObject token)
    return $ Ok (unjsonOAuthAuthorization, token)

----------------------------------------------------------------------------------------------------
--                                   Unexported local helpers                                     --
----------------------------------------------------------------------------------------------------

-- Checks that the non-superuser partner is allowed to operate on the supplied
-- `PartnerID`.  If not, throws.
guardThatUserCanAdministerPartnerID :: Kontrakcja m => User -> PartnerID -> m ()
guardThatUserCanAdministerPartnerID user pid = do
  isPartnerAdmin <- dbQuery $ IsUserPartnerAdmin (userid user) pid
  if isPartnerAdmin
     then return ()
     else apiError insufficientPrivileges

guardThatPartnerIDCanAdministerUserGroupID :: Kontrakcja m => PartnerID -> UserGroupID -> m UserGroup
guardThatPartnerIDCanAdministerUserGroupID pid ugid = do
  mug <- dbQuery $ UserGroupGet ugid
  case mug of
    Nothing -> apiError $ resourceNotFound "A user group with that ID was not found"
    Just ug -> do
      (ptUserGroupID <$> (dbQuery $ GetPartnerByID pid)) >>= \case
        Nothing -> apiError $ serverError "No user group found for supplied partner ID"
        Just pugid ->
          if (Just pugid /= get ugParentGroupID ug)
          then apiError insufficientPrivileges
          else return ug

guardThatPartnerIDCanAdministerUserID :: Kontrakcja m => PartnerID -> UserID -> m User
guardThatPartnerIDCanAdministerUserID pid uid = do
  mUser <- dbQuery $ GetUserByID uid
  case mUser of
    Nothing -> apiError $ resourceNotFound "A user with that ID was not found"
    Just user -> do
      _ug <- guardThatPartnerIDCanAdministerUserGroupID pid (usergroupid user)
      return user

guardValidEmailAndNoExistingUser :: Kontrakcja m => Email -> Maybe UserID -> m ()
guardValidEmailAndNoExistingUser email muid = do
  case asValidEmail (unEmail email) of
    Good _ -> do
      mExisting <- dbQuery (GetUserByEmail email)
      case (mExisting, liftA2 (==) muid (fmap userid mExisting)) of
        (Nothing, _) -> return ()
        (Just _, Just True) -> return ()
        _ -> apiError $ requestParameterInvalid "email" "A user with that email address already exists."
    _ -> apiError $ requestParameterInvalid "email" "The email address is invalid"
