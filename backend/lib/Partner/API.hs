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

import Data.Unjson as Unjson
import Happstack.Server.Types
import Happstack.StaticRouting
import qualified Data.Text as T

import API.V2
import API.V2.Errors
import API.V2.Parameters
import Company.Model
import DB
import InputValidation (Result(..), asValidEmail)
import Kontra
import MinutesTime
import OAuth.Model
import Partner.JSON
import Partner.Logging
import Partner.Model
import Routing
import User.Action (createUser)
import User.Email (Email(..))
import User.Model

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

partnerApiCallV1CompanyCreate :: Kontrakcja m => PartnerID -> m Response
partnerApiCallV1CompanyCreate partnerID = logPartner partnerID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  -- Parameters
  companyForUpdate <- apiV2ParameterObligatory $ ApiV2ParameterJSON "json" unjsonCompanyForUpdate
  -- API call actions
  -- if partner is migrated to user groups, migrate the company immediately
  partner <- dbQuery (GetPartnerByID partnerID)
  newCompany <- case ptUserGroupID partner of
    -- partner was not migrated to user groups yet
    Nothing -> dbUpdate CreateCompanyWithoutUserGroup
    -- partner was already migrated to user groups
    Just _  -> dbUpdate CreateCompany
  let companyInfo = updateCompanyInfoWithCompanyForUpdate def companyForUpdate
      companyInfo' = companyInfo { companypartnerid = partnerID }
  didUpdate <- dbUpdate $ SetCompanyInfo (companyid newCompany) companyInfo'
  when (not didUpdate) $
    apiError $ serverError "The company details could not be updated."
  companyRefreshed <- apiGuardJustM
    (serverError "The company could not be re-fetched from database, but was created.")
    (dbQuery $ GetCompany (companyid newCompany))

  -- Result
  Created <$> return (unjsonCompanyForUpdate, companyToCompanyForUpdate companyRefreshed)

partnerApiCallV1CompanyUpdate :: Kontrakcja m => PartnerID -> CompanyID -> m Response
partnerApiCallV1CompanyUpdate partnerID companyID = logPartnerAndCompany partnerID companyID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  company <- guardThatPartnerIDCanAdministerCompanyID partnerID companyID
  -- Parameters
  cfuJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
  -- API call actions

  cfu <- case (Unjson.update (companyToCompanyForUpdate company) unjsonCompanyForUpdate cfuJSON) of
      (Unjson.Result value []) ->
        return value
      (Unjson.Result _ errs) ->
        apiError $ requestParameterParseError "json" $ "Errors while parsing company data:" <+> T.pack (show errs)

  let companyInfo = updateCompanyInfoWithCompanyForUpdate (companyinfo company) cfu
  didUpdate <- dbUpdate $ SetCompanyInfo companyID companyInfo
  when (not didUpdate) $
    apiError $ serverError "The company details could not be updated."
  updatedCompany <- apiGuardJustM
    (serverError "Was not able to retrieve updated company")
    (dbQuery $ GetCompany companyID)
  -- Result
  Ok <$> return (unjsonCompanyForUpdate, companyToCompanyForUpdate updatedCompany)

partnerApiCallV1CompanyGet :: Kontrakcja m => PartnerID -> CompanyID -> m Response
partnerApiCallV1CompanyGet partnerID companyID = logPartnerAndCompany partnerID companyID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  company <- guardThatPartnerIDCanAdministerCompanyID partnerID companyID
  -- Result
  Ok <$> return (unjsonCompanyForUpdate, companyToCompanyForUpdate company)

partnerApiCallV1CompaniesGet :: Kontrakcja m => PartnerID -> m Response
partnerApiCallV1CompaniesGet partnerID = logPartner partnerID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  companies <- dbQuery $ GetCompaniesByPartnerID partnerID
  -- Result
  Ok <$> return (unjsonCompaniesForUpdate, companyToCompanyForUpdate <$> companies)

partnerApiCallV1UserCreate :: Kontrakcja m => PartnerID -> CompanyID -> m Response
partnerApiCallV1UserCreate partnerID companyID = logPartnerAndCompany partnerID companyID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  company <- guardThatPartnerIDCanAdministerCompanyID partnerID companyID
  -- Parameters
  (userInfo, hasAcceptedTOS, lang) <- do
    userForUpdate <- apiV2ParameterObligatory $ ApiV2ParameterJSON "json" unjsonUserForUpdate
    return ( userInfoFromUserForUpdate userForUpdate
           , ufuHasAcceptedTOS userForUpdate
           , ufuLang userForUpdate
           )
  -- More guards
  guardValidEmailAndNoExistingUser (useremail userInfo) Nothing
  when (not hasAcceptedTOS) $
    apiError $ requestParameterInvalid "has_accepted_tos" "The user must accept the Scrive Terms of Service to create an account"
  -- API call actions
  newUser <- apiGuardJustM
    (serverError "The user could not be created")
    (createUser (useremail userInfo)
                (userfstname userInfo, usersndname userInfo)
                (companyid company, False)
                lang
                PartnerInvitation
    )
  let uid = userid newUser
  didUpdate <- dbUpdate $ SetUserInfo uid userInfo
  when (not didUpdate) $
    apiError $ serverError "Could not update user details with SetUserInfo"
  when hasAcceptedTOS $
    currentTime >>= (\now -> dbUpdate (AcceptTermsOfService uid now)) >> return ()
  -- re-get from DB to go to the source of truth
  userRefreshed <- apiGuardJustM
    (serverError "Could not retrieve updated user details with GetUserByID")
    (dbQuery $ GetUserByID uid)
  -- Result
  Created <$> return (unjsonUserForUpdate, userToUserForUpdate userRefreshed)

partnerApiCallV1UserGet :: Kontrakcja m => PartnerID -> UserID -> m Response
partnerApiCallV1UserGet partnerID userID = logPartnerAndUser partnerID userID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  user <- guardThatPartnerIDCanAdministerUserID partnerID userID
  -- Result
  Ok <$> return (unjsonUserForUpdate, userToUserForUpdate user)

partnerApiCallV1CompanyUsersGet :: Kontrakcja m => PartnerID -> CompanyID -> m Response
partnerApiCallV1CompanyUsersGet partnerID companyID = logPartnerAndCompany partnerID companyID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  company <- guardThatPartnerIDCanAdministerCompanyID partnerID companyID
  users <- dbQuery $ GetCompanyAccounts (companyid company)
  -- Result
  Ok <$> return (unjsonUsersForUpdate, userToUserForUpdate <$> users)

partnerApiCallV1UserUpdate :: Kontrakcja m => PartnerID -> UserID -> m Response
partnerApiCallV1UserUpdate partnerID userID = logPartnerAndUser partnerID userID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  user <- guardThatPartnerIDCanAdministerUserID partnerID userID
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
partnerApiCallV1UserGetPersonalToken partnerID userID = logPartnerAndUser partnerID userID . api $ do
  -- Permissions
  (apiuser, _actor) <- getAPIUser APIPersonal
  -- Guards
  guardThatUserCanAdministerPartnerID apiuser partnerID
  user <- guardThatPartnerIDCanAdministerUserID partnerID userID
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

-- @devnote: checks that the non-superuser partner is allowed to operate on the supplied
-- `PartnerID`.  If not, throws.
guardThatUserCanAdministerPartnerID :: Kontrakcja m => User -> PartnerID -> m ()
guardThatUserCanAdministerPartnerID user pid = do
  isPartnerAdmin <- dbQuery $ IsUserPartnerAdmin (userid user) pid
  if isPartnerAdmin
     then return ()
     else apiError insufficientPrivileges

guardThatPartnerIDCanAdministerCompanyID :: Kontrakcja m => PartnerID -> CompanyID -> m Company
guardThatPartnerIDCanAdministerCompanyID pid cid = do
  mCompany <- dbQuery $ GetCompany cid
  case mCompany of
    Nothing -> apiError $ resourceNotFound "A company with that ID was not found"
    Just company -> do
      when (pid /= (companypartnerid . companyinfo) company) (apiError insufficientPrivileges)
      return company

guardThatPartnerIDCanAdministerUserID :: Kontrakcja m => PartnerID -> UserID -> m User
guardThatPartnerIDCanAdministerUserID pid uid = do
  mUser <- dbQuery $ GetUserByID uid
  case mUser of
    Nothing -> apiError $ resourceNotFound "A user with that ID was not found"
    Just user -> do
      _company <- guardThatPartnerIDCanAdministerCompanyID pid (usercompany user)
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
