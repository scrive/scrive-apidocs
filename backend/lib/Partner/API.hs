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

import AccessControl.Check
import AccessControl.Types
import API.V2
import API.V2.Parameters
import API.V2.Utils
import DataRetentionPolicy (defaultDataRetentionPolicy)
import DB
import Doc.API.V2.Guards (guardThatUserExists)
import Folder.Model
import InputValidation (Result(..), asValidEmail)
import Kontra
import OAuth.Model
import Partner.JSON
import Partner.Logging
import Routing
import User.Action
import User.Email (Email(..))
import User.Model
import UserGroup.Model
import UserGroup.Types

partnerAPI :: Route (Kontra Response)
partnerAPI = dir "api" $ choice [dir "v1" partnerAPIV1]

partnerAPIV1 :: Route (Kontra Response)
partnerAPIV1 = dir "partner" $ choice
  [ (param . dir "companies" . hGet . toK1) partnerApiCallV1CompaniesGet
  , (param . dir "company" . dir "new" . hPost . toK1) partnerApiCallV1CompanyCreate
  , (param . dir "company" . param . dir "update" . hPost . toK2)
    partnerApiCallV1CompanyUpdate
  , (param . dir "company" . param . hGet . toK2) partnerApiCallV1CompanyGet
  , (param . dir "company" . param . dir "user" . dir "new" . hPost . toK2)
    partnerApiCallV1UserCreate
  , (param . dir "company" . param . dir "users" . hGet . toK2)
    partnerApiCallV1CompanyUsersGet
  , (param . dir "user" . param . hGet . toK2) partnerApiCallV1UserGet
  , (param . dir "user" . param . dir "update" . hPost . toK2) partnerApiCallV1UserUpdate
  , (param . dir "user" . param . dir "getpersonalcredentials" . hGet . toK2)
    partnerApiCallV1UserGetPersonalToken
  ]

-- | Create a user group as a child of the partner's user group (root of the tree).
partnerApiCallV1CompanyCreate :: Kontrakcja m => UserGroupID -> m Response
partnerApiCallV1CompanyCreate partnerUsrGrpID = do
  logPartner partnerUsrGrpID . api $ do
    user         <- getAPIUserWithFullAccess
    requiredPerm <- apiRequirePermission . canDo CreateA $ UserGroupR partnerUsrGrpID
    apiAccessControl user requiredPerm $ do
      ugwp_partner <-
        apiGuardJustM (serverError "Was not able to retrieve partner")
        . dbQuery
        . UserGroupGetWithParents
        $ partnerUsrGrpID
      newUgFolder <- dbUpdate . FolderCreate $ defaultFolder
      let ug_new =
            defaultUserGroup
              & (#parentGroupID ?~ partnerUsrGrpID)
              & (#homeFolderID ?~ newUgFolder ^. #id)
              & (#invoicing .~ BillItem Nothing)
              & (#settings % _Just % #hasPostSignview .~ False)
      ugu <- apiV2ParameterObligatory $ ApiV2ParameterJSON "json" unjsonUserGroupForUpdate
      let ug =
            updateUserGroupWithUserGroupForUpdate (ugwpAddChild ug_new ugwp_partner) ugu
      (eUserGroup :: Either SomeException UserGroup) <-
        try . dbUpdate . UserGroupCreate $ ug
      case eUserGroup of
        Left exc -> do
          srvLogErr $ "The user group could not be created; " <> showt exc
        Right ug' -> do
          ugwp <-
            apiGuardJustM (serverError "Was not able to retrieve newly created company")
            . dbQuery
            $ UserGroupGetWithParents (ug' ^. #id)
          Created
            <$> return (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp)

partnerApiCallV1CompanyUpdate
  :: (MonadCatch m, Kontrakcja m) => UserGroupID -> UserGroupID -> m Response
partnerApiCallV1CompanyUpdate partnerUsrGrpID ugid = do
  logPartnerAndUserGroup partnerUsrGrpID ugid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo UpdateA $ UserGroupR ugid, canDo UpdateA $ UserGroupR partnerUsrGrpID]
    user <- getAPIUserWithFullAccess
    apiAccessControl user requiredPerm $ do
      dbQuery (UserGroupGetWithParents ugid) >>= \case
        Nothing   -> noUsrGrpErr
        Just ugwp -> do
          uguJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
          ugu     <-
            case
              Unjson.update (userGroupToUserGroupForUpdate ugwp)
                            unjsonUserGroupForUpdate
                            uguJSON
            of
              (Unjson.Result value []) -> return value
              (Unjson.Result _ errs) ->
                rqPrmErr $ "Errors while parsing user group data:" <+> showt errs
          let ug' = updateUserGroupWithUserGroupForUpdate ugwp ugu
          (eDidUpdate :: Either SomeException ()) <- try . dbUpdate $ UserGroupUpdate ug'
          when (isLeft eDidUpdate)
            $ srvLogErr "The user group details could not be updated."
          ugwp_updated <- apiGuardJustM
            (serverError "Was not able to retrieve updated company")
            (dbQuery . UserGroupGetWithParents $ ugid)
          return
            $ Ok (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp_updated)

partnerApiCallV1CompanyGet :: Kontrakcja m => UserGroupID -> UserGroupID -> m Response
partnerApiCallV1CompanyGet partnerUsrGrpID ugid = do
  logPartnerAndUserGroup partnerUsrGrpID ugid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo ReadA $ UserGroupR ugid, canDo CreateA $ UserGroupR partnerUsrGrpID]
    user <- getAPIUserWithFullAccess
    apiAccessControl user requiredPerm $ do
      dbQuery (UserGroupGetWithParents ugid) >>= \case
        Nothing -> noUsrGrpErr
        Just ugwp ->
          return $ Ok (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp)

partnerApiCallV1CompaniesGet :: Kontrakcja m => UserGroupID -> m Response
partnerApiCallV1CompaniesGet partnerUsrGrpID = do
  logPartner partnerUsrGrpID . api $ do
    requiredPerm <- apiRequireAllPermissions
      [ canDo ReadA $ UserGroupR partnerUsrGrpID
      , canDo CreateA $ UserGroupR partnerUsrGrpID
      ]
    -- @note The last entry is just a trick to keep company admins from
    -- reading the companies - as of now we only want partner admins to be
    -- able to read, even though in the future this will change. The trick
    -- relies on the fact that only partner admins have `(CreateA,
    -- UserGroupR, partnerUsrGrpID)`; cf. instance for HasPermissions of
    -- (AccessRole UserGroupID) in AccessControl.Types...around line 95
    user <- getAPIUserWithFullAccess
    apiAccessControl user requiredPerm $ do
      user_groups              <- dbQuery $ UserGroupGetImmediateChildren partnerUsrGrpID
      user_groups_with_parents <- fmap catMaybes . forM user_groups $ \ug ->
        dbQuery . UserGroupGetWithParents $ ug ^. #id
      Ok <$> return
        ( unjsonUserGroupsForUpdate
        , userGroupToUserGroupForUpdate <$> user_groups_with_parents
        )

partnerApiCallV1UserCreate :: Kontrakcja m => UserGroupID -> UserGroupID -> m Response
partnerApiCallV1UserCreate partnerUsrGrpID ugid = do
  logPartnerAndUserGroup partnerUsrGrpID ugid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo CreateA $ UserInGroupR ugid, canDo CreateA $ UserGroupR partnerUsrGrpID]
              {- This last one is blocking for all but partner admins.             -}
              {- Cf. `HasPermissions` instance for `(AccessRole UserGroupID)`      -}
              {- Maybe we don't need to have the _exact_ same behaviour as before? -}
    user <- getAPIUserWithFullAccess
    apiAccessControl user requiredPerm $ do
      (userInfo, hasAcceptedTOS, lang) <- do
        userForUpdate <- apiV2ParameterObligatory
          $ ApiV2ParameterJSON "json" unjsonUserForUpdate
        return
          ( userInfoFromUserForUpdate userForUpdate
          , userForUpdate ^. #hasAcceptedTOS
          , userForUpdate ^. #lang
          )
      guardValidEmailAndNoExistingUser (userInfo ^. #email) Nothing
      unless hasAcceptedTOS tosNotAcceptedErr

      -- API call actions
      cuctx   <- getCreateUserContextFromContext
      newUser <- apiGuardJustM
        (serverError "The user could not be created")
        (createUser (userInfo ^. #email)
                    (userInfo ^. #firstName, userInfo ^. #lastName)
                    (ugid                  , False)
                    lang
                    PartnerInvitation
                    cuctx
        )

      let uid = newUser ^. #id
      didUpdate <- dbUpdate $ SetUserInfo uid userInfo
      unless didUpdate $ srvLogErr "Could not update user details"
      when hasAcceptedTOS $ currentTime >>= void . dbUpdate . AcceptTermsOfService uid
      -- re-get from DB to go to the source of truth
      userRefreshed <- apiGuardJustM
        (serverError "Could not retrieve updated user details")
        (dbQuery $ GetUserByID uid)
      -- Result
      Created <$> return (unjsonUserForUpdate, userToUserForUpdate userRefreshed)

partnerApiCallV1UserGet :: Kontrakcja m => UserGroupID -> UserID -> m Response
partnerApiCallV1UserGet partnerUsrGrpID uid = do
  logPartnerAndUser partnerUsrGrpID uid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo ReadA $ UserR uid, canDo CreateA $ UserGroupR partnerUsrGrpID]
    apiUser <- getAPIUserWithFullAccess
    apiAccessControl apiUser requiredPerm $ do
      (dbQuery . GetUserByID $ uid) >>= \case
        Nothing -> do
          apiError $ resourceNotFound "A user with that ID was not found"
        Just user -> do
          Ok <$> return (unjsonUserForUpdate, userToUserForUpdate user)

partnerApiCallV1CompanyUsersGet
  :: Kontrakcja m => UserGroupID -> UserGroupID -> m Response
partnerApiCallV1CompanyUsersGet partnerUsrGrpID ugid = do
  logPartnerAndUserGroup partnerUsrGrpID ugid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo ReadA $ UserGroupR ugid, canDo CreateA $ UserGroupR partnerUsrGrpID]
    user <- getAPIUserWithFullAccess
    apiAccessControl user requiredPerm $ do
      users <- dbQuery $ UserGroupGetUsers ugid
      Ok <$> return (unjsonUsersForUpdate, userToUserForUpdate <$> users)

partnerApiCallV1UserUpdate :: Kontrakcja m => UserGroupID -> UserID -> m Response
partnerApiCallV1UserUpdate partnerUsrGrpID uid = do
  logPartnerAndUser partnerUsrGrpID uid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [canDo UpdateA $ UserR uid, canDo UpdateA $ UserGroupR partnerUsrGrpID]
    apiUser <- getAPIUserWithFullAccess
    apiAccessControl apiUser requiredPerm $ do
      user <- guardThatUserExists uid
      ufuJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
      ufu <- case Unjson.update (userToUserForUpdate user) unjsonUserForUpdate ufuJSON of
        (Result value []  ) -> return value
        (Result _     errs) -> rqPrmErr ("Errors while parsing user data:" <+> showt errs)
      let userInfo = userInfoFromUserForUpdate ufu

      guardValidEmailAndNoExistingUser (userInfo ^. #email) (Just uid)
      unless (ufu ^. #hasAcceptedTOS) tosNotAcceptedErr
      didUpdateInfo     <- dbUpdate $ SetUserInfo uid userInfo
      didUpdateSettings <- dbUpdate
        $ SetUserSettings uid (UserSettings (ufu ^. #lang) defaultDataRetentionPolicy)
      -- @todo fix retention policy ^
      unless (didUpdateInfo && didUpdateSettings) $ srvLogErr "Could not update user"
      -- re-fetch original to get what's really in the DB.
      userFromDB <- apiGuardJustM
        (serverError "The updated user could not be fetched from the database.")
        (dbQuery $ GetUserByID uid)
      Ok <$> return (unjsonUserForUpdate, userToUserForUpdate userFromDB)

partnerApiCallV1UserGetPersonalToken
  :: Kontrakcja m => UserGroupID -> UserID -> m Response
partnerApiCallV1UserGetPersonalToken partnerUsrGrpID uid = do
  logPartnerAndUser partnerUsrGrpID uid . api $ do
    requiredPerm <- apiRequireAllPermissions
      [ canDo CreateA $ UserPersonalTokenR uid
      , canDo ReadA $ UserPersonalTokenR uid
      , canDo CreateA $ UserGroupR partnerUsrGrpID
      ]
    apiUser <- getAPIUserWithFullAccess
    apiAccessControl apiUser requiredPerm $ do
      user <- guardThatUserExists uid -- @todo for now...
      void . dbUpdate $ CreatePersonalToken (user ^. #id) -- @todo in the future: avoid this DB hit?
      token <- apiGuardJustM (serverError "Could not get user personal token")
                             (dbQuery $ GetPersonalToken (user ^. #id))
      return $ Ok (unjsonOAuthAuthorization, token)
----------------------------------------------------------------------------------------------------
--                                   Unexported local helpers                                     --
----------------------------------------------------------------------------------------------------

guardValidEmailAndNoExistingUser :: Kontrakcja m => Email -> Maybe UserID -> m ()
guardValidEmailAndNoExistingUser email muid = do
  case asValidEmail (unEmail email) of
    Good _ -> do
      mExisting <- dbQuery (GetUserByEmail email)
      case (mExisting, liftA2 (==) muid (view #id <$> mExisting)) of
        (Nothing, _        ) -> return ()
        (Just _ , Just True) -> return ()
        _                    -> apiError $ requestParameterInvalid
          "email"
          "A user with that email address already exists."
    _ -> apiError $ requestParameterInvalid "email" "The email address is invalid"


tosNotAcceptedErr :: (Kontrakcja m) => m a
tosNotAcceptedErr = do
  apiError $ requestParameterInvalid
    "has_accepted_tos"
    ("The user must accept the Scrive Terms of Service" <+> "to create an account")

srvLogErr :: (Kontrakcja m) => Text -> m a
srvLogErr t = do
  logInfo "Partner API" $ object ["error_message" .= t]
  apiError $ serverError t

noUsrGrpErr :: (Kontrakcja m) => m a
noUsrGrpErr = srvLogErr "The user group could not be retrieved."

rqPrmErr :: (Kontrakcja m) => Text -> m a
rqPrmErr t = do
  logInfo "Partner API" $ object ["error_message" .= t]
  apiError . requestParameterParseError "json" $ t
