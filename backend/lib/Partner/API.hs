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
import Data.Int (Int64)
import Data.Unjson as Unjson
import Happstack.Server.Types
import Happstack.StaticRouting
import Log

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
import Partner.Model
import Routing
import User.Action (createUser)
import User.Email (Email(..))
import User.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

partnerAPI :: Route (Kontra Response)
partnerAPI = dir "api" $ choice [dir "v1" $ partnerAPIV1]

partnerAPIV1 :: Route (Kontra Response)
partnerAPIV1 = dir "partner" $ choice
  [ param $ dir "companies" $ hGet $ toK1 $ partnerApiCallV1CompaniesGet
  , param $ dir "company" $ dir "new" $ hPost $ toK1 $ partnerApiCallV1CompanyCreate
  , param
  $ dir "company"
  $ param
  $ dir "update"
  $ hPost
  $ toK2
  $ partnerApiCallV1CompanyUpdate
  , param $ dir "company" $ param $ hGet $ toK2 $ partnerApiCallV1CompanyGet
  , param
  $ dir "company"
  $ param
  $ dir "user"
  $ dir "new"
  $ hPost
  $ toK2
  $ partnerApiCallV1UserCreate
  , param
  $ dir "company"
  $ param
  $ dir "users"
  $ hGet
  $ toK2
  $ partnerApiCallV1CompanyUsersGet
  , param $ dir "user" $ param $ hGet $ toK2 $ partnerApiCallV1UserGet
  , param $ dir "user" $ param $ dir "update" $ hPost $ toK2 $ partnerApiCallV1UserUpdate
  , param
  $ dir "user"
  $ param
  $ dir "getpersonalcredentials"
  $ hGet
  $ toK2
  $ partnerApiCallV1UserGetPersonalToken
  ]

-- | Create a user group as a child of the partner's user group (root of the tree).
partnerApiCallV1CompanyCreate :: Kontrakcja m => Int64 -> m Response
partnerApiCallV1CompanyCreate ptOrUgID = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartner mPartnerID partnerUsrGrpID . api $ do
    let acc = [mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)]
    apiAccessControl acc $ do
      ugwp_partner <-
        apiGuardJustM (serverError "Was not able to retrieve partner")
        . dbQuery
        . UserGroupGetWithParents
        $ partnerUsrGrpID
      newUgFolder <- dbUpdate . FolderCreate $ defaultFolder
      let ug_new = defaultUserGroup
            { ugParentGroupID = Just partnerUsrGrpID
            , ugHomeFolderID  = Just $ folderID newUgFolder
            , ugInvoicing     = BillItem $ Just FreePlan
            }
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
            . UserGroupGetWithParents
            . ugID
            $ ug'
          Created
            <$> return (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp)

partnerApiCallV1CompanyUpdate
  :: (MonadCatch m, Kontrakcja m) => Int64 -> UserGroupID -> m Response
partnerApiCallV1CompanyUpdate ptOrUgID ugid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUserGroup mPartnerID partnerUsrGrpID ugid . api $ do
    -- for backwards compatibility we check _both_ that the user is allowed
    -- to update the specified partner _and_ the user group. In the future
    -- this should be unnecessary.
    let acc =
          [ mkAccPolicyItem (UpdateA, UserGroupR, ugid)
          , mkAccPolicyItem (UpdateA, UserGroupR, partnerUsrGrpID)
          ]
    apiAccessControl acc $ do
      dbQuery (UserGroupGetWithParents ugid) >>= \case
        Nothing   -> noUsrGrpErr
        Just ugwp -> do
          uguJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
          ugu     <-
            case
              (Unjson.update (userGroupToUserGroupForUpdate ugwp)
                             unjsonUserGroupForUpdate
                             uguJSON
              )
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
            . Ok
            $ (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp_updated)

partnerApiCallV1CompanyGet :: Kontrakcja m => Int64 -> UserGroupID -> m Response
partnerApiCallV1CompanyGet ptOrUgID ugid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUserGroup mPartnerID partnerUsrGrpID ugid . api $ do
    -- for backwards compatibility we check _both_ that the user is allowed
    -- to update the specified partner _and_ the user group. In the future
    -- this should be unnecessary.
    let acc =
          [ mkAccPolicyItem (ReadA, UserGroupR, ugid)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
        -- see @note for `partnerApiCallV1CompaniesGet`
    apiAccessControl acc $ do
      (dbQuery $ UserGroupGetWithParents ugid) >>= \case
        Nothing   -> noUsrGrpErr
        Just ugwp -> do
          Ok <$> return (unjsonUserGroupForUpdate, userGroupToUserGroupForUpdate ugwp)

partnerApiCallV1CompaniesGet :: Kontrakcja m => Int64 -> m Response
partnerApiCallV1CompaniesGet ptOrUgID = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartner mPartnerID partnerUsrGrpID . api $ do
    let acc =
          [ mkAccPolicyItem (ReadA, UserGroupR, partnerUsrGrpID)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
    -- @note The last entry is just a trick to keep company admins from
    -- reading the companies - as of now we only want partner admins to be
    -- able to read, even though in the future this will change. The trick
    -- relies on the fact that only partner admins have `(CreateA,
    -- UserGroupR, partnerUsrGrpID)`; cf. instance for HasPermissions of
    -- (AccessRole UserGroupID) in AccessControl.Types...around line 95
    apiAccessControl acc $ do
      user_groups              <- dbQuery $ UserGroupGetImmediateChildren partnerUsrGrpID
      user_groups_with_parents <- fmap catMaybes . forM user_groups $ \ug ->
        dbQuery . UserGroupGetWithParents . ugID $ ug
      Ok <$> return
        ( unjsonUserGroupsForUpdate
        , userGroupToUserGroupForUpdate <$> user_groups_with_parents
        )

partnerApiCallV1UserCreate :: Kontrakcja m => Int64 -> UserGroupID -> m Response
partnerApiCallV1UserCreate ptOrUgID ugid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUserGroup mPartnerID partnerUsrGrpID ugid . api $ do
    let acc =
          [ mkAccPolicyItem (CreateA, UserR, ugid)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
              {- This last one is blocking for all but partner admins.             -}
              {- Cf. `HasPermissions` instance for `(AccessRole UserGroupID)`      -}
              {- Maybe we don't need to have the _exact_ same behaviour as before? -}
    apiAccessControl acc $ do
      (userInfo, hasAcceptedTOS, lang) <- do
        userForUpdate <- apiV2ParameterObligatory
          $ ApiV2ParameterJSON "json" unjsonUserForUpdate
        return
          ( userInfoFromUserForUpdate userForUpdate
          , ufuHasAcceptedTOS userForUpdate
          , ufuLang userForUpdate
          )
      guardValidEmailAndNoExistingUser (useremail userInfo) Nothing
      unless hasAcceptedTOS $ tosNotAcceptedErr

      -- API call actions
      newUser <- apiGuardJustM
        (serverError "The user could not be created")
        (createUser (useremail userInfo)
                    (userfstname userInfo, usersndname userInfo)
                    (ugid                , False)
                    lang
                    PartnerInvitation
        )

      let uid = userid newUser
      didUpdate <- dbUpdate $ SetUserInfo uid userInfo
      unless didUpdate $ srvLogErr "Could not update user details"
      when hasAcceptedTOS $ currentTime >>= void . dbUpdate . AcceptTermsOfService uid
      -- re-get from DB to go to the source of truth
      userRefreshed <- apiGuardJustM
        (serverError "Could not retrieve updated user details")
        (dbQuery $ GetUserByID uid)
      -- Result
      Created <$> return (unjsonUserForUpdate, userToUserForUpdate userRefreshed)

partnerApiCallV1UserGet :: Kontrakcja m => Int64 -> UserID -> m Response
partnerApiCallV1UserGet ptOrUgID uid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUser mPartnerID partnerUsrGrpID uid . api $ do
    let acc =
          [ mkAccPolicyItem (ReadA, UserR, uid)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
        -- see @note for `partnerApiCallV1CompaniesGet`
    apiAccessControl acc $ do
      (dbQuery . GetUserByID $ uid) >>= \case
        Nothing -> do
          apiError $ resourceNotFound "A user with that ID was not found"
        Just user -> do
          Ok <$> return (unjsonUserForUpdate, userToUserForUpdate user)

partnerApiCallV1CompanyUsersGet :: Kontrakcja m => Int64 -> UserGroupID -> m Response
partnerApiCallV1CompanyUsersGet ptOrUgID ugid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUserGroup mPartnerID partnerUsrGrpID ugid . api $ do
    let acc =
          [ mkAccPolicyItem (ReadA, UserGroupR, ugid)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
        -- see @note for `partnerApiCallV1CompaniesGet`
    apiAccessControl acc $ do
      users <- dbQuery $ UserGroupGetUsers ugid
      Ok <$> return (unjsonUsersForUpdate, userToUserForUpdate <$> users)

partnerApiCallV1UserUpdate :: Kontrakcja m => Int64 -> UserID -> m Response
partnerApiCallV1UserUpdate ptOrUgID uid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUser mPartnerID partnerUsrGrpID uid . api $ do
    let acc =
          [ mkAccPolicyItem (UpdateA, UserR, uid)
          , mkAccPolicyItem (UpdateA, UserGroupR, partnerUsrGrpID)
          ]
        -- see @note for `partnerApiCallV1CompaniesGet`
    apiAccessControl acc $ do
      user    <- guardThatUserExists uid
      ufuJSON <- apiV2ParameterObligatory $ ApiV2ParameterAeson "json"
      ufu     <-
        case (Unjson.update (userToUserForUpdate user) unjsonUserForUpdate ufuJSON) of
          (Result value []) -> return value
          (Result _ errs) -> rqPrmErr ("Errors while parsing user data:" <+> showt errs)
      let userInfo = userInfoFromUserForUpdate ufu

      guardValidEmailAndNoExistingUser (useremail userInfo) (Just uid)
      unless (ufuHasAcceptedTOS ufu) $ tosNotAcceptedErr
      didUpdateInfo     <- dbUpdate $ SetUserInfo uid userInfo
      didUpdateSettings <- dbUpdate
        $ SetUserSettings uid (UserSettings (ufuLang ufu) defaultDataRetentionPolicy)
      -- @todo fix retention policy ^
      unless (didUpdateInfo && didUpdateSettings) $ srvLogErr "Could not update user"
      -- re-fetch original to get what's really in the DB.
      userFromDB <- apiGuardJustM
        (serverError "The updated user could not be fetched from the database.")
        (dbQuery $ GetUserByID uid)
      Ok <$> return (unjsonUserForUpdate, userToUserForUpdate userFromDB)

partnerApiCallV1UserGetPersonalToken :: Kontrakcja m => Int64 -> UserID -> m Response
partnerApiCallV1UserGetPersonalToken ptOrUgID uid = do
  (mPartnerID, partnerUsrGrpID) <- resolveUserGroupID ptOrUgID
  logPartnerAndUser mPartnerID partnerUsrGrpID uid . api $ do
    let acc =
          [ mkAccPolicyItem (CreateA, UserPersonalTokenR, uid)
          , mkAccPolicyItem (ReadA, UserPersonalTokenR, uid)
          , mkAccPolicyItem (CreateA, UserGroupR, partnerUsrGrpID)
          ]
    apiAccessControl acc $ do
      user <- guardThatUserExists uid -- @todo for now...
      void $ dbUpdate $ CreatePersonalToken (userid user) -- @todo in the future: avoid this DB hit?
      token <- apiGuardJustM (serverError "Could not get user personal token")
                             (dbQuery $ GetPersonalToken (userid user))
      return $ Ok (unjsonOAuthAuthorization, token)
----------------------------------------------------------------------------------------------------
--                                   Unexported local helpers                                     --
----------------------------------------------------------------------------------------------------

guardValidEmailAndNoExistingUser :: Kontrakcja m => Email -> Maybe UserID -> m ()
guardValidEmailAndNoExistingUser email muid = do
  case asValidEmail (unEmail email) of
    Good _ -> do
      mExisting <- dbQuery (GetUserByEmail email)
      case (mExisting, liftA2 (==) muid (fmap userid mExisting)) of
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

noUsrGrpErrPartner :: (Kontrakcja m) => m a
noUsrGrpErrPartner =
  srvLogErr $ "The user group could not be retrieved for the given partner identifier."

rqPrmErr :: (Kontrakcja m) => Text -> m a
rqPrmErr t = do
  logInfo "Partner API" $ object ["error_message" .= t]
  apiError . requestParameterParseError "json" $ t

resolveUserGroupID
  :: Kontrakcja m
  => Int64
  -- ^ Argument to be checked whether it is a 'PartnerID' or a
  -- 'UserGroupID'.
  -> m (Maybe PartnerID, UserGroupID)
  -- ^ If the original argument is a 'PartnerID', return this along
  -- with the 'UserGroupID' (mostly for logging purposes).
resolveUserGroupID k = do
  (ePartner :: Either SomeException Partner) <- do
    try . dbQuery . GetPartnerByID . unsafePartnerID $ k
  mUserGroup <- do
    dbQuery . UserGroupGet . unsafeUserGroupID $ k
  case (ePartner, mUserGroup) of
    (Right partner, Nothing) -> do
      case ptUserGroupID partner of
        Nothing     -> noUsrGrpErrPartner
        (Just ugid) -> return (Just . ptID $ partner, ugid)

    (Left _, Just ug) -> do
      return (Nothing, ugID ug)

    (Right partner, Just ug) -> do
      -- This won't ever happen *except* in tests the way they're implemented now.
      -- Should it happen anyway it's actually OK, but let's log it.
      logInfo "Partner API" $ object
        [ "message"
          .= ("Identifier corresponds to a partner ID" <+> "as well as a user group ID" :: Text
             )
        , "identifier" .= k
        ]
      let mpID = ptUserGroupID partner
      unless (isJust mpID && (Just $ ugID ug) == mpID) $ do
        srvLogErr $ "The partner ID and the user group ID are not connected"
      return (Just . ptID $ partner, ugID ug)

    (_, _) -> do
      srvLogErr "No partner, no user group for the given identifier"
