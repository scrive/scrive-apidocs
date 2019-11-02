module User.Model.Update (
    AddUser(..)
  , AcceptTermsOfService(..)
  , DeleteUser(..)
  , RemoveInactiveUser(..)
  , SetSignupMethod(..)
  , SetUserUserGroup(..)
  , SetUserGroup(..)
  , SetUserHomeFolder(..)
  , SetUserEmail(..)
  , SetUserCompanyAdmin(..)
  , SetUserInfo(..)
  , SetUserPassword(..)
  , SetUserTOTPKey(..)
  , ConfirmUserTOTPSetup(..)
  , DisableUserTOTP(..)
  , SetUserSettings(..)
  , SetUserTotpIsMandatory(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.ByteString (ByteString)
import Log
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import DataRetentionPolicy
import DB
import Doc.DocStateData (DocumentStatus(..))
import Doc.Types.Document (DocumentSharing(..), DocumentType(..))
import Doc.Types.SignatoryField
import Folder.Types
import IPAddress
import User.Email
import User.Lang
import User.Model.Query
import User.Password
import User.Types.SignupMethod
import User.Types.User
import User.UserID
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo

data AcceptTermsOfService = AcceptTermsOfService UserID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m AcceptTermsOfService Bool where
  update (AcceptTermsOfService uid time) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "has_accepted_terms_of_service" time
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data AddUser = AddUser
  (Text, Text)
  Text
  (Maybe Password)
  (UserGroupID, Maybe FolderID, Bool)
  Lang
  BrandedDomainID
  SignupMethod

instance (MonadDB m, MonadThrow m) => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd (ugid, mFid, admin) l ad sm) = do
    mu <- query $ GetUserByEmail $ Email email
    case mu of
      Just _  -> return Nothing -- user with the same email address exists
      Nothing -> do
        runQuery_ $ sqlInsert "users" $ do
          sqlSet "password" $ pwdHash <$> mpwd
          sqlSet "salt" $ pwdSalt <$> mpwd
          sqlSet "password_algorithm" $ pwdAlgorithmToInt16 . pwdAlgorithm <$> mpwd
          sqlSet "is_company_admin"  admin
          sqlSet "account_suspended" False
          sqlSet "has_accepted_terms_of_service" (Nothing :: Maybe UTCTime)
          sqlSet "signup_method"     sm
          sqlSet "first_name"        fname
          sqlSet "last_name"         lname
          sqlSet "personal_number"   ("" :: String)
          sqlSet "company_position"  ("" :: String)
          sqlSet "phone"             ("" :: String)
          sqlSet "email" $ T.toLower email
          sqlSet "lang"                 l
          sqlSet "deleted"              (Nothing :: Maybe UTCTime)
          sqlSet "associated_domain_id" ad
          sqlSet "user_group_id"        ugid
          sqlSet "home_folder_id"       mFid
          mapM_ sqlResult selectUsersSelectorsList
        fetchMaybe fetchUser

-- | Mark a user as deleted so that queries won't return it anymore and delete
-- sensitive information.
--
-- Attachments, companies and documents purged separately.
data DeleteUser = DeleteUser UserID
instance (MonadDB m, MonadThrow m, MonadTime m) =>
  DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    now   <- currentTime
    muser <- query $ GetUserByID uid
    user  <- case muser of
      Nothing   -> unexpectedError $ "Couldn't find user " <> showt uid
      Just user -> return user

    runQuery_ $ sqlDelete "email_change_requests" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "oauth_access_token" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "oauth_api_token" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "oauth_temp_credential" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "sessions" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "sessions" $ sqlWhereEq "pad_user_id" uid
    runQuery_ $ sqlDelete "user_account_requests" $ sqlWhereEq "user_id" uid
    runQuery_ $ sqlDelete "user_callback_scheme" $ sqlWhereEq "user_id" uid

    -- Give the shared attachments and templates to the oldest admin or user if
    -- there are no admins left.
    runQuery_ $ sqlSelect "users u" $ do
      mapM_ (sqlResult . ("u." <>)) selectUsersSelectorsList
      -- In either the same user group or a subgroup.
      sqlJoinOn "user_groups ug" "u.user_group_id = ug.id"
      sqlWhereEq "ug.id" $ usergroupid user
      sqlWhereIsNULL "u.deleted"
      sqlWhereNotEq "u.id" uid
      sqlOrderBy "u.is_company_admin DESC, u.has_accepted_terms_of_service ASC"
      sqlLimit 1
    mNewOwner <- fetchMaybe fetchUser

    case mNewOwner of
      -- Nothing in case it is the last user. In this particular case, we don't
      -- care about document ownership and we leave the documents and
      -- attachments untouched.
      Nothing       -> return ()
      Just newOwner -> do
        runQuery_ $ sqlUpdate "signatory_links" $ do
          sqlWith "signatory_link_ids_to_change" . sqlUpdate "documents" $ do
            sqlSet "author_user_id" $ userid newOwner
            sqlWhereEq "sharing"        Shared
            sqlWhereEq "status"         Preparation
            sqlWhereEq "type"           Template
            sqlWhereEq "author_user_id" uid
            sqlResult "author_id AS id"

          let
            sqlFieldToChange =
              sqlWhere
                "signatory_link_id IN\
                         \ (SELECT id FROM signatory_link_ids_to_change)"

          sqlWith "_updated_firstnames" . sqlUpdate "signatory_link_fields" $ do
            sqlSet "value_text" $ getFirstName newOwner
            sqlWhereEq "type"       NameFT
            sqlWhereEq "name_order" (1 :: Int)
            sqlFieldToChange

          sqlWith "_updated_lastnames" . sqlUpdate "signatory_link_fields" $ do
            sqlSet "value_text" $ getLastName newOwner
            sqlWhereEq "type"       NameFT
            sqlWhereEq "name_order" (2 :: Int)
            sqlFieldToChange

          sqlWith "_updated_emails" . sqlUpdate "signatory_link_fields" $ do
            sqlSet "value_text" $ getEmail newOwner
            sqlWhereEq "type" EmailFT
            sqlFieldToChange

          sqlWith "_updated_mobiles" . sqlUpdate "signatory_link_fields" $ do
            sqlSet "value_text" $ getMobile newOwner
            sqlWhereEq "type" MobileFT
            sqlFieldToChange

          sqlWith "_updated_personal_numbers" . sqlUpdate "signatory_link_fields" $ do
            sqlSet "value_text" $ getPersonalNumber newOwner
            sqlWhereEq "type" PersonalNumberFT
            sqlFieldToChange

          sqlSet "user_id" $ userid newOwner
          sqlWhere "id IN (SELECT id FROM signatory_link_ids_to_change)"

        runQuery_ $ sqlUpdate "attachments" $ do
          sqlSet "user_id" $ userid newOwner
          sqlWhere "shared"
          sqlWhereEq "user_id" uid

    runQuery_ $ sqlUpdate "users_history" $ do
      sqlSet "event_data" (Nothing :: Maybe String)
      sqlSet "ip"         noIP
      sqlWhereEq "user_id" uid

    runQuery01 $ sqlUpdate "users" $ do
      sqlSet "deleted"          now
      sqlSet "password"         (Nothing :: Maybe ByteString)
      sqlSet "salt"             (Nothing :: Maybe ByteString)
      sqlSet "first_name"       ("" :: String)
      sqlSet "last_name"        ("" :: String)
      sqlSet "personal_number"  ("" :: String)
      sqlSet "company_position" ("" :: String)
      sqlSet "phone"            ("" :: String)
      sqlSet "email"            ("" :: String)
      sqlSet "lang"             (defaultLang :: Lang)
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

-- | Removes user who didn't accept TOS from the database
data RemoveInactiveUser = RemoveInactiveUser UserID
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m RemoveInactiveUser Bool where
  -- There is a chance that a signatory_links gets connected to an
  -- yet not active account the true fix is to not have inactive
  -- accounts, but we are not close to that point yet. Here is a
  -- kludge to get around our own bug.
  update (RemoveInactiveUser uid) = do
    runQuery_ $ "SELECT TRUE FROM companyinvites where user_id = " <?> uid
    ci :: Maybe Bool <- fetchMaybe runIdentity
    if isJust ci
      then return False
      else do
        runQuery_
          $   "UPDATE signatory_links SET user_id = NULL WHERE user_id = "
          <?> uid
          <+> "AND EXISTS (SELECT TRUE FROM users WHERE users.id = "
          <?> uid
          <+> " AND users.has_accepted_terms_of_service IS NULL)"
        runQuery01
          $   "DELETE FROM users WHERE id = "
          <?> uid
          <+> "AND has_accepted_terms_of_service IS NULL"

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance (MonadDB m, MonadThrow m) => DBUpdate m SetSignupMethod Bool where
  update (SetSignupMethod uid signupmethod) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "signup_method" signupmethod
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserUserGroup = SetUserUserGroup UserID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserUserGroup Bool where
  -- also set the new user_group company
  update (SetUserUserGroup uid ugid) = dbQuery (UserGroupGet ugid) >>= \case
    Nothing -> return False
    Just ug -> runQuery01 $ sqlUpdate "users" $ do
      sqlSet "user_group_id" $ ug ^. #ugID
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserGroup = SetUserGroup UserID (Maybe UserGroupID)
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserGroup Bool where
  update (SetUserGroup uid mugid) = runQuery01 $ sqlUpdate "users" $ do
    sqlSet "user_group_id" mugid
    sqlWhereEq "id" uid
    sqlWhereIsNULL "deleted"

data SetUserCompanyAdmin = SetUserCompanyAdmin UserID Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserCompanyAdmin Bool where
  update (SetUserCompanyAdmin uid iscompanyadmin) = do
    runQuery_
      $   "SELECT user_group_id FROM users WHERE id ="
      <?> uid
      <+> "AND deleted IS NULL FOR UPDATE"
    mugid <- fetchMaybe runIdentity
    case mugid :: Maybe UserGroupID of
      Nothing -> return False
      Just _  -> runQuery01 . sqlUpdate "users" $ do
        sqlSet "is_company_admin" iscompanyadmin
        sqlWhereEq "id" uid
        sqlWhereIsNULL "deleted"

data SetUserEmail = SetUserEmail UserID Email
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserEmail Bool where
  update (SetUserEmail uid email) = do
    res <- runQuery01 . sqlUpdate "users" $ do
      sqlSet "email" $ T.toLower $ unEmail email
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    void $ update $ UpdateDraftsAndTemplatesWithUserData uid
    return res

data SetUserInfo = SetUserInfo UserID UserInfo
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserInfo Bool where
  update (SetUserInfo uid info) = do
    res <- runQuery01 . sqlUpdate "users" $ do
      sqlSet "first_name" $ userfstname info
      sqlSet "last_name" $ usersndname info
      sqlSet "personal_number" $ userpersonalnumber info
      sqlSet "company_position" $ usercompanyposition info
      sqlSet "phone" $ userphone info
      sqlSet "email" $ T.toLower $ unEmail $ useremail info
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    void $ update $ UpdateDraftsAndTemplatesWithUserData uid
    return res

data SetUserPassword = SetUserPassword UserID Password
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserPassword Bool where
  update (SetUserPassword uid pwd) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "password" $ pwdHash pwd
      sqlSet "salt" $ pwdSalt pwd
      sqlSet "password_algorithm" $ pwdAlgorithmToInt16 $ pwdAlgorithm $ pwd
      sqlWhereEq "id" uid

data SetUserTOTPKey = SetUserTOTPKey UserID ByteString
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserTOTPKey Bool where
  update (SetUserTOTPKey uid key) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "totp_key" key
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data ConfirmUserTOTPSetup = ConfirmUserTOTPSetup UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m ConfirmUserTOTPSetup Bool where
  update (ConfirmUserTOTPSetup uid) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "totp_active" True
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data DisableUserTOTP = DisableUserTOTP UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DisableUserTOTP Bool where
  update (DisableUserTOTP uid) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSetCmd "totp_key" "NULL"
      sqlSet "totp_active" False
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserTotpIsMandatory = SetUserTotpIsMandatory UserID Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserTotpIsMandatory Bool where
  update (SetUserTotpIsMandatory uid totp_is_mandatory) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "totp_is_mandatory" totp_is_mandatory
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserSettings = SetUserSettings UserID UserSettings
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserSettings Bool where
  update (SetUserSettings uid us) = do
    runQuery01 . sqlUpdate "users" $ do
      let drp = dataretentionpolicy us
      sqlSet "lang" $ getLang us
      sqlSet "idle_doc_timeout_preparation" $ drpIdleDocTimeoutPreparation drp
      sqlSet "idle_doc_timeout_closed" $ drpIdleDocTimeoutClosed drp
      sqlSet "idle_doc_timeout_canceled" $ drpIdleDocTimeoutCanceled drp
      sqlSet "idle_doc_timeout_timedout" $ drpIdleDocTimeoutTimedout drp
      sqlSet "idle_doc_timeout_rejected" $ drpIdleDocTimeoutRejected drp
      sqlSet "idle_doc_timeout_error" $ drpIdleDocTimeoutError drp
      sqlSet "immediate_trash" $ drpImmediateTrash drp
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data UpdateDraftsAndTemplatesWithUserData = UpdateDraftsAndTemplatesWithUserData UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateDraftsAndTemplatesWithUserData () where
  update (UpdateDraftsAndTemplatesWithUserData userid) = do
    muser <- query $ GetUserByID userid
    case muser of
      Nothing   -> return ()
      Just user -> do
        -- Update first name
        runQuery_ $ sqlUpdate "signatory_link_fields" $ do
          sqlSet "value_text" (userfstname $ userinfo user)
          sqlWhereEq "type"       NameFT
          sqlWhereEq "name_order" (NameOrder 1)
          whereSignatoryLinkCanBeChanged

        runQuery_ $ sqlUpdate "signatory_link_fields" $ do
          sqlSet "value_text" (usersndname $ userinfo user)
          sqlWhereEq "type"       NameFT
          sqlWhereEq "name_order" (NameOrder 2)
          whereSignatoryLinkCanBeChanged

        runQuery_ $ sqlUpdate "signatory_link_fields" $ do
          sqlSet "value_text" (useremail $ userinfo user)
          sqlWhereEq "type" EmailFT
          whereSignatoryLinkCanBeChanged
    where
      whereSignatoryLinkCanBeChanged = sqlWhereExists $ sqlSelect "documents" $ do
        sqlJoinOn "signatory_links" "documents.author_id = signatory_links.id"
        sqlWhere "signatory_links.id = signatory_link_fields.signatory_link_id"
        sqlWhereEq "documents.status"        Preparation
        sqlWhereEq "signatory_links.user_id" userid

data SetUserHomeFolder = SetUserHomeFolder UserID FolderID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserHomeFolder Bool where
  update (SetUserHomeFolder userid fdrid) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "home_folder_id" fdrid
      sqlWhereEq "id" userid
      sqlWhereIsNULL "deleted"
