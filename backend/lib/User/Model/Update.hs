module User.Model.Update (
    AddUser(..)
  , AcceptTermsOfService(..)
  , DeleteUser(..)
  , RemoveInactiveUser(..)
  , SetSignupMethod(..)
  , SetUserUserGroup(..)
  , SetUserGroup(..)
  , SetUserEmail(..)
  , SetUserCompanyAdmin(..)
  , SetUserInfo(..)
  , SetUserPassword(..)
  , SetUserTOTPKey(..)
  , ConfirmUserTOTPSetup(..)
  , DisableUserTOTP(..)
  , SetUserSettings(..)
  , MakeUserPartnerAdmin(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.ByteString (ByteString)
import Data.Char
import Log

import BrandedDomain.BrandedDomainID
import DB
import Doc.Data.SignatoryField
import Doc.DocStateData (DocumentStatus(..))
import Log.Identifier
import User.Data.SignupMethod
import User.Data.User
import User.Email
import User.Lang
import User.Model.Query
import User.Password
import User.UserID
import UserGroup.Data
import UserGroup.Model

data AcceptTermsOfService = AcceptTermsOfService UserID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m AcceptTermsOfService Bool where
  update (AcceptTermsOfService uid time) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "has_accepted_terms_of_service" time
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data AddUser = AddUser (String, String) String (Maybe Password) (UserGroupID, Bool) Lang BrandedDomainID SignupMethod
instance (MonadDB m, MonadThrow m) => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd (ugid, admin) l ad sm) = do
    mu <- query $ GetUserByEmail $ Email email
    case mu of
      Just _ -> return Nothing -- user with the same email address exists
      Nothing -> do
        runQuery_ $ sqlInsert "users" $ do
            sqlSet "password" $ pwdHash <$> mpwd
            sqlSet "salt" $ pwdSalt <$> mpwd
            sqlSet "password_algorithm" $
              pwdAlgorithmToInt16 . pwdAlgorithm <$> mpwd
            sqlSet "is_company_admin" admin
            sqlSet "account_suspended" False
            sqlSet "has_accepted_terms_of_service" (Nothing :: Maybe UTCTime)
            sqlSet "signup_method" sm
            sqlSet "first_name" fname
            sqlSet "last_name" lname
            sqlSet "personal_number" ("" :: String)
            sqlSet "company_position" ("" :: String)
            sqlSet "phone" ("" :: String)
            sqlSet "email" $ map toLower email
            sqlSet "lang" l
            sqlSet "deleted" (Nothing :: Maybe UTCTime)
            sqlSet "associated_domain_id" ad
            sqlSet "user_group_id" ugid
            mapM_ sqlResult selectUsersSelectorsList
        fetchMaybe fetchUser

-- | Marks a user as deleted so that queries won't return them any more.
data DeleteUser = DeleteUser UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    now <- currentTime
    runQuery01 $ sqlUpdate "users" $ do
      sqlSet "deleted" now
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

-- | Removes user who didn't accept TOS from the database
data RemoveInactiveUser = RemoveInactiveUser UserID
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m RemoveInactiveUser Bool where
  update (RemoveInactiveUser uid) = do
    -- There is a chance that a signatory_links gets connected to an
    -- yet not active account the true fix is to not have inactive
    -- accounts, but we are not close to that point yet. Here is a
    -- kludge to get around our own bug.
    runQuery_ $ "SELECT TRUE FROM companyinvites where user_id = " <?> uid
    ci :: Maybe Bool <- fetchMaybe runIdentity
    if isJust ci then
      return False
    else do
      runQuery_ $ "UPDATE signatory_links SET user_id = NULL WHERE user_id = " <?> uid <+> "AND EXISTS (SELECT TRUE FROM users WHERE users.id = " <?> uid <+> " AND users.has_accepted_terms_of_service IS NULL)"
      runQuery01 $ "DELETE FROM users WHERE id = " <?> uid <+> "AND has_accepted_terms_of_service IS NULL"

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance (MonadDB m, MonadThrow m) => DBUpdate m SetSignupMethod Bool where
  update (SetSignupMethod uid signupmethod) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "signup_method" signupmethod
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserUserGroup = SetUserUserGroup UserID UserGroupID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserUserGroup Bool where
  update (SetUserUserGroup uid ugid) =
    -- also set the new user_group company
    dbQuery (UserGroupGet ugid) >>= \case
      Nothing -> return False
      Just ug ->
        runQuery01 $ sqlUpdate "users" $ do
          sqlSet "user_group_id" . get ugID $ ug
          sqlWhereEq "id" uid
          sqlWhereIsNULL "deleted"

data SetUserGroup = SetUserGroup UserID (Maybe UserGroupID)
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserGroup Bool where
  update (SetUserGroup uid mugid) =
    runQuery01 $ sqlUpdate "users" $ do
      sqlSet "user_group_id" mugid
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserCompanyAdmin = SetUserCompanyAdmin UserID Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserCompanyAdmin Bool where
  update (SetUserCompanyAdmin uid iscompanyadmin) = do
    runQuery_ $ "SELECT user_group_id FROM users WHERE id =" <?> uid <+> "AND deleted IS NULL FOR UPDATE"
    mugid <- fetchMaybe runIdentity
    case mugid :: Maybe UserGroupID of
      Nothing -> return False
      Just _ -> runQuery01 . sqlUpdate "users" $ do
        sqlSet "is_company_admin" iscompanyadmin
        sqlWhereEq "id" uid
        sqlWhereIsNULL "deleted"

data SetUserEmail = SetUserEmail UserID Email
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserEmail Bool where
  update (SetUserEmail uid email) = do
    res <- runQuery01 . sqlUpdate "users" $ do
      sqlSet "email" $ map toLower $ unEmail email
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    _ <- update $ UpdateDraftsAndTemplatesWithUserData uid
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
      sqlSet "email" $ map toLower $ unEmail $ useremail info
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    _ <- update $ UpdateDraftsAndTemplatesWithUserData uid
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

data SetUserSettings = SetUserSettings UserID UserSettings
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserSettings Bool where
  update (SetUserSettings uid us) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "lang" $ getLang us
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data UpdateDraftsAndTemplatesWithUserData = UpdateDraftsAndTemplatesWithUserData UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateDraftsAndTemplatesWithUserData () where
 update (UpdateDraftsAndTemplatesWithUserData userid) = do
   muser <- query $ GetUserByID userid
   case muser of
     Nothing -> return ()
     Just user -> do
       -- Update first name
       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value_text" (userfstname $ userinfo user)
         sqlWhereEq "type" NameFT
         sqlWhereEq "name_order" (NameOrder 1)
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value_text" (usersndname $ userinfo user)
         sqlWhereEq "type" NameFT
         sqlWhereEq "name_order" (NameOrder 2)
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value_text" (useremail $ userinfo user)
         sqlWhereEq "type" EmailFT
         whereSignatoryLinkCanBeChanged
  where
    whereSignatoryLinkCanBeChanged =
      sqlWhereExists $ sqlSelect "documents" $ do
        sqlJoinOn "signatory_links" "documents.author_id = signatory_links.id"
        sqlWhere "signatory_links.id = signatory_link_fields.signatory_link_id"
        sqlWhereEq "documents.status" Preparation
        sqlWhereEq "signatory_links.user_id" userid

-- this is only for testing.
data MakeUserPartnerAdmin = MakeUserPartnerAdmin UserID UserGroupID
instance (MonadDB m, MonadThrow m, MonadLog m) => DBUpdate m MakeUserPartnerAdmin Bool where
  update (MakeUserPartnerAdmin uid ugid) = do
    -- guard that the user is member of that "partner" user group
    user_ug <- dbQuery . UserGroupGetByUserID $ uid
    case (ugid == get ugID user_ug) of
      False -> do
        logAttention "User is not member of this usergroup" $ object [
              identifier_ uid
            , identifier_ ugid
            ]
        return False
      True ->
        runQuery01 . sqlInsert "partner_admins" $ do
          sqlSet "user_id" uid
          sqlSet "partner_id" ugid
