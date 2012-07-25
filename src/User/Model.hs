{-# OPTIONS_GHC -fcontext-stack=50 #-}
module User.Model (
    module User.Lang
  , module User.Region
  , module User.Locale
  , module User.Password
  , module User.UserID
  , Email(..)
  , InviteType(..)
  , SignupMethod(..)
  , InviteInfo(..)
  , User(..)
  , UserInfo(..)
  , UserSettings(..)
  , GetUsers(..)
  , GetUserByID(..)
  , GetUserByEmail(..)
  , GetCompanyAccounts(..)
  , GetInviteInfo(..)
  , SetUserCompany(..)
  , DeleteUser(..)
  , RemoveInactiveUser(..)
  , AddUser(..)
  , SetUserEmail(..)
  , SetUserPassword(..)
  , SetInviteInfo(..)
  , SetUserInfo(..)
  , SetUserSettings(..)
  , AcceptTermsOfService(..)
  , SetSignupMethod(..)
  , SetUserCompanyAdmin(..)
  , UserFilter(..)
  , SetUserIsFree(..)

  , IsUserDeletable(..)
  , composeFullName
  , userFilterToSQL

  , UserOrderBy(..)
  , userOrderByToSQL
  , userOrderByAscDescToSQL
  , UserPagination(..)
  ) where

import Control.Applicative
import Data.Monoid
import Data.List
import Data.Char
import Database.HDBC

import API.Service.Model
import Company.Model
import DB
import MinutesTime
import User.Lang
import User.Locale
import User.Password
import User.Region
import User.Tables
import User.UserID
import DB.SQL2
import Doc.DocStateData (DocumentStatus(..), SignatoryRole(..), DocumentID)

-- newtypes
newtype Email = Email { unEmail :: String }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''Email)
$(newtypeDeriveUnderlyingReadShow ''Email)

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''InviteType)

data SignupMethod = AccountRequest | ViralInvitation | BySigning
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignupMethod)

-- data structures
data InviteInfo = InviteInfo {
    userinviter :: UserID
  , invitetime  :: Maybe MinutesTime
  , invitetype  :: Maybe InviteType
  } deriving (Eq, Ord, Show)

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe MinutesTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , userservice                   :: Maybe ServiceID
  , usercompany                   :: Maybe CompanyID
  , userisfree          :: Bool
  } deriving (Eq, Ord, Show)

data UserInfo = UserInfo {
    userfstname         :: String
  , usersndname         :: String
  , userpersonalnumber  :: String
  , usercompanyposition :: String
  , userphone           :: String
  , usermobile          :: String
  , useremail           :: Email
  , usercompanyname     :: String
  , usercompanynumber   :: String
  } deriving (Eq, Ord, Show)

data UserSettings  = UserSettings {
    locale              :: Locale
  , customfooter        :: Maybe String
  } deriving (Eq, Ord, Show)

instance HasLocale User where
  getLocale = getLocale . usersettings

instance HasLocale UserSettings where
  getLocale = locale


sqlOR :: SQL -> SQL -> SQL
sqlOR sql1 sql2 = mconcat [parenthesize sql1, SQL " OR " [], parenthesize sql2]


sqlJoinWith :: SQL -> [SQL] -> SQL
sqlJoinWith comm list = mconcat $ intersperse comm $ map parenthesize list


sqlJoinWithAND :: [SQL] -> SQL
sqlJoinWithAND = sqlJoinWith (SQL " AND " [])

parenthesize :: SQL -> SQL
parenthesize (SQL command values) = SQL ("(" ++ command ++ ")") values

data UserFilter
  = UserFilterByString String             -- ^ Contains the string in name, email or anywhere


userFilterToSQL :: UserFilter -> SQL
userFilterToSQL (UserFilterByString string) =
  sqlJoinWithAND (map (\wordpat -> SQL "users.first_name ILIKE ?" [wordpat] `sqlOR`
                                   SQL "users.last_name ILIKE ?" [wordpat] `sqlOR`
                                   SQL "users.email ILIKE ?" [wordpat] `sqlOR`
                                   SQL "translate(users.mobile,'-+ .,()','') ILIKE translate(?,'-+ .,()','')" [wordpat] `sqlOR`
                                   SQL "translate(users.personal_number,'-+ .,()','') ILIKE translate(?,'-+ .,()','')" [wordpat]
                      ) sqlwordpat)
  where
      sqlwordpat = map (\word -> toSql $ "%" ++ concatMap escape word ++ "%") (words string)
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]

data UserPagination =
  UserPagination
  { userOffset :: Int        -- ^ use for SQL OFFSET command
  , userLimit  :: Int        -- ^ use for SQL LIMIT command
  }


data UserOrderBy
  = UserOrderByName
  | UserOrderByCompanyName
  | UserOrderByEmail
  | UserOrderByAccountCreationDate

-- | Convert UserOrderBy enumeration into proper SQL order by statement
userOrderByToSQL :: UserOrderBy -> SQL
userOrderByToSQL UserOrderByName                = SQL "(users.first_name || ' ' || users.last_name)" []
userOrderByToSQL UserOrderByCompanyName         = SQL "users.company_name" []
userOrderByToSQL UserOrderByEmail               = SQL "users.email" []
userOrderByToSQL UserOrderByAccountCreationDate = SQL "users.has_accepted_terms_of_service" []

userOrderByAscDescToSQL :: AscDesc UserOrderBy -> SQL
userOrderByAscDescToSQL (Asc x@UserOrderByAccountCreationDate) = userOrderByToSQL x `mappend` SQL " ASC NULLS FIRST " []
userOrderByAscDescToSQL (Desc x@UserOrderByAccountCreationDate) = userOrderByToSQL x `mappend` SQL " DESC NULLS LAST " []
userOrderByAscDescToSQL (Asc x) = userOrderByToSQL x
userOrderByAscDescToSQL (Desc x) = userOrderByToSQL x `mappend` SQL " DESC" []

data GetUsers = GetUsers
instance MonadDB m => DBQuery m GetUsers [User] where
  query GetUsers = do
    kPrepare $ selectUsersSQL ++ " WHERE deleted = FALSE ORDER BY first_name || ' ' || last_name DESC"
    _ <- kExecute []
    fetchUsers

data GetUserByID = GetUserByID UserID
instance MonadDB m => DBQuery m GetUserByID (Maybe User) where
  query (GetUserByID uid) = do
    kPrepare $ selectUsersSQL ++ " WHERE id = ? AND deleted = FALSE"
    _ <- kExecute [toSql uid]
    fetchUsers >>= oneObjectReturnedGuard

data GetUserByEmail = GetUserByEmail (Maybe ServiceID) Email
instance MonadDB m => DBQuery m GetUserByEmail (Maybe User) where
  query (GetUserByEmail msid email) = do
    kPrepare $ selectUsersSQL ++ " WHERE deleted = FALSE AND service_id IS NOT DISTINCT FROM ? AND email = ?"
    _ <- kExecute [toSql msid, toSql $ map toLower $ unEmail email]
    fetchUsers >>= oneObjectReturnedGuard

data GetCompanyAccounts = GetCompanyAccounts CompanyID
instance MonadDB m => DBQuery m GetCompanyAccounts [User] where
  query (GetCompanyAccounts cid) = do
    kPrepare $ selectUsersSQL ++ " WHERE company_id = ? AND deleted = FALSE ORDER BY email DESC"
    _ <- kExecute [toSql cid]
    fetchUsers

data GetInviteInfo = GetInviteInfo UserID
instance MonadDB m => DBQuery m GetInviteInfo (Maybe InviteInfo) where
  query (GetInviteInfo uid) = do
    kPrepare "SELECT inviter_id, invite_time, invite_type FROM user_invite_infos WHERE user_id = ?"
    _ <- kExecute [toSql uid]
    foldDB fetchInviteInfos [] >>= oneObjectReturnedGuard
    where
      fetchInviteInfos acc inviter_id invite_time invite_type = InviteInfo {
          userinviter = inviter_id
        , invitetime = invite_time
        , invitetype = invite_type
        } : acc

data SetUserCompany = SetUserCompany UserID (Maybe CompanyID)
instance MonadDB m => DBUpdate m SetUserCompany Bool where
  update (SetUserCompany uid mcid) = case mcid of
    Nothing -> do
      kPrepare "UPDATE users SET company_id = NULL, is_company_admin = FALSE WHERE id = ? AND deleted = FALSE"
      kExecute01 [toSql uid]
    Just cid -> do
      kPrepare "UPDATE users SET company_id = ? WHERE id = ? AND deleted = FALSE"
      kExecute01 [toSql cid, toSql uid]

data IsUserDeletable = IsUserDeletable UserID
instance MonadDB m => DBQuery m IsUserDeletable Bool where
  query (IsUserDeletable uid) = do
    kRun_ $ sqlSelect "users" $ do
      sqlWhereEq "users.deleted" False
      sqlWhereEq "users.id" uid
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhereEq "signatory_links.deleted" False
      sqlWhere $ SQL "(signatory_links.roles & ?) <> 0" [toSql [SignatoryAuthor]]
      sqlJoinOn "documents" "documents.id = signatory_links.document_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    (results :: [DocumentID]) <- foldDB (flip (:)) []
    return (null results)

-- | Marks a user as deleted so that queries won't return them any more.
-- TODO: change deleted to time
data DeleteUser = DeleteUser UserID
instance MonadDB m => DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    kPrepare $ "UPDATE users SET deleted = ? WHERE id = ? AND deleted = FALSE"
    kExecute01 [toSql True, toSql uid]

-- | Removes user who didn't accept TOS from the database
data RemoveInactiveUser = RemoveInactiveUser UserID
instance MonadDB m => DBUpdate m RemoveInactiveUser Bool where
  update (RemoveInactiveUser uid) = kRun01 $ SQL "DELETE FROM users WHERE deleted = FALSE AND id = ? AND has_accepted_terms_of_service IS NULL" [toSql uid]

data AddUser = AddUser (String, String) String (Maybe Password) (Maybe ServiceID) (Maybe CompanyID) Locale
instance MonadDB m => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd msid mcid l) = do
    mu <- query $ GetUserByEmail msid $ Email email
    case mu of
      Just _ -> return Nothing -- user with the same email address exists
      Nothing -> do
        _ <- kRun $ mkSQL INSERT tableUsers [
            sql "password" $ pwdHash <$> mpwd
          , sql "salt" $ pwdSalt <$> mpwd
          , sql "is_company_admin" False
          , sql "account_suspended" False
          , sql "has_accepted_terms_of_service" SqlNull
          , sql "signup_method" AccountRequest
          , sql "service_id" msid
          , sql "company_id" mcid
          , sql "first_name" fname
          , sql "last_name" lname
          , sql "personal_number" ""
          , sql "company_position" ""
          , sql "company_name" ""
          , sql "company_number" ""
          , sql "phone" ""
          , sql "mobile" ""
          , sql "email" $ map toLower email
          , sql "lang" $ getLang l
          , sql "region" $ getRegion l
          , sql "deleted" False
          , sql "is_free" False
          ] <> SQL ("RETURNING " ++ selectUsersSelectors) []
        fetchUsers >>= oneObjectReturnedGuard

data SetUserEmail = SetUserEmail (Maybe ServiceID) UserID Email
instance MonadDB m => DBUpdate m SetUserEmail Bool where
  update (SetUserEmail msid uid email) = do
    kPrepare $ "UPDATE users SET email = ?"
      ++ " WHERE id = ? AND deleted = FALSE AND service_id IS NOT DISTINCT FROM ?"
    kExecute01 [toSql $ map toLower $ unEmail email, toSql uid, toSql msid]

data SetUserPassword = SetUserPassword UserID Password
instance MonadDB m => DBUpdate m SetUserPassword Bool where
  update (SetUserPassword uid pwd) = do
    kPrepare $ "UPDATE users SET"
      ++ "  password = ?"
      ++ ", salt = ?"
      ++ "  WHERE id = ? AND deleted = FALSE"
    kExecute01 [toSql $ pwdHash pwd, toSql $ pwdSalt pwd, toSql uid]

data SetInviteInfo = SetInviteInfo (Maybe UserID) MinutesTime InviteType UserID
instance MonadDB m => DBUpdate m SetInviteInfo Bool where
  update (SetInviteInfo minviterid invitetime invitetype uid) = do
    exists <- checkIfUserExists uid
    if exists
      then do
        case minviterid of
          Just inviterid -> do
            _ <- kRunRaw "LOCK TABLE user_invite_infos IN ACCESS EXCLUSIVE MODE"
            rec_exists <- checkIfAnyReturned $ SQL "SELECT 1 FROM user_invite_infos WHERE user_id = ?" [toSql uid]
            if rec_exists
              then do
                kPrepare $ "UPDATE user_invite_infos SET"
                  ++ "  inviter_id = ?"
                  ++ ", invite_time = ?"
                  ++ ", invite_type = ?"
                  ++ "  WHERE user_id = ?"
                kExecute01 [
                    toSql inviterid
                  , toSql invitetime
                  , toSql invitetype
                  , toSql uid
                  ]
              else do
                kPrepare $ "INSERT INTO user_invite_infos ("
                  ++ "  user_id"
                  ++ ", inviter_id"
                  ++ ", invite_time"
                  ++ ", invite_type) VALUES (?, ?, ?, ?)"
                kExecute01 [
                    toSql uid
                  , toSql inviterid
                  , toSql invitetime
                  , toSql invitetype
                  ]
          Nothing -> do
            kPrepare "DELETE FROM user_invite_infos WHERE user_id = ?"
            kExecute01 [toSql uid]
      else return False

data SetUserInfo = SetUserInfo UserID UserInfo
instance MonadDB m => DBUpdate m SetUserInfo Bool where
  update (SetUserInfo uid info) = do
    kPrepare $ "UPDATE users SET"
      ++ "  first_name = ?"
      ++ ", last_name = ?"
      ++ ", personal_number = ?"
      ++ ", company_position = ?"
      ++ ", phone = ?"
      ++ ", mobile = ?"
      ++ ", email = ?"
      ++ ", company_name = ?"
      ++ ", company_number = ?"
      ++ "  WHERE id = ? AND deleted = FALSE"
    kExecute01 [
        toSql $ userfstname info
      , toSql $ usersndname info
      , toSql $ userpersonalnumber info
      , toSql $ usercompanyposition info
      , toSql $ userphone info
      , toSql $ usermobile info
      , toSql $ map toLower $ unEmail $ useremail info
      , toSql $ usercompanyname info
      , toSql $ usercompanynumber info
      , toSql uid
      ]

data SetUserSettings = SetUserSettings UserID UserSettings
instance MonadDB m => DBUpdate m SetUserSettings Bool where
  update (SetUserSettings uid us) = do
    kPrepare $ "UPDATE users SET"
      ++ "  lang = ?"
      ++ ", region = ?"
      ++ ", customfooter = ?"
      ++ "  WHERE id = ? AND deleted = FALSE"
    kExecute01 [
        toSql $ getLang us
      , toSql $ getRegion us
      , toSql $ customfooter us
      , toSql uid
      ]

data AcceptTermsOfService = AcceptTermsOfService UserID MinutesTime
instance MonadDB m => DBUpdate m AcceptTermsOfService Bool where
  update (AcceptTermsOfService uid time) = do
    kPrepare $ "UPDATE users SET"
      ++ "  has_accepted_terms_of_service = ?"
      ++ "  WHERE id = ? AND deleted = FALSE"
    kExecute01 [
        toSql time
      , toSql uid
      ]

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance MonadDB m => DBUpdate m SetSignupMethod Bool where
  update (SetSignupMethod uid signupmethod) = do
    kPrepare "UPDATE users SET signup_method = ? WHERE id = ? AND deleted = FALSE"
    kExecute01 [toSql signupmethod, toSql uid]

data SetUserCompanyAdmin = SetUserCompanyAdmin UserID Bool
instance MonadDB m => DBUpdate m SetUserCompanyAdmin Bool where
  update (SetUserCompanyAdmin uid iscompanyadmin) = do
    mcid <- getOne $ SQL "SELECT company_id FROM users WHERE id = ? AND deleted = FALSE FOR UPDATE" [toSql uid]
    case mcid :: Maybe CompanyID of
      Nothing -> return False
      Just _ -> kRun01 $ SQL
        "UPDATE users SET is_company_admin = ? WHERE id = ? AND deleted = FALSE"
        [toSql iscompanyadmin, toSql uid]

data SetUserIsFree = SetUserIsFree UserID Bool
instance MonadDB m => DBUpdate m SetUserIsFree Bool where
  update (SetUserIsFree uid isfree) = do
    kRun01 $ SQL "UPDATE users SET is_free = ? WHERE id = ? AND deleted = FALSE"
      [toSql isfree, toSql uid]

-- helpers

composeFullName :: (String, String) -> String
composeFullName (fstname, sndname) = if null sndname
  then fstname
  else fstname ++ " " ++ sndname

checkIfUserExists :: MonadDB m => UserID -> DBEnv m Bool
checkIfUserExists uid = checkIfAnyReturned
  $ SQL "SELECT 1 FROM users WHERE id = ? AND deleted = FALSE" [toSql uid]

selectUsersSQL :: String
selectUsersSQL = "SELECT " ++ selectUsersSelectors ++ " FROM users"

selectUsersSelectors :: String
selectUsersSelectors =
 "  id"
 ++ ", password"
 ++ ", salt"
 ++ ", is_company_admin"
 ++ ", account_suspended"
 ++ ", has_accepted_terms_of_service"
 ++ ", signup_method"
 ++ ", service_id"
 ++ ", company_id"
 ++ ", first_name"
 ++ ", last_name"
 ++ ", personal_number"
 ++ ", company_position"
 ++ ", phone"
 ++ ", mobile"
 ++ ", email"
 ++ ", lang"
 ++ ", region"
 ++ ", customfooter"
 ++ ", company_name"
 ++ ", company_number"
 ++ ", is_free"

fetchUsers :: MonadDB m => DBEnv m [User]
fetchUsers = foldDB decoder []
  where
    -- Note: this function gets users in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc uid password salt is_company_admin account_suspended
      has_accepted_terms_of_service signup_method service_id company_id
      first_name last_name personal_number company_position phone mobile
      email lang region customfooter
      company_name company_number is_free = User {
          userid = uid
        , userpassword = maybePassword (password, salt)
        , useriscompanyadmin = is_company_admin
        , useraccountsuspended = account_suspended
        , userhasacceptedtermsofservice = has_accepted_terms_of_service
        , usersignupmethod = signup_method
        , userinfo = UserInfo {
            userfstname = first_name
          , usersndname = last_name
          , userpersonalnumber = personal_number
          , usercompanyposition = company_position
          , userphone = phone
          , usermobile = mobile
          , useremail = email
          , usercompanyname  = company_name
          , usercompanynumber = company_number
          }
        , usersettings = UserSettings {
            locale = mkLocale region lang
          , customfooter = customfooter
          }
        , userservice = service_id
        , usercompany = company_id
        , userisfree = is_free
        } : acc
