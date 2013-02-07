{-# OPTIONS_GHC -fcontext-stack=50 #-}
module User.Model (
    module User.Lang
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
  , GetUserByIDIncludeDeleted(..)
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
  ) where

import Control.Applicative
import Data.Monoid
import Data.Char
import Database.HDBC
import Happstack.Server (FromReqURI(..))

import Company.Model
import DB
import MinutesTime
import User.Lang
import User.Password
import User.UserID
import DB.SQL2
import Doc.DocStateData (DocumentStatus(..))
import Doc.DocumentID
import Utils.Read

-- newtypes
newtype Email = Email { unEmail :: String }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''Email)
$(newtypeDeriveUnderlyingReadShow ''Email)

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''InviteType)

data SignupMethod = AccountRequest | ViralInvitation | BySigning | ByAdmin | CompanyInvitation
  deriving (Eq, Ord, Show, Read)
$(enumDeriveConvertible ''SignupMethod)

instance FromReqURI SignupMethod where
  fromReqURI = maybeRead

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
    lang                :: Lang
  , customfooter        :: Maybe String
  } deriving (Eq, Ord, Show)

instance HasLang User where
  getLang = getLang . usersettings

instance HasLang UserSettings where
  getLang = lang


data UserFilter
  = UserFilterByString String             -- ^ Contains the string in name, email or anywhere


userFilterToSQL :: UserFilter -> SQL
userFilterToSQL (UserFilterByString string) =
    sqlConcatAND (map (\wordpat -> SQL "users.first_name ILIKE ?" [wordpat] `sqlOR`
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
    kRun_ $ selectUsersSQL <+> "WHERE deleted = FALSE ORDER BY first_name || ' ' || last_name DESC"
    fetchUsers

data GetUserByID = GetUserByID UserID
instance MonadDB m => DBQuery m GetUserByID (Maybe User) where
  query (GetUserByID uid) = do
    kRun_ $ selectUsersSQL <+> "WHERE id =" <?> uid <+> "AND deleted = FALSE"
    fetchUsers >>= oneObjectReturnedGuard

data GetUserByIDIncludeDeleted = GetUserByIDIncludeDeleted UserID
instance MonadDB m => DBQuery m GetUserByIDIncludeDeleted (Maybe User) where
  query (GetUserByIDIncludeDeleted uid) = do
    kRun_ $ selectUsersSQL <+> "WHERE id =" <?> uid
    fetchUsers >>= oneObjectReturnedGuard

data GetUserByEmail = GetUserByEmail Email
instance MonadDB m => DBQuery m GetUserByEmail (Maybe User) where
  query (GetUserByEmail email) = do
    kRun_ $ selectUsersSQL <+> "WHERE deleted = FALSE AND email =" <?> map toLower (unEmail email)
    fetchUsers >>= oneObjectReturnedGuard

data GetCompanyAccounts = GetCompanyAccounts CompanyID
instance MonadDB m => DBQuery m GetCompanyAccounts [User] where
  query (GetCompanyAccounts cid) = do
    kRun_ $ selectUsersSQL <+> "WHERE company_id =" <?> cid <+> "AND deleted = FALSE ORDER BY email DESC"
    fetchUsers

data GetInviteInfo = GetInviteInfo UserID
instance MonadDB m => DBQuery m GetInviteInfo (Maybe InviteInfo) where
  query (GetInviteInfo uid) = do
    kRun_ $ SQL "SELECT inviter_id, invite_time, invite_type FROM user_invite_infos WHERE user_id = ?"
            [toSql uid]
    kFold fetchInviteInfos [] >>= oneObjectReturnedGuard
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
      kRun01 $ SQL "UPDATE users SET company_id = NULL, is_company_admin = FALSE WHERE id = ? AND deleted = FALSE"
               [toSql uid]
    Just cid -> do
      kRun01 $ SQL "UPDATE users SET company_id = ? WHERE id = ? AND deleted = FALSE"
               [toSql cid, toSql uid]

data IsUserDeletable = IsUserDeletable UserID
instance MonadDB m => DBQuery m IsUserDeletable Bool where
  query (IsUserDeletable uid) = do
    kRun_ $ sqlSelect "users" $ do
      sqlWhereEq "users.deleted" False
      sqlWhereEq "users.id" uid
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhereEq "signatory_links.deleted" False
      sqlWhere "signatory_links.is_author"
      sqlJoinOn "documents" "documents.id = signatory_links.document_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    (results :: [DocumentID]) <- kFold (flip (:)) []
    return (null results)

-- | Marks a user as deleted so that queries won't return them any more.
-- TODO: change deleted to time
data DeleteUser = DeleteUser UserID
instance MonadDB m => DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    kRun01 $ SQL "UPDATE users SET deleted = ? WHERE id = ? AND deleted = FALSE"
             [toSql True, toSql uid]

-- | Removes user who didn't accept TOS from the database
data RemoveInactiveUser = RemoveInactiveUser UserID
instance MonadDB m => DBUpdate m RemoveInactiveUser Bool where
  update (RemoveInactiveUser uid) = do
    -- There is a chance that a signatory_links gets connected to an
    -- yet not active account the true fix is to not have inactive
    -- accounts, but we are not close to that point yet. Here is a
    -- kludge to get around our own bug.
    kRun_ $ "UPDATE signatory_links SET user_id = NULL WHERE user_id = " <?> uid <+> "AND EXISTS (SELECT TRUE FROM users WHERE users.id = signatory_links.user_id AND users.has_accepted_terms_of_service IS NULL)"
    kRun01 $ "DELETE FROM users WHERE id = " <?> uid <+> "AND has_accepted_terms_of_service IS NULL"

data AddUser = AddUser (String, String) String (Maybe Password) (Maybe CompanyID) Lang
instance MonadDB m => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd mcid l) = do
    mu <- query $ GetUserByEmail $ Email email
    case mu of
      Just _ -> return Nothing -- user with the same email address exists
      Nothing -> do
        kRun_ $ sqlInsert "users" $ do
            sqlSet "password" $ pwdHash <$> mpwd
            sqlSet "salt" $ pwdSalt <$> mpwd
            sqlSet "is_company_admin" False
            sqlSet "account_suspended" False
            sqlSet "has_accepted_terms_of_service" SqlNull
            sqlSet "signup_method" AccountRequest
            sqlSet "company_id" mcid
            sqlSet "first_name" fname
            sqlSet "last_name" lname
            sqlSet "personal_number" ("" :: String)
            sqlSet "company_position" ("" :: String)
            sqlSet "company_name" ("" :: String)
            sqlSet "company_number" ("" :: String)
            sqlSet "phone" ("" :: String)
            sqlSet "mobile" ("" :: String)
            sqlSet "email" $ map toLower email
            sqlSet "lang" l
            sqlSet "deleted" False
            sqlSet "is_free" False
            mapM_ (sqlResult . raw) selectUsersSelectorsList
        fetchUsers >>= oneObjectReturnedGuard

data SetUserEmail = SetUserEmail UserID Email
instance MonadDB m => DBUpdate m SetUserEmail Bool where
  update (SetUserEmail uid email) = do
    kRun01 $ SQL ("UPDATE users SET email = ?"
                  <> " WHERE id = ? AND deleted = FALSE")
             [toSql $ map toLower $ unEmail email, toSql uid]

data SetUserPassword = SetUserPassword UserID Password
instance MonadDB m => DBUpdate m SetUserPassword Bool where
  update (SetUserPassword uid pwd) = do
    kRun01 $ SQL ("UPDATE users SET"
                  <> "  password = ?"
                  <> ", salt = ?"
                  <> "  WHERE id = ? AND deleted = FALSE")
             [toSql $ pwdHash pwd, toSql $ pwdSalt pwd, toSql uid]

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
                kRun01 $ SQL ("UPDATE user_invite_infos SET"
                              <> "  inviter_id = ?"
                              <> ", invite_time = ?"
                              <> ", invite_type = ?"
                              <> "  WHERE user_id = ?")
                         [ toSql inviterid
                         , toSql invitetime
                         , toSql invitetype
                         , toSql uid
                         ]
              else do
                kRun01 $ SQL ("INSERT INTO user_invite_infos ("
                              <> "  user_id"
                              <> ", inviter_id"
                              <> ", invite_time"
                              <> ", invite_type) VALUES (?, ?, ?, ?)")
                              [ toSql uid
                              , toSql inviterid
                              , toSql invitetime
                              , toSql invitetype
                              ]
          Nothing -> do
            kRun01 $ SQL ("DELETE FROM user_invite_infos WHERE user_id = ?")
                     [toSql uid]
      else return False

data SetUserInfo = SetUserInfo UserID UserInfo
instance MonadDB m => DBUpdate m SetUserInfo Bool where
  update (SetUserInfo uid info) = do
    kRun01 $ SQL ("UPDATE users SET"
                  <> "  first_name = ?"
                  <> ", last_name = ?"
                  <> ", personal_number = ?"
                  <> ", company_position = ?"
                  <> ", phone = ?"
                  <> ", mobile = ?"
                  <> ", email = ?"
                  <> ", company_name = ?"
                  <> ", company_number = ?"
                  <> "  WHERE id = ? AND deleted = FALSE")
             [ toSql $ userfstname info
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
    kRun01 $ SQL ("UPDATE users SET"
                  <> "  lang = ?"
                  <> ", customfooter = ?"
                  <> "  WHERE id = ? AND deleted = FALSE")
             [ toSql $ getLang us
             , toSql $ customfooter us
             , toSql uid
             ]

data AcceptTermsOfService = AcceptTermsOfService UserID MinutesTime
instance MonadDB m => DBUpdate m AcceptTermsOfService Bool where
  update (AcceptTermsOfService uid time) = do
    kRun01 $ SQL ("UPDATE users SET"
                  <> "  has_accepted_terms_of_service = ?"
                  <> "  WHERE id = ? AND deleted = FALSE")
             [ toSql time
             , toSql uid
             ]

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance MonadDB m => DBUpdate m SetSignupMethod Bool where
  update (SetSignupMethod uid signupmethod) = do
    kRun01 $ SQL ("UPDATE users SET signup_method = ? WHERE id = ? AND deleted = FALSE")
           [toSql signupmethod, toSql uid]

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

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

selectUsersSelectorsList :: [RawSQL]
selectUsersSelectorsList =
  [ "id"
  , "password"
  , "salt"
  , "is_company_admin"
  , "account_suspended"
  , "has_accepted_terms_of_service"
  , "signup_method"
  , "company_id"
  , "first_name"
  , "last_name"
  , "personal_number"
  , "company_position"
  , "phone"
  , "mobile"
  , "email"
  , "lang"
  , "customfooter"
  , "company_name"
  , "company_number"
  , "is_free"
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma (map raw selectUsersSelectorsList)

fetchUsers :: MonadDB m => DBEnv m [User]
fetchUsers = kFold decoder []
  where
    -- Note: this function gets users in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc uid password salt is_company_admin account_suspended
      has_accepted_terms_of_service signup_method company_id
      first_name last_name personal_number company_position phone mobile
      email lang customfooter
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
            lang = lang
          , customfooter = customfooter
          }
        , usercompany = company_id
        , userisfree = is_free
        } : acc
