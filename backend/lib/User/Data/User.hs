module User.Data.User
    ( composeFullName
    , fetchUser
    , fetchUserWithUserGroupName
    , selectUsersSQL
    , selectUsersSelectors
    , selectUsersSelectorsList
    , selectUsersWithUserGroupNamesSQL
    , User(..)
    , UserInfo(..)
    , UserSettings(..)
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import Data.Int (Int16)
import Data.String.Utils (strip)
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import Company.Data
import DB
import Log.Identifier
import MinutesTime
import User.Data.SignupMethod
import User.Email
import User.Lang
import User.Password
import User.UserID
import UserGroup.Data
import Util.HasSomeUserInfo

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , usertotp                      :: Maybe ByteString
  , usertotpactive                :: Bool
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe UTCTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , usercompany                   :: CompanyID
  , userassociateddomainid        :: BrandedDomainID
  , usergroupid                   :: UserGroupID
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo User where
  getEmail          = strip . unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo
  getMobile         = userphone          . userinfo

instance Loggable User where
  logValue User{..} = object [
      identifier_ userid
    , "email" .= useremail userinfo
    , "name" .= (userfstname userinfo <> " " <> usersndname userinfo)
    ]
  logDefaultLabel _ = "user"

data UserInfo = UserInfo {
    userfstname         :: String
  , usersndname         :: String
  , userpersonalnumber  :: String
  , usercompanyposition :: String
  , userphone           :: String
  , useremail           :: Email
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo UserInfo where
  getEmail          = strip . unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  getMobile         = userphone

instance Default User where
    def = User {
    userid                        = unsafeUserID 0
  , userpassword                  = Nothing
  , usertotp                      = Nothing
  , usertotpactive                = False
  , useriscompanyadmin            = False
  , useraccountsuspended          = False
  , userhasacceptedtermsofservice = Nothing
  , usersignupmethod              = ByAdmin
  , userinfo                      = def
  , usersettings                  = UserSettings LANG_EN
  , usercompany                   = unsafeCompanyID 0
  , userassociateddomainid        = unsafeBrandedDomainID 0
  , usergroupid                   = emptyUserGroupID
  }

instance Default UserInfo where
    def = UserInfo {
    userfstname         = ""
  , usersndname         = ""
  , userpersonalnumber  = ""
  , usercompanyposition = ""
  , userphone           = ""
  , useremail           = Email ""
  }

data UserSettings  = UserSettings {
    lang                :: Lang
  } deriving (Eq, Ord, Show)

instance HasLang User where
  getLang = getLang . usersettings

instance HasLang UserSettings where
  getLang = lang

selectUsersSelectorsList :: [SQL]
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
  , "email"
  , "lang"
  , "associated_domain_id"
  , "password_algorithm"
  , "totp_key"
  , "totp_active"
  , "user_group_id"
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma selectUsersSelectorsList

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

selectUsersWithUserGroupNamesSQL :: SQL
selectUsersWithUserGroupNamesSQL = "SELECT"
  -- User:
  <> "  users.id AS user_id"
  <> ", users.password"
  <> ", users.salt"
  <> ", users.is_company_admin"
  <> ", users.account_suspended"
  <> ", users.has_accepted_terms_of_service"
  <> ", users.signup_method"
  <> ", users.company_id AS user_company_id"
  <> ", users.first_name"
  <> ", users.last_name"
  <> ", users.personal_number"
  <> ", users.company_position"
  <> ", users.phone"
  <> ", users.email"
  <> ", users.lang"
  <> ", users.associated_domain_id"
  <> ", users.password_algorithm"
  <> ", users.totp_key"
  <> ", users.totp_active"
  <> ", users.user_group_id"
  <> ", ug.name"
  <> "  FROM users"
  <> "  LEFT JOIN user_groups ug ON users.user_group_id = ug.id"
  <> "  WHERE users.deleted IS NULL"

composeFullName :: (String, String) -> String
composeFullName (fstname, sndname) = if null sndname
  then fstname
  else fstname ++ " " ++ sndname

fetchUser :: (UserID, Maybe ByteString, Maybe ByteString, Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID, Maybe Int16, Maybe ByteString, Bool, UserGroupID) -> User
fetchUser (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, password_algorithm, totp_key, totp_active, ugid) = User {
  userid = uid
, userpassword = maybeMkPassword ( password, salt
                                 , int16ToPwdAlgorithm <$> password_algorithm )
, usertotp = totp_key
, usertotpactive = totp_active
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
  , useremail = email
  }
, usersettings = UserSettings { lang = lang }
, usercompany = company_id
, userassociateddomainid = associated_domain_id
, usergroupid = ugid
}

fetchUserWithUserGroupName :: (UserID, Maybe ByteString, Maybe ByteString, Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID, Maybe Int16, Maybe ByteString, Bool, UserGroupID, T.Text) -> (User, T.Text)
fetchUserWithUserGroupName (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, password_algorithm, totp_key, totp_active, ugid, name) = (user, name)
  where
    user = User {
      userid = uid
    , userpassword = maybeMkPassword
                     ( password, salt
                     , int16ToPwdAlgorithm <$> password_algorithm )
    , usertotp = totp_key
    , usertotpactive = totp_active
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
      , useremail = email
      }
    , usersettings = UserSettings { lang = lang }
    , usercompany = company_id
    , userassociateddomainid = associated_domain_id
    , usergroupid = ugid
    }
