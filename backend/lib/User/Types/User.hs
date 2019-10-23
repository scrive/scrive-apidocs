module User.Types.User
    ( composeFullName
    , defaultUser
    , defaultUserInfo
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
import Data.Int (Int16)
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import DataRetentionPolicy
import DB
import Folder.Types
import Log.Identifier
import MinutesTime
import User.Email
import User.Lang
import User.Password
import User.Types.SignupMethod
import User.UserID
import UserGroup.Types
import Util.HasSomeUserInfo

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , usertotp                      :: Maybe ByteString
  , usertotpactive                :: Bool
  , usertotpismandatory           :: Bool
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe UTCTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , userassociateddomainid        :: BrandedDomainID
  , usergroupid                   :: UserGroupID
  , userhomefolderid              :: Maybe FolderID
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo User where
  getEmail          = T.strip . unEmail . useremail . userinfo
  getFirstName      = userfstname . userinfo
  getLastName       = usersndname . userinfo
  getPersonalNumber = userpersonalnumber . userinfo
  getMobile         = userphone . userinfo

instance Loggable User where
  logValue User {..} = object
    [ identifier userid
    , "email" .= useremail userinfo
    , "name" .= (userfstname userinfo <> " " <> usersndname userinfo)
    ]
  logDefaultLabel _ = "user"

data UserInfo = UserInfo {
    userfstname         :: Text
  , usersndname         :: Text
  , userpersonalnumber  :: Text
  , usercompanyposition :: Text
  , userphone           :: Text
  , useremail           :: Email
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo UserInfo where
  getEmail          = T.strip . unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  getMobile         = userphone

defaultUser :: User
defaultUser = User { userid                 = unsafeUserID 0
                   , userpassword           = Nothing
                   , usertotp               = Nothing
                   , usertotpactive         = False
                   , usertotpismandatory    = False
                   , useriscompanyadmin     = False
                   , useraccountsuspended   = False
                   , userhasacceptedtermsofservice = Nothing
                   , usersignupmethod       = ByAdmin
                   , userinfo               = defaultUserInfo
                   , usersettings           = defaultUserSettings
                   , userassociateddomainid = unsafeBrandedDomainID 0
                   , usergroupid            = emptyUserGroupID
                   , userhomefolderid       = Nothing
                   }

defaultUserInfo :: UserInfo
defaultUserInfo = UserInfo { userfstname         = ""
                           , usersndname         = ""
                           , userpersonalnumber  = ""
                           , usercompanyposition = ""
                           , userphone           = ""
                           , useremail           = Email ""
                           }

data UserSettings  = UserSettings
  { lang                :: Lang
  , dataretentionpolicy :: DataRetentionPolicy
  } deriving (Eq, Ord, Show)

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings LANG_EN defaultDataRetentionPolicy

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
  , "first_name"
  , "last_name"
  , "personal_number"
  , "company_position"
  , "phone"
  , "email"
  , "lang"
  , "idle_doc_timeout_preparation"
  , "idle_doc_timeout_closed"
  , "idle_doc_timeout_canceled"
  , "idle_doc_timeout_timedout"
  , "idle_doc_timeout_rejected"
  , "idle_doc_timeout_error"
  , "immediate_trash"
  , "associated_domain_id"
  , "password_algorithm"
  , "totp_key"
  , "totp_active"
  , "user_group_id"
  , "home_folder_id"
  , "totp_is_mandatory"
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma selectUsersSelectorsList

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

selectUsersWithUserGroupNamesSQL :: SQL
selectUsersWithUserGroupNamesSQL =
  "SELECT"
  -- User:
    <> "  users.id AS user_id"
    <> ", users.password"
    <> ", users.salt"
    <> ", users.is_company_admin"
    <> ", users.account_suspended"
    <> ", users.has_accepted_terms_of_service"
    <> ", users.signup_method"
    <> ", users.first_name"
    <> ", users.last_name"
    <> ", users.personal_number"
    <> ", users.company_position"
    <> ", users.phone"
    <> ", users.email"
    <> ", users.lang"
    <> ", users.idle_doc_timeout_preparation"
    <> ", users.idle_doc_timeout_closed"
    <> ", users.idle_doc_timeout_canceled"
    <> ", users.idle_doc_timeout_timedout"
    <> ", users.idle_doc_timeout_rejected"
    <> ", users.idle_doc_timeout_error"
    <> ", users.immediate_trash"
    <> ", users.associated_domain_id"
    <> ", users.password_algorithm"
    <> ", users.totp_key"
    <> ", users.totp_active"
    <> ", users.user_group_id"
    <> ", users.home_folder_id"
    <> ", users.totp_is_mandatory"
    <> ", ug.name"
    <> "  FROM users"
    <> "  LEFT JOIN user_groups ug ON users.user_group_id = ug.id"
    <> "  WHERE users.deleted IS NULL"

composeFullName :: (Text, Text) -> Text
composeFullName (fstname, sndname) =
  if T.null sndname then fstname else fstname <> " " <> sndname

fetchUser
  :: ( UserID
     , Maybe ByteString
     , Maybe ByteString
     , Bool
     , Bool
     , Maybe UTCTime
     , SignupMethod
     , Text
     , Text
     , Text
     , Text
     , Text
     , Email
     , Lang
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Bool
     , BrandedDomainID
     , Maybe Int16
     , Maybe ByteString
     , Bool
     , UserGroupID
     , Maybe FolderID
     , Bool
     )
  -> User
fetchUser (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, first_name, last_name, personal_number, company_position, phone, email, lang, idle_doc_timeout_preparation, idle_doc_timeout_closed, idle_doc_timeout_canceled, idle_doc_timeout_timedout, idle_doc_timeout_rejected, idle_doc_timeout_error, immediate_trash, associated_domain_id, password_algorithm, totp_key, totp_active, ugid, mfid, totp_is_mandatory)
  = User
    { userid                 = uid
    , userpassword           = maybeMkPassword
                                 (password, salt, int16ToPwdAlgorithm <$> password_algorithm)
    , usertotp               = totp_key
    , usertotpactive         = totp_active
    , usertotpismandatory    = totp_is_mandatory
    , useriscompanyadmin     = is_company_admin
    , useraccountsuspended   = account_suspended
    , userhasacceptedtermsofservice = has_accepted_terms_of_service
    , usersignupmethod       = signup_method
    , userinfo               = UserInfo { userfstname         = first_name
                                        , usersndname         = last_name
                                        , userpersonalnumber  = personal_number
                                        , usercompanyposition = company_position
                                        , userphone           = phone
                                        , useremail           = email
                                        }
    , usersettings           =
      UserSettings
        { lang                = lang
        , dataretentionpolicy = DataRetentionPolicy
                                  { _drpIdleDocTimeoutPreparation =
                                    idle_doc_timeout_preparation
                                  , _drpIdleDocTimeoutClosed = idle_doc_timeout_closed
                                  , _drpIdleDocTimeoutCanceled = idle_doc_timeout_canceled
                                  , _drpIdleDocTimeoutTimedout = idle_doc_timeout_timedout
                                  , _drpIdleDocTimeoutRejected = idle_doc_timeout_rejected
                                  , _drpIdleDocTimeoutError = idle_doc_timeout_error
                                  , _drpImmediateTrash = immediate_trash
                                  }
        }
    , userassociateddomainid = associated_domain_id
    , usergroupid            = ugid
    , userhomefolderid       = mfid
    }

fetchUserWithUserGroupName
  :: ( UserID
     , Maybe ByteString
     , Maybe ByteString
     , Bool
     , Bool
     , Maybe UTCTime
     , SignupMethod
     , Text
     , Text
     , Text
     , Text
     , Text
     , Email
     , Lang
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Maybe Int16
     , Bool
     , BrandedDomainID
     , Maybe Int16
     , Maybe ByteString
     , Bool
     , UserGroupID
     , Maybe FolderID
     , Bool
     , Text
     )
  -> (User, Text)
fetchUserWithUserGroupName (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, first_name, last_name, personal_number, company_position, phone, email, lang, idle_doc_timeout_preparation, idle_doc_timeout_closed, idle_doc_timeout_canceled, idle_doc_timeout_timedout, idle_doc_timeout_rejected, idle_doc_timeout_error, immediate_trash, associated_domain_id, password_algorithm, totp_key, totp_active, ugid, mfid, totp_is_mandatory, name)
  = (user, name)
  where
    user = User
      { userid                 = uid
      , userpassword           = maybeMkPassword
                                   (password, salt, int16ToPwdAlgorithm <$> password_algorithm)
      , usertotp               = totp_key
      , usertotpactive         = totp_active
      , usertotpismandatory    = totp_is_mandatory
      , useriscompanyadmin     = is_company_admin
      , useraccountsuspended   = account_suspended
      , userhasacceptedtermsofservice = has_accepted_terms_of_service
      , usersignupmethod       = signup_method
      , userinfo               = UserInfo { userfstname         = first_name
                                          , usersndname         = last_name
                                          , userpersonalnumber  = personal_number
                                          , usercompanyposition = company_position
                                          , userphone           = phone
                                          , useremail           = email
                                          }
      , usersettings           =
        UserSettings
          { lang                = lang
          , dataretentionpolicy =
            DataRetentionPolicy
              { _drpIdleDocTimeoutPreparation = idle_doc_timeout_preparation
              , _drpIdleDocTimeoutClosed      = idle_doc_timeout_closed
              , _drpIdleDocTimeoutCanceled    = idle_doc_timeout_canceled
              , _drpIdleDocTimeoutTimedout    = idle_doc_timeout_timedout
              , _drpIdleDocTimeoutRejected    = idle_doc_timeout_rejected
              , _drpIdleDocTimeoutError       = idle_doc_timeout_error
              , _drpImmediateTrash            = immediate_trash
              }
          }
      , userassociateddomainid = associated_domain_id
      , usergroupid            = ugid
      , userhomefolderid       = mfid
      }
