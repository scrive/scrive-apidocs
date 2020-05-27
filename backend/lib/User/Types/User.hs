module User.Types.User
  ( User(User)
  , UserInfo(UserInfo)
  , UserSettings(UserSettings)
  , composeFullName
  , defaultUser
  , defaultUserInfo
  , fetchUser
  , fetchUserWithUserGroupName
  , selectUsersSQL
  , selectUsersSelectors
  , selectUsersSelectorsList
  , selectUsersWithUserGroupNamesSQL
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16)
import qualified Data.Set as S
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import DataRetentionPolicy
import DB
import Folder.Types
import LoginAuth.LoginAuthMethod
import MinutesTime
import Tag.Tables
import User.Email
import User.Lang
import User.Password
import User.Types.SignupMethod
import User.Types.User.Internal
import User.UserID
import UserGroup.Types
import qualified DataRetentionPolicy.Internal

defaultUser :: User
defaultUser = User { id                 = unsafeUserID 0
                   , password           = Nothing
                   , totpKey            = Nothing
                   , totpActive         = False
                   , totpIsMandatory    = False
                   , isCompanyAdmin     = False
                   , accountSuspended   = False
                   , hasAcceptedTOS     = Nothing
                   , signupMethod       = ByAdmin
                   , info               = defaultUserInfo
                   , settings           = defaultUserSettings
                   , associatedDomainID = unsafeBrandedDomainID 0
                   , groupID            = emptyUserGroupID
                   , homeFolderID       = Nothing
                   , sysAuth            = LoginAuthNative
                   , internalTags       = S.empty
                   , externalTags       = S.empty
                   }

defaultUserInfo :: UserInfo
defaultUserInfo = UserInfo { firstName       = ""
                           , lastName        = ""
                           , personalNumber  = ""
                           , companyPosition = ""
                           , phone           = ""
                           , email           = Email ""
                           }

defaultUserSettings :: UserSettings
defaultUserSettings = UserSettings LANG_EN defaultDataRetentionPolicy

internalUserTagsSelector :: SQL
internalUserTagsSelector =
  "ARRAY(SELECT ("
    <>  mintercalate ", " tagSelectors
    <>  ")::"
    <>  raw (ctName ctTag)
    <+> "FROM user_tags ut WHERE users.id = ut.user_id AND ut.internal ORDER BY ut.name)"

externalUserTagsSelector :: SQL
externalUserTagsSelector =
  "ARRAY(SELECT ("
    <>  mintercalate ", " tagSelectors
    <>  ")::"
    <>  raw (ctName ctTag)
    <+> "FROM user_tags ut WHERE users.id = ut.user_id AND NOT ut.internal ORDER BY ut.name)"

tagSelectors :: [SQL]
tagSelectors = ["name", "value"]

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
  , "sysauth"
  , internalUserTagsSelector
  , externalUserTagsSelector
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma selectUsersSelectorsList

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

selectUsersWithUserGroupNamesSQL :: SQL
selectUsersWithUserGroupNamesSQL =
  "  users.id AS user_id"
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
    <> ", users.sysauth"
    <> ", "
    <> internalUserTagsSelector
    <> ", "
    <> externalUserTagsSelector
    <> ", ug.name, uga.entity_name"
    <> "  FROM users"
    <> "  LEFT JOIN user_groups ug ON users.user_group_id = ug.id"
    <> "  LEFT JOIN user_group_addresses uga ON ug.id = uga.user_group_id"

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
     , LoginAuthMethod
     , CompositeArray1 Tag
     , CompositeArray1 Tag
     )
  -> User
fetchUser (id, password, salt, isCompanyAdmin, accountSuspended, hasAcceptedTOS, signupMethod, firstName, lastName, personalNumber, companyPosition, phone, email, lang, idleDocTimeoutPreparation, idleDocTimeoutClosed, idleDocTimeoutCanceled, idleDocTimeoutTimedout, idleDocTimeoutRejected, idleDocTimeoutError, immediateTrash, associatedDomainID, passwordAlgorithm, totpKey, totpActive, groupID, homeFolderID, totpIsMandatory, sysAuth, CompositeArray1 iTags, CompositeArray1 eTags)
  = User
    { password = maybeMkPassword password salt (int16ToPwdAlgorithm <$> passwordAlgorithm)
    , info         = UserInfo { .. }
    , settings     = UserSettings { lang                = lang
                                  , dataRetentionPolicy = DataRetentionPolicy { .. }
                                  }
    , internalTags = S.fromList iTags
    , externalTags = S.fromList eTags
    , ..
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
     , LoginAuthMethod
     , CompositeArray1 Tag
     , CompositeArray1 Tag
     , Text
     , Text
     )
  -> (User, Text, Text)
fetchUserWithUserGroupName (id, password, salt, isCompanyAdmin, accountSuspended, hasAcceptedTOS, signupMethod, firstName, lastName, personalNumber, companyPosition, phone, email, lang, idleDocTimeoutPreparation, idleDocTimeoutClosed, idleDocTimeoutCanceled, idleDocTimeoutTimedout, idleDocTimeoutRejected, idleDocTimeoutError, immediateTrash, associatedDomainID, passwordAlgorithm, totpKey, totpActive, groupID, homeFolderID, totpIsMandatory, sysAuth, CompositeArray1 iTags, CompositeArray1 eTags, ugname, entityName)
  = (user, ugname, entityName)
  where
    user = User
      { password     = maybeMkPassword password
                                       salt
                                       (int16ToPwdAlgorithm <$> passwordAlgorithm)
      , info         = UserInfo { .. }
      , settings     = UserSettings { lang                = lang
                                    , dataRetentionPolicy = DataRetentionPolicy { .. }
                                    }
      , internalTags = S.fromList iTags
      , externalTags = S.fromList eTags
      , ..
      }
