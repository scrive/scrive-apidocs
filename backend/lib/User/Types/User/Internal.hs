{-# LANGUAGE TemplateHaskell #-}
module User.Types.User.Internal
  ( User(..)
  , UserInfo(..)
  , UserSettings(..)
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Optics.TH
import qualified Data.Set as S
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import DataRetentionPolicy
import Folder.Types
import Log.Identifier
import LoginAuth.LoginAuthMethod
import MinutesTime
import Tag
import User.Email
import User.Lang
import User.Password
import User.Types.SignupMethod
import User.UserID
import UserGroup.Types
import Util.HasSomeUserInfo

data User = User
  { id                 :: UserID
  , password           :: Maybe Password
  , totpKey            :: Maybe ByteString
  , totpActive         :: Bool
  , totpIsMandatory    :: Bool
  , isCompanyAdmin     :: Bool
  , accountSuspended   :: Bool
  , hasAcceptedTOS     :: Maybe UTCTime
  , signupMethod       :: SignupMethod
  , info               :: UserInfo
  , settings           :: UserSettings
  , associatedDomainID :: BrandedDomainID
  , groupID            :: UserGroupID
  , homeFolderID       :: Maybe FolderID
  , sysAuth            :: LoginAuthMethod
  , internalTags       :: !(S.Set Tag)
  , externalTags       :: !(S.Set Tag)
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo User where
  getEmail          = T.strip . unEmail . email . info
  getFirstName      = firstName . info
  getLastName       = lastName . info
  getPersonalNumber = personalNumber . info
  getMobile         = phone . info

instance Loggable User where
  logValue User {..} = object
    [ identifier id
    , "email" .= email info
    , "name" .= (firstName info <> " " <> lastName info)
    ]
  logDefaultLabel _ = "user"

data UserInfo = UserInfo
  { firstName       :: Text
  , lastName        :: Text
  , personalNumber  :: Text
  , companyPosition :: Text
  , phone           :: Text
  , email           :: Email
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo UserInfo where
  getEmail          = T.strip . unEmail . email
  getFirstName      = firstName
  getLastName       = lastName
  getPersonalNumber = personalNumber
  getMobile         = phone

data UserSettings = UserSettings
  { lang                :: Lang
  , dataRetentionPolicy :: DataRetentionPolicy
  } deriving (Eq, Ord, Show)

instance HasLang User where
  getLang = getLang . settings

instance HasLang UserSettings where
  getLang = lang

makeFieldLabelsWith noPrefixFieldLabels ''User
makeFieldLabelsWith noPrefixFieldLabels ''UserInfo
makeFieldLabelsWith noPrefixFieldLabels ''UserSettings
