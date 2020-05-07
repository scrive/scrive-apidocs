{-# LANGUAGE DeriveGeneric #-}

module Flow.Model.Types where

import GHC.Generics (Generic)

import User.UserID (UserID)
import UserGroup.Internal (UserGroupID)

data InsertTemplate = InsertTemplate
    { name :: Text
    , process :: Text
    , userId :: UserID
    , userGroupId :: UserGroupID
    }
  deriving (Show, Eq, Generic)
