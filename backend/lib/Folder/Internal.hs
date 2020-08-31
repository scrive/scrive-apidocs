{-# LANGUAGE TemplateHaskell #-}
module Folder.Internal (
    Folder(..)
  , FolderWithChildren(..)
  ) where

import Optics.TH

import Folder.FolderID
import User.UserID
import UserGroup.Types (UserGroupID)

data Folder = Folder
  { id               :: FolderID
  , parentID         :: Maybe FolderID
  , name             :: Text
  , homeForUser      :: Maybe UserID
  , homeForUserGroup :: Maybe UserGroupID
  } deriving (Show, Eq)

data FolderWithChildren = FolderWithChildren
  { folder   :: Folder
  , children :: [FolderWithChildren]
  }

makeFieldLabelsWith noPrefixFieldLabels ''Folder
makeFieldLabelsWith noPrefixFieldLabels ''FolderWithChildren
