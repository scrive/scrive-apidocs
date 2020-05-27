module Folder.Types
  ( Folder(Folder)
  , FolderID
  , FolderWithChildren(FolderWithChildren)
  , fetchFolder
  , defaultFolder
  , fromFolderID
  , emptyFolderID
  , unsafeFolderID
  , fwcToList
  ) where

import Folder.FolderID
import Folder.Internal
import User.UserID
import UserGroup.Types (UserGroupID)

fwcToList :: FolderWithChildren -> [Folder]
fwcToList fwc = folder fwc : concatMap fwcToList (children fwc)

fetchFolder :: (FolderID, Maybe FolderID, Text, Maybe UserID, Maybe UserGroupID) -> Folder
fetchFolder (id, parentID, name, homeForUser, homeForUserGroup) = Folder { .. }

defaultFolder :: Folder
defaultFolder = Folder { id               = emptyFolderID
                       , parentID         = Nothing
                       , name             = ""
                       , homeForUser      = Nothing
                       , homeForUserGroup = Nothing
                       }
