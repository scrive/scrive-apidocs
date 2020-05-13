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

import Folder.Internal

fwcToList :: FolderWithChildren -> [Folder]
fwcToList fwc = folder fwc : concatMap fwcToList (children fwc)

fetchFolder :: (FolderID, Maybe FolderID, Text) -> Folder
fetchFolder (id, parentID, name) = Folder { .. }

defaultFolder :: Folder
defaultFolder = Folder { id = emptyFolderID, parentID = Nothing, name = "" }
