module Folder.Internal (
    Folder(..)
  , FolderID
  , FolderWithChildren(..)
  , defaultFolder
  , fromFolderID
  , emptyFolderID
  , unsafeFolderID
  , fwcToList
  ) where

import Data.Int
import Data.Text (Text)
import Happstack.Server

import DB
import Log.Identifier

data Folder = Folder {
    _folderID       :: !FolderID
  , _folderParentID :: !(Maybe FolderID)
  , _folderName     :: !Text
  } deriving (Show, Eq)

-- Folder and all its children down to the bottom
data FolderWithChildren = FolderWithChildren
  { _fwcFolder :: Folder
  , _fwcChildren :: [FolderWithChildren]
  }

fwcToList :: FolderWithChildren -> [Folder]
fwcToList fwc = _fwcFolder fwc : concatMap fwcToList (_fwcChildren fwc)

type instance CompositeRow Folder = (
    FolderID
  , Maybe FolderID
  , Text
  )

instance PQFormat Folder where
  pqFormat = "%folder"

instance CompositeFromSQL Folder where
  toComposite (dgid, mparentgroupid, name) =
    Folder
    {
      _folderID = dgid
    , _folderParentID = mparentgroupid
    , _folderName = name
    }

defaultFolder :: Folder
defaultFolder = 
    Folder
    {
      _folderID       = emptyFolderID
    , _folderParentID = Nothing
    , _folderName     = ""
    }

newtype FolderID = FolderID Int64
  deriving (Eq, Ord)
deriving newtype instance Read FolderID
deriving newtype instance Show FolderID

instance PQFormat FolderID where
  pqFormat = pqFormat @Int64

instance FromSQL FolderID where
  type PQBase FolderID = PQBase Int64
  fromSQL mbase = FolderID <$> fromSQL mbase

instance ToSQL FolderID where
  type PQDest FolderID = PQDest Int64
  toSQL (FolderID n) = toSQL n

instance FromReqURI FolderID where
  fromReqURI = maybeRead

unsafeFolderID :: Int64 -> FolderID
unsafeFolderID = FolderID

emptyFolderID :: FolderID
emptyFolderID = FolderID 0

fromFolderID :: FolderID -> Int64
fromFolderID (FolderID k) = k

instance Identifier FolderID where
  idDefaultLabel          = "folder_id"
  idValue (FolderID k) = int64AsStringIdentifier k
