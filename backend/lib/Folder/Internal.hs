{-# LANGUAGE TemplateHaskell #-}
module Folder.Internal (
    Folder(..)
  , FolderWithChildren(..)
  , FolderID
  , fromFolderID
  , emptyFolderID
  , unsafeFolderID
  ) where

import Data.Aeson
import Data.Int
import Data.Text (Text, pack)
import Data.Unjson
import Happstack.Server
import Optics.TH
import Text.JSON.ToJSValue
import qualified Data.Text as T

import DB
import Log.Identifier

data Folder = Folder
  { id       :: !FolderID
  , parentID :: !(Maybe FolderID)
  , name     :: !Text
  } deriving (Show, Eq)

-- Folder and all its children down to the bottom
data FolderWithChildren = FolderWithChildren
  { folder   :: !Folder
  , children :: ![FolderWithChildren]
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
  fromReqURI = maybeRead . T.pack

unsafeFolderID :: Int64 -> FolderID
unsafeFolderID = FolderID

emptyFolderID :: FolderID
emptyFolderID = FolderID 0

fromFolderID :: FolderID -> Int64
fromFolderID (FolderID k) = k

instance Identifier FolderID where
  idDefaultLabel = "folder_id"
  idValue (FolderID k) = int64AsStringIdentifier k

instance Unjson FolderID where
  unjsonDef = unjsonInvmapR
    (maybe (fail "Can't parse FolderID") return . maybeRead . T.pack)
    show
    unjsonDef

instance ToJSValue FolderID where
  toJSValue (FolderID k) = toJSValue $ show k

instance ToJSON FolderID where
  toJSON (FolderID k) = toJSON . pack $ show k

instance FromJSON FolderID where
  parseJSON v = do
    fidStr <- parseJSON v
    case maybeRead fidStr of
      Nothing  -> fail "Could not parse Folder ID"
      Just fid -> return fid

makeFieldLabelsWith noPrefixFieldLabels ''Folder
makeFieldLabelsWith noPrefixFieldLabels ''FolderWithChildren
