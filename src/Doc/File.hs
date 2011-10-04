module Doc.File 
    ( File(..)
    , FileID(..)
    , FileStorage(..)
    ) where

import DB.Derive

import Data.Int
import Data.Data
import Happstack.Server
import Happstack.Util.Common
import qualified Data.ByteString.Char8 as BS

newtype FileID = FileID { unFileID :: Int64 }
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveConvertible ''FileID)
$(newtypeDeriveUnderlyingReadShow ''FileID)

instance FromReqURI FileID where
  fromReqURI = readM

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
  | FileStorageDisk FilePath -- ^ filepath
    deriving (Eq, Ord, Show, Data, Typeable)

$(jsonableDeriveConvertible [t| FileStorage |])

data File = File {
    fileid      :: FileID
  , filename    :: BS.ByteString
  , filestorage :: FileStorage
  } deriving (Eq, Ord, Show)
