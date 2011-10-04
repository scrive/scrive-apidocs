module File.File 
    ( File(..)
    , module File.FileID
    , FileStorage(..)
    ) where

import DB.Derive
import File.FileID
import Data.Data
import qualified Data.ByteString.Char8 as BS

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
