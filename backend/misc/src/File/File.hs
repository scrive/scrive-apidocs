module File.File
    ( File(..)
    , FileStorage(..)
    ) where

import Data.Aeson
import Data.Int (Int32)
import Data.Typeable
import qualified Data.ByteString as BS

import Crypto
import File.FileID
import KontraPrelude
import Log.Identifier

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS String AESConf -- ^ url inside bucket, aes key/iv
    deriving (Eq, Ord, Show, Typeable)

instance LogObject FileStorage where
  logObject (FileStorageMemory _)  = object ["type" .= ("in_memory" :: String)]
  logObject (FileStorageAWS url _) = object [
      "type" .= ("aws_bucket" :: String)
    , "url"  .= url
    ]
  logDefaultLabel _ = "file_storage"

data File = File {
    fileid       :: FileID
  , filename     :: String
  -- if there is conversion error from sql, detect it immediately
  , filestorage  :: !FileStorage
  , filechecksum :: Maybe BS.ByteString
  , filesize     :: Int32
  } deriving (Typeable)

instance Eq File where
  a == b = fileid a == fileid b

instance Ord File where
  compare a b | fileid a == fileid b = EQ
              | otherwise = compare (fileid a, filename a)
                                    (fileid b, filename b)
instance Show File where
  show = filename

instance LogObject File where
  logObject File{..} = object [
      identifier_ fileid
    , "name" .= filename
    , "size" .= filesize
    , logPair_ filestorage
    ]
  logDefaultLabel _ = "file"
