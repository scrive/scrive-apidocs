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
import Log.Identifier

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS String AESConf -- ^ url inside bucket, aes key/iv
    deriving (Eq, Ord, Show, Typeable)

instance Loggable FileStorage where
  logValue (FileStorageMemory _)  = object ["type" .= ("in_memory" :: String)]
  logValue (FileStorageAWS url _) = object [
      "type" .= ("aws_bucket" :: String)
    , "url"  .= url
    ]
  logDefaultLabel _ = "file_storage"

data File = File {
    fileid       :: FileID
  , filename     :: String
  -- if there is conversion error from sql, detect it immediately
  , filestorage  :: !FileStorage
  , filechecksum :: BS.ByteString
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

instance Loggable File where
  logValue File{..} = object [
      identifier fileid
    , "name" .= filename
    , "size" .= filesize
    , logPair_ filestorage
    ]
  logDefaultLabel _ = "file"
