module File.File 
    ( File(..)
    , FileStorage(..)
    ) where

import Data.Typeable
import qualified Data.ByteString.UTF8 as BS

import Crypto
import File.FileID

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS String String AESConf -- ^ bucket, url inside bucket, aes key/iv
  | FileStorageDisk FilePath -- ^ filepath
    deriving (Eq, Ord, Show, Typeable)

data File = File {
    fileid       :: FileID
  , filename     :: String
  , filestorage  :: FileStorage
  , filechecksum :: BS.ByteString
  } deriving (Typeable)

instance Eq File where
    a == b = fileid a == fileid b

instance Ord File where
    compare a b | fileid a == fileid b = EQ
                | otherwise = compare (fileid a,filename a)
                                      (fileid b,filename b)
instance Show File where
    showsPrec _prec file = (++) (filename file)
