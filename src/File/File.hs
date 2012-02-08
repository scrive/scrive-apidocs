module File.File 
    ( File(..)
    , FileStorage(..)
    ) where

import File.FileID
import qualified Data.ByteString.UTF8 as BS

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
  | FileStorageDisk FilePath -- ^ filepath
    deriving (Eq, Ord, Show)

data File = File {
    fileid      :: FileID
  , filename    :: BS.ByteString
  , filestorage :: FileStorage
  }

instance Eq File where
    a == b = fileid a == fileid b

instance Ord File where
    compare a b | fileid a == fileid b = EQ
                | otherwise = compare (fileid a,filename a)
                                      (fileid b,filename b)
instance Show File where
    showsPrec _prec file = (++) (BS.toString (filename file))
