module File.File 
    ( File(..)
    , FileStorage(..)
    ) where

import DB.Derive
import File.FileID
import Data.Data
import qualified Data.ByteString.UTF8 as BS
import Happstack.State
import Misc

data FileStorage =
    FileStorageMemory BS.ByteString
  | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
  | FileStorageDisk FilePath -- ^ filepath
    deriving (Eq, Ord, Show, Data, Typeable)

data File = File {
    fileid      :: FileID
  , filename    :: BS.ByteString
  , filestorage :: FileStorage
  } 

{- | Watch out. This instance is a bit special. It has to be
   "File" - as this is what database uses as table name.  Simple
   deriving clause will create a "MyApp.MyModule.File"!  -}

instance Typeable File where typeOf _ = mkTypeOf "File"

instance Eq File where
    a == b = fileid a == fileid b

instance Ord File where
    compare a b | fileid a == fileid b = EQ
                | otherwise = compare (fileid a,filename a)
                                      (fileid b,filename b)
instance Show File where
    showsPrec _prec file = (++) (BS.toString (filename file))


$(jsonableDeriveConvertible [t| FileStorage |])

instance Migrate () File where
    migrate () = error "Cannot migrate old files, code removed"

$(deriveSerialize ''File)
instance Version File where
    mode = extension 3 (Proxy :: Proxy ())

$(deriveSerialize ''FileStorage)
instance Version FileStorage where
