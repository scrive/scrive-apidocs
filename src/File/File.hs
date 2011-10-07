module File.File 
    ( File(..)
    , Files
    , FileStorage(..)

    , FileMovedToAWS(..)
    , FileMovedToDisk(..)
    , GetFileByFileID(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PutFileUnchecked(..)
    ) where

import DB.Derive
import File.FileID
import Data.Data
import qualified Data.ByteString.UTF8 as BS
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
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


type Files = IxSet File

data FileMovePending = FileMovePending
                     deriving (Eq, Ord, Typeable)

instance Indexable File where
  empty = 
    ixSet [ ixFun (\x -> [fileid x] :: [FileID])
          , ixFun (\x -> case x of
                             File{ filestorage = FileStorageMemory{} } -> [FileMovePending]
                             _ -> [])
          ]

instance Component Files where
  type Dependencies Files = End
  initialValue = empty

getFileByFileID :: FileID -> Query Files (Maybe File)
getFileByFileID fid = ask >>= \files ->
    return $ getOne $ files @= fid

newFile :: BS.ByteString -> BS.ByteString -> Update Files File
newFile name content = do
  files <- ask
  fid <- getUnique64 files FileID
  let file = File { fileid = fid
                  , filename = name
                  , filestorage = FileStorageMemory content
                  }
  modify $ insert file
  return file

putFileUnchecked :: File -> Update Files FileID
putFileUnchecked file = do
  modify $ insert file
  return (fileid file)

fileMovedToAWS :: FileID
               -> BS.ByteString
               -> BS.ByteString
               -> Update Files ()
fileMovedToAWS fileid bucket url = fileMovedTo fileid $ FileStorageAWS bucket url

fileMovedToDisk :: FileID -> FilePath -> Update Files ()
fileMovedToDisk fileid filepath = fileMovedTo fileid $ FileStorageDisk filepath

fileMovedTo :: FileID -> FileStorage -> Update Files ()
fileMovedTo fid fstorage = do
    files <- ask
    case getOne (files @= fid) of
        Nothing -> return ()
        Just file -> do
            modify (updateIx fid $ file { filestorage = fstorage } )
            return ()

getFilesThatShouldBeMovedToAmazon :: Query Files [File]
getFilesThatShouldBeMovedToAmazon = do
    files <- ask
    return (IxSet.toList (files @= FileMovePending))

$(mkMethods ''Files [ 'getFileByFileID
                    , 'newFile
                    , 'fileMovedToAWS
                    , 'fileMovedToDisk
                    , 'getFilesThatShouldBeMovedToAmazon
                    , 'putFileUnchecked
                    ])
