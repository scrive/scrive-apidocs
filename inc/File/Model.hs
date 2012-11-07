{-# LANGUAGE OverloadedStrings #-}
module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , GetFileWithNoChecksum(..)
    , SetChecksum(..)
    , SetContentToMemoryAndEncryptIt(..)
    ) where

import Control.Applicative
import Control.Monad.Trans
import Data.Monoid
import Database.HDBC

import Crypto
import Crypto.RNG
import DB
import File.File
import File.FileID
import File.Tables
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1

data GetFileByFileID = GetFileByFileID FileID
instance MonadDB m => DBQuery m GetFileByFileID (Maybe File) where
  query (GetFileByFileID fid) = do
    kRun_ $ selectFilesSQL <> SQL "WHERE id = ?" [toSql fid]
    fetchFiles >>= oneObjectReturnedGuard

data NewFile = NewFile String Binary
instance (Applicative m, CryptoRNG m, MonadDB m) => DBUpdate m NewFile File where
  update (NewFile filename content) = do
    Right aes <- lift $ mkAESConf <$> randomBytes 32 <*> randomBytes 16
    kRun_ $ mkSQL INSERT tableFiles [
        sql "name" filename
      , sql "content" $ aesEncrypt aes `binApp` content
      , sql "checksum" $ SHA1.hash `binApp` content
      , sql "size" $ BS.length $ unBinary content
      , sql "aes_key" $ aesKey aes
      , sql "aes_iv" $ aesIV aes
      ] <+> "RETURNING" <+> filesSelectors
    fetchFiles >>= exactlyOneObjectReturnedGuard

data FileMovedToAWS = FileMovedToAWS FileID String String
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url) =
    kRun_ $ mkSQL UPDATE tableFiles [
        sql "content" SqlNull
      , sql "amazon_bucket" bucket
      , sql "amazon_url" url
      ] <> SQL "WHERE id = ?" [toSql fid]

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance MonadDB m => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    kRun_ $ selectFilesSQL <> SQL "WHERE content IS NOT NULL LIMIT 1" []
    fetchFiles >>= oneObjectReturnedGuard

-- | Needed for encrypting/calculating checksum for old files. To be removed after 15.08.2012.
data GetFileWithNoChecksum = GetFileWithNoChecksum
instance MonadDB m => DBQuery m GetFileWithNoChecksum (Maybe File) where
  query GetFileWithNoChecksum = do
    kRun_ $ selectFilesSQL <> SQL "WHERE checksum IS NULL LIMIT 1" []
    fetchFiles >>= oneObjectReturnedGuard

-- | Needed for encrypting/calculating checksum for old files. To be removed after 15.08.2012.
data SetChecksum = SetChecksum FileID Binary
instance MonadDB m => DBUpdate m SetChecksum Bool where
  update (SetChecksum fid checksum) = do
    kRun01 $ mkSQL UPDATE tableFiles [sql "checksum" checksum]
      <> SQL "WHERE id = ?" [toSql fid]

-- | Needed for encrypting/calculating checksum for old files. To be removed after 15.08.2012.
data SetContentToMemoryAndEncryptIt = SetContentToMemoryAndEncryptIt FileID Binary
instance (Applicative m, CryptoRNG m, MonadDB m) => DBUpdate m SetContentToMemoryAndEncryptIt Bool where
  update (SetContentToMemoryAndEncryptIt fid content) = do
    Right aes <- lift $ mkAESConf <$> randomBytes 32 <*> randomBytes 16
    kRun01 $ mkSQL UPDATE tableFiles [
        sql "content" $ aesEncrypt aes `binApp` content
      , sql "size" $ BS.length $ unBinary content
      , sql "aes_key" $ aesKey aes
      , sql "aes_iv" $ aesIV aes
      ] <> SQL "WHERE id = ?" [toSql fid]

selectFilesSQL :: SQL
selectFilesSQL = "SELECT" <+> filesSelectors <+> "FROM files "

filesSelectors :: SQL
filesSelectors = sqlConcatComma [
    "id"
  , "name"
  , "content"
  , "amazon_bucket"
  , "amazon_url"
  , "checksum"
  , "aes_key"
  , "aes_iv"
  ]

fetchFiles :: MonadDB m => DBEnv m [File]
fetchFiles = foldDB decoder []
  where
    decoder acc fid fname content amazon_bucket
      amazon_url checksum aes_key aes_iv = File {
        fileid = fid
      , filename = fname
      , filestorage =
        case content of
          Just (Binary mem) -> case eaes of
            Right aes -> FileStorageMemory mem aes
            Left msg  -> err msg
          Nothing -> case (amazon_bucket, amazon_url, eaes) of
            (Just bucket, Just url, Right aes) -> FileStorageAWS bucket url aes
            (Just _,      Just _,   Left msg)  -> err msg
            d                                  -> error $ "Invalid AWS data for file with id = " ++ show fid ++ ": " ++ show d
      , filechecksum = unBinary `fmap` checksum
    } : acc
      where
        err :: String -> FileStorage
        err msg = error $ "File with id = " ++ show fid ++ " has invalid aes/iv pair: " ++ msg
        eaes = mkAESConf (unBinary aes_key) (unBinary aes_iv)
