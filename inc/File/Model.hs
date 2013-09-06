module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    ) where

import Control.Applicative
import Data.Monoid
import Database.HDBC

import Crypto
import Crypto.RNG
import DB
import DB.SQL2
import File.File
import File.FileID
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1

data GetFileByFileID = GetFileByFileID FileID
instance MonadDB m => DBQuery m GetFileByFileID (Maybe File) where
  query (GetFileByFileID fid) = do
    kRun_ $ selectFilesSQL <> SQL "WHERE id = ?" [toSql fid]
    fetchFiles >>= oneObjectReturnedGuard

data NewFile = NewFile String Binary
instance (Applicative m, CryptoRNG m, MonadDB m) => DBUpdate m NewFile FileID where
  update (NewFile filename content) = do
    kRun_ $ sqlInsert "files" $ do
        sqlSet "name" filename
        sqlSet "content" $ content
        sqlSet "checksum" $ SHA1.hash `binApp` content
        sqlSet "size" $ BS.length $ unBinary content
        sqlResult "id"
    let fetchIDs = kFold decoder []
        decoder acc fid = fid : acc
    fetchIDs >>= exactlyOneObjectReturnedGuard

data FileMovedToAWS = FileMovedToAWS FileID String String AESConf
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url aes) =
    kRun_ $ sqlUpdate "files" $ do
        sqlSet "content" SqlNull
        sqlSet "amazon_bucket" bucket
        sqlSet "amazon_url" url
        sqlSet "aes_key" $ aesKey aes
        sqlSet "aes_iv" $ aesIV aes
        sqlWhereEq "id" fid

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance MonadDB m => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    kRun_ $ selectFilesSQL <> SQL "WHERE content IS NOT NULL LIMIT 1" []
    fetchFiles >>= oneObjectReturnedGuard

selectFilesSQL :: SQL
selectFilesSQL = "SELECT" <+> sqlConcatComma (map raw filesSelectors) <+> "FROM files "

filesSelectors :: [RawSQL]
filesSelectors = [
    "id"
  , "name"
  , "content"
  , "amazon_bucket"
  , "amazon_url"
  , "checksum"
  , "aes_key"
  , "aes_iv"
  ]

fetchFiles :: MonadDB m => m [File]
fetchFiles = kFold decoder []
  where
    decoder acc fid fname content amazon_bucket
      amazon_url checksum maes_key maes_iv = File {
        fileid = fid
      , filename = fname
      , filestorage =
        -- Here we need to support the following cases:
        --
        -- * plain data in the database: just return content
        -- * encrypted data in the database (backward compatibility only):
        --      decrypt and return content
        -- * encrypted data in Amazon S3: return (bucket, url, aes)
        -- * invalid AES key: error out at this place
        --
        -- Binary data in database is temporary: next cron run should
        -- move it to Amazon.  Encrypted data in database is backward
        -- compatibility only: we are not going to put entrypted data
        -- anymore, but we need to handle some leftovers that may be
        -- lingering there.
        case content of
          Just (Binary mem) -> case eaes of
            Nothing          -> FileStorageMemory mem
            Just (Right aes) -> FileStorageMemory (aesDecrypt aes mem)
            Just (Left msg)  -> err msg
          Nothing -> case (amazon_bucket, amazon_url, eaes) of
            (Just bucket, Just url, Just (Right aes)) -> FileStorageAWS bucket url aes
            (Just _,      Just _,   Just (Left msg))  -> err msg
            d                                  -> error $ "Invalid AWS data for file with id = " ++ show fid ++ ": " ++ show d
      , filechecksum = unBinary `fmap` checksum
    } : acc
      where
        err :: String -> FileStorage
        err msg = error $ "File with id = " ++ show fid ++ " has invalid aes/iv pair: " ++ msg
        eaes = case (maes_key, maes_iv) of
                 (Just aes_key, Just aes_iv) -> Just $ mkAESConf (unBinary aes_key) (unBinary aes_iv)
                 _ -> Nothing
