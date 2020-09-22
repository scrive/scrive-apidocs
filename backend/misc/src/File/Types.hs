module File.Types
    ( File(..)
    , FileStorage(..)
    , filesSelectors
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Typeable
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Model.CompositeType
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Crypto
import File.FileID
import File.Tables
import Log.Identifier

data FileStorage
  = FileStorageAWS Text AESConf -- ^ url inside bucket, aes key/iv
  deriving (Eq, Ord, Show, Typeable)

instance Loggable FileStorage where
  logValue (FileStorageAWS url _) =
    object ["type" .= ("aws_bucket" :: String), "url" .= url]
  logDefaultLabel _ = "file_storage"

data File = File
  { fileid       :: FileID
  , filename     :: Text
  -- if there is conversion error from sql, detect it immediately
  , filestorage  :: !FileStorage
  , filechecksum :: BS.ByteString
  , filesize     :: Int32
  } deriving (Typeable)

instance Eq File where
  a == b = fileid a == fileid b

instance Ord File where
  compare a b | fileid a == fileid b = EQ
              | otherwise = compare (fileid a, filename a) (fileid b, filename b)

instance Show File where
  show = T.unpack . filename

instance Loggable File where
  logValue File {..} = object
    [identifier fileid, "name" .= filename, "size" .= filesize, logPair_ filestorage]
  logDefaultLabel _ = "file"


{- SQL Deserialisation -}

filesSelectors :: [SQL]
filesSelectors = ["id", "name", "amazon_url", "size", "checksum", "aes_key", "aes_iv"]

type instance CompositeRow File
  = (FileID, Text, Maybe Text, Int32, ByteString, Maybe ByteString, Maybe ByteString)

instance PQFormat File where
  pqFormat = compositeTypePqFormat ctFile

instance CompositeFromSQL File where
  toComposite (fid, fname, mamazon_url, size, checksum, maes_key, maes_iv) = File
    { fileid       = fid
    , filename     = fname
    , filestorage  =
          -- Here we need to support the following cases:
          --
          --  * encrypted data in Amazon S3: return (url, aes)
          --  * missing URL: error (see NewEmptyFileForAWS)
          --  * invalid AES key: error out at this place
      case (mamazon_url, maes) of
        (Just url, Just (Right aes)) -> FileStorageAWS url aes
        (Just _  , Just (Left msg) ) -> err $ T.pack msg
        d ->
          unexpectedError
            $   "invalid AWS data for file with id ="
            <+> showt fid
            <>  ":"
            <+> showt d
    , filechecksum = checksum
    , filesize     = size
    }
    where
      err :: Text -> FileStorage
      err msg =
        unexpectedError
          $   "file with id ="
          <+> showt fid
          <+> "has invalid aes/iv pair:"
          <+> msg

      maes = case (maes_key, maes_iv) of
        (Just aes_key, Just aes_iv) -> Just $ mkAESConf aes_key aes_iv
        _ -> Nothing
