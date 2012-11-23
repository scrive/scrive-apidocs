module SMS.Data where

import Data.Int

import DB.Derive
import MagicHash

-- -------

import Data.Char
import Data.Hash.MD5
import qualified Codec.Text.IConv as IConv
import qualified Codec.Binary.Url as URL
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU

import Utils.IO

newtype ShortMessageID = ShortMessageID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''ShortMessageID)
$(newtypeDeriveConvertible ''ShortMessageID)

data ShortMessage = ShortMessage {
    smID         :: ShortMessageID
  , smToken      :: MagicHash
  , smOriginator :: String
  , smMSISDN     :: String
  , smBody       :: String
  } deriving (Eq, Ord, Show)

sendSMS :: (String, String) -> String -> String -> String -> IO ()
sendSMS (user, password) originator msisdn body = do
  (code, stdout, stderr) <- readProcessWithExitCode' "curl" [url] BS.empty
  putStrLn $ show code
  putStr "STDOUT: "
  BSC.putStrLn stdout
  putStr "STDERR: "
  BSC.putStrLn stderr
  where
    latin_user = toLatin user
    latin_password = toLatin password
    latin_originator = toLatin originator
    latin_msisdn = toLatin msisdn
    latin_body = toLatin body
    hash = md5s . Str $ concat [
        latin_user
      , latin_body
      , latin_originator
      , latin_msisdn
      , md5s . Str $ latin_user ++ ":" ++ latin_password
      ]
    url = concat [
        "http://mcm.globalmouth.com:8080/api/mcm?"
      , "username=", urlEncode latin_user, "&"
      , "body=", urlEncode latin_body, "&"
      , "msisdn=", urlEncode latin_msisdn, "&"
      , "originator=", urlEncode latin_originator, "&"
      , "hash=", hash
      ]

    toLatin = BSC.unpack . IConv.convert "utf8" "latin1" . BSU.fromString
    urlEncode = URL.encode . map (fromIntegral . ord)
