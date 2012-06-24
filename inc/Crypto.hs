module Crypto (
    AESConf
  , mkAESConf
  , aesEncrypt
  , aesDecrypt
  , aesKey
  , aesIV
  ) where

import Codec.Crypto.AES
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data AESConf = AESConf ByteString ByteString -- key, iv
  deriving (Eq, Ord, Show)

mkAESConf :: ByteString -> ByteString -> Either String AESConf
mkAESConf key iv =
  if key_len == 32
    then if BS.length iv == 16
           then Right $ AESConf key iv
           else Left $ "Invalid AES IV length: " ++ show iv_len ++ ", should be 16"
    else Left $ "Invalid AES key length: " ++ show key_len ++ ", should be 32"
  where
    key_len = BS.length key
    iv_len  = BS.length iv

aesEncrypt :: AESConf -> ByteString -> ByteString
aesEncrypt (AESConf key iv) = crypt' CFB key iv Encrypt

aesDecrypt :: AESConf -> ByteString -> ByteString
aesDecrypt (AESConf key iv) = crypt' CFB key iv Decrypt

aesKey :: AESConf -> ByteString
aesKey (AESConf key _) = key

aesIV :: AESConf -> ByteString
aesIV (AESConf _ iv) = iv
