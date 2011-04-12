{-# OPTIONS_GHC -Werror #-}
module Crypto (
      AESConf(..)
    , verifyAESConf
    , aesEncrypt
    , aesDecrypt
    ) where

import Data.ByteString (ByteString)
import qualified Codec.Crypto.AES as AES
import qualified Data.ByteString as BS

data AESConf = AESConf {
      aesKey :: BS.ByteString -- ^ AES key, has to be 32 bytes long
    , aesIV  :: BS.ByteString -- ^ initialization vector, has to be 16 bytes long
    } deriving (Eq, Ord, Show, Read)

verifyAESConf :: AESConf -> Either String ()
verifyAESConf (AESConf key iv) =
    if key_len == 32
       then if BS.length iv == 16
               then Right ()
               else Left $ "Invalid AES IV length: " ++ show iv_len ++ ", should be 16"
       else Left $ "Invalid AES key length: " ++ show key_len ++ ", should be 32"
    where
        key_len = BS.length key
        iv_len  = BS.length iv

aesEncrypt :: AESConf -> ByteString -> ByteString
aesEncrypt AESConf{aesKey = key, aesIV = iv} =
    AES.crypt' AES.CFB key iv AES.Encrypt

aesDecrypt :: AESConf -> ByteString -> ByteString
aesDecrypt AESConf{aesKey = key, aesIV = iv} =
    AES.crypt' AES.CFB key iv AES.Decrypt
