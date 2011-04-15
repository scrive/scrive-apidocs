{-# OPTIONS_GHC -Werror #-}
module Crypto (
      AESConf(..)
    , verifyAESConf
    , aesEncrypt
    , aesDecrypt
    ) where

import Data.ByteString (ByteString)
import Data.Word
import qualified Crypto.Cipher.AES as AES
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
aesEncrypt AESConf{aesKey = bskey, aesIV = iv} = 
    AES.encryptCBC (initKey bskey) iv . align16

aesDecrypt :: AESConf -> ByteString -> Maybe ByteString
aesDecrypt AESConf{aesKey = bskey, aesIV = iv} s =
    if BS.length s `mod` 16 == 0
       then Just $ unalign16 $ AES.decryptCBC (initKey bskey) iv s
       else Nothing

initKey :: ByteString -> AES.Key
initKey bskey =
    case AES.initKey256 bskey of
         Right key -> key
         Left err  -> error $ "This should never happen: " ++ err

alignByte :: Word8
alignByte = 1

align16 :: ByteString -> ByteString
align16 s =
    case BS.length s `mod` 16 of
         0 -> s
         n -> BS.replicate (16-n) alignByte `BS.append` s

unalign16 :: ByteString -> ByteString
unalign16 = BS.dropWhile (== alignByte)
