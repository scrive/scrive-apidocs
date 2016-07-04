module Log.Utils (
    equalsExternalBS
  , equalsExternalBSL
  , localRandomID
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Log.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Crypto.RNG
import KontraPrelude

equalsExternalBS :: T.Text -> BS.ByteString -> Pair
equalsExternalBS name value
  -- If any char is lower than 32 and not \t, \n or \r
  -- then we treat is like binary and encode as base64.
  | BS.any ((< '\32') && not isSpace) value = resultBase64
  -- Otherwise we check whether it's valid UTF-8.
  | otherwise = case T.decodeUtf8' value of
    -- If it is, great.
    Right tvalue -> name .= tvalue
    -- If it's not, decode it as-is byte-by-byte as
    -- it's most likely text in some other encoding.
    Left _ -> name .= T.decodeLatin1 value
  where
    resultBase64 = (name <> "_base64") .= T.decodeLatin1 (B64.encode value)

equalsExternalBSL :: T.Text -> BSL.ByteString -> Pair
equalsExternalBSL name = equalsExternalBS name . BSL.toStrict

localRandomID :: (MonadLog m, CryptoRNG m) => T.Text -> m a -> m a
localRandomID name action = do
  uuid <- randomBytes 8
  let b64uuid = T.decodeUtf8 $ B16.encode uuid
  localData [name .= b64uuid] action
