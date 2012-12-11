module Data.ByteString.RFC2397 (decode, encode) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Monoid ((<>))
import Prelude hiding (break)

break :: Char -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
break c i = case BS.break (==c) i of
  (a,r) -> case BS.uncons r of 
    Just (c',r') | c' == c -> Just (a,r')
    _                      -> Nothing

-- | Decode a data URI into its mimetype and content, according to RFC
-- 2397.  Assumes that character encoding is absent, and that encoding is base64.
decode :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
decode i = do
  ("data",i2) <- break ':' i
  (e,d) <- break ',' i2
  (mt, "base64") <- break ';' e
  return (mt, Base64.decodeLenient d)

-- | Encode a mimetype and content into a data URI using base64,
-- according to RFC 2397.
encode :: BS.ByteString -> BS.ByteString -> BS.ByteString
encode mt i = "data:" <> mt <> ";base64," <> Base64.encode i
