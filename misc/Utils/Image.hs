module Utils.Image (
    imgEncodeRFC2397
  , imgMimeType
  ) where

import qualified Data.ByteString as B

import KontraPrelude
import qualified Data.ByteString.RFC2397 as RFC2397

-- | Encode image data according to RFC2397. We try to detect content type
-- with 'imgMimeType' and if 'Nothing' is returned, we simply don't include it.
imgEncodeRFC2397 :: B.ByteString -> B.ByteString
imgEncodeRFC2397 img = RFC2397.encode (fromMaybe B.empty $ imgMimeType img) img

-- | Identify content type of supplied image data.
imgMimeType :: B.ByteString -> Maybe B.ByteString
imgMimeType bs
  | is_jpeg   = Just "image/jpeg"
  | is_png    = Just "image/png"
  | otherwise = Nothing
  where
    -- source: https://en.wikipedia.org/wiki/Magic_number_%28programming%29
    is_jpeg = B.pack [0xFF, 0xD8] `B.isPrefixOf` bs
           && B.pack [0xFF, 0xD9] `B.isSuffixOf` bs
    is_png  = B.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] `B.isPrefixOf` bs
