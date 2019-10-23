module Utils.Image (
    imgEncodeRFC2397
  , imgEncodeRFC2397ToText
  , imgMimeType
  ) where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString.RFC2397 as RFC2397

-- | Encode image data according to RFC2397. We try to detect content type
-- with 'imgMimeType' and if 'Nothing' is returned, we simply don't include it.
imgEncodeRFC2397 :: B.ByteString -> B.ByteString
imgEncodeRFC2397 img = RFC2397.encode (fromMaybe B.empty $ imgMimeType img) img

-- Encode image data to RFC2397 data URI text
imgEncodeRFC2397ToText :: B.ByteString -> Text
imgEncodeRFC2397ToText = TE.decodeLatin1 . imgEncodeRFC2397

-- | Identify content type of supplied image data.
imgMimeType :: B.ByteString -> Maybe B.ByteString
imgMimeType bs | is_jpeg   = Just "image/jpeg"
               | is_png    = Just "image/png"
               | otherwise = Nothing
  where
    -- source: https://en.wikipedia.org/wiki/Magic_number_%28programming%29
    is_jpeg =
      B.pack [0xFF, 0xD8] `B.isPrefixOf` bs && B.pack [0xFF, 0xD9] `B.isSuffixOf` bs
    is_png = B.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A] `B.isPrefixOf` bs
