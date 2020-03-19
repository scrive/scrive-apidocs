
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.SMSLinkShortening
-- Author      :  Mariusz Rak
-- Stability   :  development
-- Portability :  portable
--
-- Utility for making some URL data parts shorter - print + parse
--
-----------------------------------------------------------------------------
module Util.SMSLinkShortening (
    short
  , unshort
) where

import Data.Int
import Data.Word
import Numeric
import qualified Data.Text as T

import Doc.SignatoryLinkID
import MagicHash
import Utils.Read

-- Encoding is based on
base :: Word64
base = 32 -- 2 ^ 5

textLength :: Int
textLength = 13 -- ~64 / 5

-- We have little more char then we need, but it doesn't matter
charMap :: String
charMap = take (fromIntegral base) $ ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']


short :: (SignatoryLinkID, MagicHash) -> T.Text
short (sid, mh) =
  T.pack $ encodeInt64 (fromSignatoryLinkID sid) <> encodeInt64 (unMagicHash mh)

unshort :: T.Text -> Maybe (SignatoryLinkID, MagicHash)
unshort txt = do
  if T.length txt == 2 * textLength && T.all (`elem` charMap) txt
    then Just (slid, mh)
    else Nothing
  where
    slid = unsafeSignatoryLinkID $ decodeInt64 h
    mh   = unsafeMagicHash $ decodeInt64 t
    (h :: String, t :: String) = splitAt textLength $ T.unpack txt

decodeInt64 :: String -> Int64
decodeInt64 = fromIntegral . decodeWord64
  where
    decodeWord64 :: String -> Word64
    decodeWord64 [] = 0
    decodeWord64 (c : r) =
      fromIntegral (fromMaybe 0 $ elemIndex c charMap) + base * decodeWord64 r

encodeInt64 :: Int64 -> String
encodeInt64 x = reverse . pad0 textLength $ showIntAtBase base
                                                          char
                                                          (fromIntegral x :: Word64)
                                                          ""
  where char i = charMap !! i
