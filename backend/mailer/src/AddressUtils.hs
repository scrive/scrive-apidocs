module AddressUtils (punyEncode) where

import Data.ByteString.Char8 (unpack)
import Data.String.Utils (split)
import Data.Text (pack)
import Data.Text.Punycode (encode)

punyEncode :: String -> String
punyEncode s = name ++ "@" ++ encodedDomain
  where name = takeWhile (/= '@') s
        domainElems = split "." $ tail $ dropWhile (/= '@') s
        encodeStr = unpack . encode . pack
        needsEncoding str = str ++ "-" /= encodeStr str
        encodedDomain = concat $ intersperse "." $ map encodeElem domainElems
        encodeElem str | needsEncoding str = "xn--" ++ encodeStr str
                       | otherwise = str
