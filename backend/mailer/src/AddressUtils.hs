module AddressUtils (punyEncode) where

import Data.Text.Punycode (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

punyEncode :: Text -> Text
punyEncode s = name <> "@" <> encodedDomain
  where
    name :: Text
    name = T.takeWhile (/= '@') s

    domainElems :: [Text]
    domainElems = T.split (== '.') . T.tail $ T.dropWhile (/= '@') s

    encodeStr :: Text -> Text
    encodeStr = TE.decodeUtf8 . encode

    needsEncoding :: Text -> Bool
    needsEncoding str = str <> "-" /= encodeStr str

    encodedDomain :: Text
    encodedDomain = T.concat . intersperse "." $ map encodeElem domainElems

    encodeElem :: Text -> Text
    encodeElem str | needsEncoding str = "xn--" <> encodeStr str
                   | otherwise         = str
