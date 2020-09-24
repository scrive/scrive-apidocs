module Text.JSON.Convert (jsonToAeson, Base64ByteString, toBase64, fromBase64) where

import Control.Arrow ((***))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Ratio
import Data.Scientific
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Text.JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

jsonToAeson :: JSValue -> Value
jsonToAeson JSNull               = Null
jsonToAeson (JSBool b          ) = Bool b
jsonToAeson (JSRational _ ratio) = Number $ if denominator ratio == 1
  then scientific (numerator ratio) 0
  -- Convert to Double first as converting from Rational to Scientific
  -- with fromRational diverges if a number has infinite decimal expansion.
  else let x = fromRational ratio :: Double in fromFloatDigits x
jsonToAeson (JSString s  ) = String . T.pack $ fromJSString s
jsonToAeson (JSArray  arr) = Array . V.fromList $ map jsonToAeson arr
jsonToAeson (JSObject obj) =
  Object . H.fromList . map (T.pack *** jsonToAeson) $ fromJSObject obj

-- | A ByteString whose JSON representation is a Base64 string.
newtype Base64ByteString = Base64ByteString { fromBase64 :: ByteString } deriving Show

toBase64 :: ByteString -> Base64ByteString
toBase64 = Base64ByteString

instance FromJSON Base64ByteString where
  parseJSON =
    withText "Base64" $ either fail (pure . Base64ByteString) . B64.decode . encodeUtf8

instance ToJSON Base64ByteString where
  toJSON (Base64ByteString dat) = String . decodeLatin1 $ B64.encode dat
