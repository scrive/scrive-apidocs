module Text.JSON.Convert (jsonToAeson) where

import Control.Arrow ((***))
import Data.Aeson
import Data.Ratio
import Data.Scientific
import Text.JSON
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
