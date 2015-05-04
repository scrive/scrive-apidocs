module Text.JSON.Convert (jsonToAeson) where

import Control.Arrow ((***))
import Data.Aeson
import Data.Attoparsec.Number
import Data.Ratio
import Text.JSON
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import KontraPrelude

jsonToAeson :: JSValue -> Value
jsonToAeson JSNull = Null
jsonToAeson (JSBool b) = Bool b
jsonToAeson (JSRational _ ratio) = Number $ if denominator ratio == 1
  then I $ numerator ratio
  else D $ fromRational ratio
jsonToAeson (JSString s) = String . T.pack $ fromJSString s
jsonToAeson (JSArray arr) = Array . V.fromList $ map jsonToAeson arr
jsonToAeson (JSObject obj) = Object . H.fromList . map (T.pack *** jsonToAeson) $ fromJSObject obj
