{-# LANGUAGE OverlappingInstances #-}
module Text.JSON.ToJSValue (ToJSValue(..)) where

import Text.JSON
import qualified Data.Map as M

class ToJSValue a where
  toJSValue :: a -> JSValue

instance ToJSValue a => ToJSValue [a] where
  toJSValue = JSArray . fmap toJSValue

instance ToJSValue a => ToJSValue (M.Map String a) where
  toJSValue = JSObject . toJSObject . M.toList . fmap toJSValue

instance ToJSValue a => ToJSValue (Maybe a) where
  toJSValue = maybe JSNull toJSValue

instance Real a => ToJSValue a where
  toJSValue = JSRational True . toRational

instance ToJSValue JSValue where
  toJSValue = id

instance ToJSValue Bool where
  toJSValue = JSBool

instance ToJSValue String where
  toJSValue = JSString . toJSString
