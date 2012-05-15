{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.JSON (
    -- Stuff that was here before
      getJSONField
    , getJSONStringFieldSafe  
    , getJSONStringField
    -- | Class for structures that have JSON object inside
    -- | Interface for structures that can bve read from JSON
    , jsempty
    , jsget
    , jsgetA
    , jsmodify
    , jsgetdef
    , jsmodifydef
    , jsset
    , jsrem
    , fromJSONString
    , fromJSONRational
    , fromJSONArray
    , JSONPath()
    , pathList
    , jsonType
    , jsonPack
    )where

import Text.JSON
import Control.Monad
import qualified Data.List.Utils as List
import Misc
import Text.JSON.FromJSValue
import qualified Data.Text as T
import Control.Applicative

fromJSONString :: JSValue -> String
fromJSONString (JSString s) = fromJSString s
fromJSONString x = error $ "Expected JSString but found " ++ show x

fromJSONRational :: JSValue -> Double
fromJSONRational (JSRational _ r) = fromRational r
fromJSONRational x = error $ "Expected JSString but found " ++ show x

fromJSONArray :: JSValue -> [JSValue]
fromJSONArray (JSArray a) = a
fromJSONArray x = error $ "Expected JSArray but found " ++ show x

getJSONField :: String -> JSObject JSValue -> Maybe JSValue
getJSONField s = lookup s .fromJSObject

getJSONStringFieldSafe :: String -> JSValue -> Either String String
getJSONStringFieldSafe name jsvalue = case jsvalue of
  JSObject obj -> case (getJSONField name obj) of
    Just (JSString s) -> Right $ fromJSString s
    Just x -> Left $ "Did not find String; instead, found " ++ show x
    Nothing -> Left $ "Field " ++ show name ++ " not found."
  _ -> Left $ "The JSON is not an object; " ++ show jsvalue

getJSONStringField :: String -> JSObject JSValue ->  String
getJSONStringField name obj =
    case (getJSONField name obj) of
        Just (JSString s) -> fromJSString s
        _ -> ""

instance JSON T.Text where
  readJSON a = T.pack <$> readJSON a
  showJSON = showJSON . T.unpack

instance (SafeEnum a) => FromJSValue a where
    fromJSValue = join . (fmap toSafeEnumInt) . fromJSValue
    
-- JSON Object construction; partial functions

class JSONPath a where
  pathList :: a -> [String]
  
instance JSONPath String where
  pathList a = [a]
  
instance JSONPath a => JSONPath [a] where
  pathList a = concatMap pathList a

jsempty :: JSValue
jsempty = JSObject $ toJSObject []

jsset :: (JSONPath path, JSON a) => path -> a -> JSValue -> Either String JSValue
jsset path val _ | pathList path == [] = Right $ showJSON val
jsset path val obj = let (p:ps) = pathList path in
  modifyStrDef p (jsset ps val) jsempty obj

jsget :: JSONPath path => path -> JSValue -> Either String JSValue
jsget path obj = foldl (\a b -> a >>= getStr b) (Right obj) $ pathList path

jsgetA :: Int -> JSValue ->  Either String JSValue
jsgetA 0 (JSArray (h:_)) = Right h
jsgetA v (JSArray (_:hs)) = jsgetA (v-1) (JSArray hs)
jsgetA _ _ = Left "No index found"

jsgetdef :: JSONPath path => path -> JSValue -> JSValue -> Either String JSValue
jsgetdef path _ val | pathList path == [] = Right val
jsgetdef path d obj | [p] <- pathList path = getStrDef p d obj
jsgetdef path d obj = let (p:ps) = pathList path in
  getStr p obj >>= jsgetdef ps d

jsmodify :: JSONPath path => path -> (JSValue -> Either String JSValue) -> JSValue -> Either String JSValue
jsmodify path f val | pathList path == [] = f val
jsmodify path f obj@(JSObject _) = jsget path obj >>= f >>= \v -> jsset path v obj
jsmodify _ _ obj = Left $ "Cannot set value on non-object: " ++ encode obj

jsmodifydef :: JSONPath path => path -> (JSValue -> Either String JSValue) -> JSValue -> JSValue -> Either String JSValue
jsmodifydef path f _ val | pathList path == [] = f val
jsmodifydef path f d obj@(JSObject _) = jsgetdef path d obj >>= f >>= \v -> jsset path v obj
jsmodifydef _ _ _ obj = Left $ "Cannot set value on non-object: " ++ encode obj

jsrem :: JSONPath path => path -> JSValue -> Either String JSValue
jsrem path val | pathList path == [] = Right val
jsrem path val | [p] <-pathList path = remStr p val
jsrem path val = jsmodify (init $ pathList path) (remStr (last $ pathList path)) val

getStr :: String -> JSValue -> Either String JSValue
getStr k obj@(JSObject kvs) = maybe (Left $ "Key " ++ show k ++ " not found in : " ++ encode obj) 
                              Right $ lookup k (fromJSObject kvs)
getStr _ obj = Left $ "Cannot get value on non-object: " ++ encode obj

getStrDef :: JSON a => String -> a -> JSValue -> Either String JSValue
getStrDef k d (JSObject kvs) = maybe (Right $ showJSON d) Right $ lookup k (fromJSObject kvs)
getStrDef _ _ obj = Left $ "Cannot get value on non-object: " ++ encode obj

setStr :: JSON a => String -> a -> JSValue -> Either String JSValue
setStr k v (JSObject jsobj) = Right $ JSObject $ toJSObject $ List.addToAL (fromJSObject jsobj) k $ showJSON v
setStr _ _ obj            = Left $ "Cannot set value on non-object: " ++ encode obj

modifyStrDef :: JSON a => String -> (JSValue -> Either String JSValue) -> a -> JSValue -> Either String JSValue
modifyStrDef k f d obj@(JSObject _) = getStrDef k d obj >>= f >>= \v -> setStr k v obj
modifyStrDef _ _ _ obj              = Left $ "Cannot set value on non-object: " ++ encode obj

remStr :: String -> JSValue -> Either String JSValue
remStr k (JSObject jsobj) = case lookup k (fromJSObject jsobj) of
  Nothing -> Left  $ "Key " ++ show k ++ " not found in : " ++ encode jsobj
  Just _  -> Right $ JSObject $ toJSObject $ filter ((/=) k . fst) $ fromJSObject jsobj
remStr _ obj = Left $ "Cannot remove value on non-object: " ++ encode obj

_modifyStr :: String -> (JSValue -> Either String JSValue) -> JSValue -> Either String JSValue
_modifyStr k f obj@(JSObject _) = getStr k obj >>= f >>= \v -> setStr k v obj
_modifyStr _ _ obj = Left $ "Cannot set value on non-object: " ++ encode obj

jsonType :: JSValue -> String
jsonType (JSString _) = "string"
jsonType (JSObject _) = "object"
jsonType (JSRational _ _) = "number"
jsonType (JSBool _) = "bool"
jsonType (JSNull) = "null"
jsonType (JSArray _) = "array"

jsonPack :: [(String,String)] -> JSValue
jsonPack = JSObject . toJSObject . (mapSnd (JSString . toJSString))
