{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}
module Util.JSON (
    -- Stuff that was here before
      getJSONField
    , getJSONStringFieldSafe  
    , getJSONStringField
    -- | Class for structures that have JSON object inside
    ,  JSONContainer(..)
    -- | Interface for structures that can bve read from JSON
    ,  FromJSON(..) 
    -- | Diggers - Asking local envirement about JSON object
    , fromJSONLocal
    , fromJSONLocalMap 
    , fromJSONField       
    , fromJSONFieldBase64
    , askJSON 
    -- | Simple runner
    , withJSON
    , jsget
    , jsmodify
    , jsset
    )where

import Text.JSON
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as BASE64
import Data.Ratio
import Control.Monad.Identity
import Data.Maybe
import qualified Data.List.Utils as List

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

-- | Class of containers where inside sits JSON object
-- | Getter an setter are required. 
-- | Fix Idea - there probably is generic for that
class JSONContainer a where
    getJSON :: a -> JSValue
    setJSON :: JSValue -> a -> a
    
instance JSONContainer JSValue where
    getJSON = id
    setJSON = const
    
-- FromJSON - structures that can be parsed from JSON
-- Instances must declare at least fromJSValue - simple parser
-- or fromJSON - diggers based parser
class FromJSON a where
    fromJSValue :: JSValue -> Maybe a
    fromJSValue j = runIdentity $ withJSON j $ liftM fromJSON askJSON
    fromJSON :: (JSONContainer c , MonadReader c m) => m (Maybe a)
    fromJSON = liftM fromJSValue askJSON


fromJSONField ::  (JSONContainer c , MonadReader c m, FromJSON a) => String -> m (Maybe a)
fromJSONField s = liftM fromObject askJSON
    where
      fromObject (JSObject object) = join (fmap fromJSValue (lookup s $ fromJSObject object))
      fromObject _ = Nothing         

fromJSONFieldBase64 ::(JSONContainer c , MonadReader c m) =>String -> m  (Maybe BS.ByteString)
fromJSONFieldBase64 s =  liftM dc (fromJSONField s)
    where dc s' = case (fmap BASE64.decode s') of
                            Just (Right r) -> Just r
                            _ -> Nothing
    

instance FromJSON JSValue where
    fromJSValue = Just

instance FromJSON String where
    fromJSValue (JSString string) = Just $ fromJSString string
    fromJSValue _ = Nothing

instance FromJSON BS.ByteString where
    fromJSValue s = fmap BS.fromString (fromJSValue s)

instance FromJSON Integer where
    fromJSValue (JSRational _ r) = Just $ numerator r
    fromJSValue _ = Nothing

instance (FromJSON a) => FromJSON [a] where
    fromJSValue (JSArray list) = let plist = map fromJSValue list 
                                 in if (all isJust plist) 
                                     then Just $ map fromJust plist
                                     else Nothing
    
    fromJSValue _ = Nothing

instance (FromJSON a) => FromJSON (Maybe a) where
    fromJSValue = join . fromJSValue

-- Locallized diggers. The diffrence is that they run themself in local context. Version for objects and lists

fromJSONLocal :: (JSONContainer c , MonadReader c m) =>  String -> m (Maybe a) -> m (Maybe a)
fromJSONLocal s digger = do
    mobj <- fromJSONField s
    case mobj of
         Just obj -> local (setJSON obj) (digger)
         Nothing -> return Nothing

fromJSONLocalMap :: (JSONContainer c , MonadReader c m) => m (Maybe a) -> m (Maybe [a])
fromJSONLocalMap digger = do
    mlist <- fromJSON
    case mlist of 
         Nothing -> return Nothing
         Just list ->  runDiggers list
    where
         runDiggers (j:js) = do
             mres <- local (setJSON j) digger
             case mres of
                 Just res -> do
                     mress <- runDiggers js
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing
                 _ -> return Nothing
         runDiggers _ = return $ Just []

-- | Getting JSON part of envirement
askJSON :: (JSONContainer c , MonadReader c m) => m JSValue
askJSON = liftM getJSON ask
        
-- | Simple runner        
withJSON :: (Monad m) => JSValue -> ReaderT JSValue m a -> m a
withJSON j a = runReaderT a j

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

jsmodify :: JSONPath path => path -> (JSValue -> Either String JSValue) -> JSValue -> Either String JSValue
jsmodify path f val | pathList path == [] = f val
jsmodify path f obj = jsget path obj >>= f >>= \v -> jsset path v obj

getStr :: String -> JSValue -> Either String JSValue
getStr k obj@(JSObject kvs) = maybe (Left $ "Key " ++ show k ++ " not found in : " ++ show obj) 
                              Right $ lookup k (fromJSObject kvs)
getStr _ obj = Left $ "Cannot get value on non-object: " ++ show obj

getStrDef :: JSON a => String -> a -> JSValue -> Either String JSValue
getStrDef k d (JSObject kvs) = maybe (Right $ showJSON d) Right $ lookup k (fromJSObject kvs)
getStrDef _ _ obj = Left $ "Cannot get value on non-object: " ++ show obj

setStr :: JSON a => String -> a -> JSValue -> Either String JSValue
setStr k v (JSObject jsobj) = Right $ JSObject $ toJSObject $ List.addToAL (fromJSObject jsobj) k $ showJSON v
setStr _ _ obj            = Left $ "Cannot set value on non-object: " ++ show obj

modifyStrDef :: JSON a => String -> (JSValue -> Either String JSValue) -> a -> JSValue -> Either String JSValue
modifyStrDef k f d obj@(JSObject _) = getStrDef k d obj >>= f >>= \v -> setStr k v obj
modifyStrDef _ _ _ obj              = Left $ "Cannot set value on non-object: " ++ show obj

_modifyStr :: String -> (JSValue -> Either String JSValue) -> JSValue -> Either String JSValue
_modifyStr k f obj@(JSObject _) = getStr k obj >>= f >>= \v -> setStr k v obj
_modifyStr _ _ obj = Left $ "Cannot set value on non-object: " ++ show obj
