{-# LANGUAGE OverlappingInstances #-}
module Util.JSON (
    -- Stuff that was here before
      getJSONField
    , getJSONStringFieldSafe  
    , getJSONStringField
    -- | Class for structures that have JSON object inside
    ,  JSONContainer(..)
    -- | Interface for structures that can bve read from JSON
    ,  FromJSON(..) 
    , eitherJSValue
    -- | Diggers - Asking local envirement about JSON object
    , fromJSONLocal
    , fromJSONLocalMap 
    , fromJSONLocalMapList
    , fromJSONField       
    , fromJSONFieldBase64
    , askJSON 
    -- | Simple runner
    , withJSON
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
    , withJSONFromField
    , JSONPath()
    , pathList
    , jsonType
    , jsonPack
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
import Misc
import Text.JSON.String
import Happstack.Server (HasRqData,ServerMonad)


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
    
eitherJSValue :: FromJSON a => JSValue -> Either String a
eitherJSValue j = maybe (Left $ "Could not convert value : " ++ show j) Right $ fromJSValue j

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
    
instance FromJSON Int where
    fromJSValue j = liftM fromIntegral (fromJSValue j :: Maybe Integer)

instance FromJSON Bool where
    fromJSValue (JSBool v) = Just $ v
    fromJSValue _ = Nothing
    
instance (SafeEnum a) => FromJSON a where
    fromJSValue = join . (fmap toSafeEnumInt) . fromJSValue
    
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
fromJSONLocalMap digger = fromJSONLocalMapList (repeat digger)


fromJSONLocalMapList :: (JSONContainer c , MonadReader c m) => [m (Maybe a)] -> m (Maybe [a])
fromJSONLocalMapList diggers = do
    mlist <- fromJSON
    case mlist of 
         Nothing -> return Nothing
         Just list ->  runDiggers list diggers
    where
         runDiggers (j:js) (d:ds) = do
             mres <- local (setJSON j) d
             case mres of
                 Just res -> do
                     mress <- runDiggers js ds
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing
                 _ -> return Nothing
         runDiggers _ _ = return $ Just []         
         
-- | Getting JSON part of envirement
askJSON :: (JSONContainer c , MonadReader c m) => m JSValue
askJSON = liftM getJSON ask
        
-- | Simple runner        
withJSON :: (Monad m) => JSValue -> ReaderT JSValue m a -> m a
withJSON j a = runReaderT a j

withJSONFromField :: (HasRqData m, MonadIO m, Functor m, ServerMonad m) => String -> ReaderT JSValue m (Maybe a) -> m (Maybe a)
withJSONFromField s a = do
    mj <- liftM (runGetJSON readJSObject) (getField' s)
    case mj of
         Right j -> withJSON j a
         Left _  -> return Nothing
         
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

