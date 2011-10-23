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
    )where

import Text.JSON
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as BASE64
import Data.Ratio
import Control.Monad.Identity
import Data.Maybe

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