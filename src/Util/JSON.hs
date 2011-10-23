module Util.JSON (
    -- 
      getJSONField
    , getJSONStringFieldSafe  
    , getJSONStringField
    -- | Class for structures that have JSON object inside
    ,  JSONContainer(..)
    -- | Diggers - Asking local envirement about JSON object
    , askJSONBS 
    , askJSONString       
    , askJSONInteger 
    , askJSONBase64
    , askJSONList
    , askJSONLocal
    , askJSONLocalList
    , askJSONLocalMap 
    , askJSONField       
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
    
instance (JSONContainer c) => JSONContainer (c,a) where
    getJSON (c,_)  = getJSON c
    setJSON j (c,a) = (setJSON j c,a)


askJSONBS ::(JSONContainer c , MonadReader c m) => String -> m (Maybe BS.ByteString)
askJSONBS = liftM (liftM BS.fromString) . askJSONString

askJSONString :: (JSONContainer c , MonadReader c m) =>String -> m (Maybe String)
askJSONString s = askJSONLocal s (liftM fromString askJSON)
    where
        fromString (JSString string) = Just $ fromJSString string
        fromString _ = Nothing
        
askJSONInteger :: (JSONContainer c , MonadReader c m) =>String -> m  (Maybe Integer)
askJSONInteger s = askJSONLocal s (liftM fromNumerator askJSON)
    where
        fromNumerator (JSRational _ r) = Just $ numerator r
        fromNumerator _ = Nothing        

askJSONBase64 ::(JSONContainer c , MonadReader c m) =>String -> m  (Maybe BS.ByteString)
askJSONBase64 s =  do
    coded <- liftM (liftM BASE64.decode) (askJSONBS s)
    case coded of
         Just (Right r) -> return $ Just r
         _ -> return Nothing

askJSONList :: (JSONContainer c , MonadReader c m) => String -> m (Maybe [JSValue])
askJSONList s = askJSONLocal s askJSONLocalList

askJSONLocalList :: (JSONContainer c , MonadReader c m) => m (Maybe [JSValue])
askJSONLocalList = liftM fromList askJSON
    where
        fromList (JSArray list) = Just list
        fromList _ = Nothing


-- Locallized diggers. The diffrence is that they run themself in local context. Version for objects and lists

askJSONLocal :: (JSONContainer c , MonadReader c m) =>  String -> m (Maybe a) -> m (Maybe a)
askJSONLocal s digger = do
    mobj <- askJSONField s
    case mobj of
         Just obj -> local (setJSON obj) (digger)
         Nothing -> return Nothing

askJSONLocalMap :: (JSONContainer c , MonadReader c m) => m (Maybe a) -> m (Maybe [a])
askJSONLocalMap digger = do
    mlist <- askJSONLocalList
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




askJSONField ::  (JSONContainer c , MonadReader c m) => String -> m (Maybe JSValue)
askJSONField s = liftM fromObject askJSON
    where
      fromObject (JSObject object) = lookup s $ fromJSObject object
      fromObject _ = Nothing         

askJSON :: (JSONContainer c , MonadReader c m) => m JSValue
askJSON = liftM getJSON ask
        
-- | Simple runner        
withJSON :: (Monad m) => JSValue -> ReaderT JSValue m a -> m a
withJSON j a = runReaderT a j