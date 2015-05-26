module Happstack.Fields where

import Control.Monad.IO.Class
import Happstack.Server
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Text.JSON as J

import KontraPrelude
import Utils.Monoid
import Utils.Read

-- | Since we sometimes want to get 'Maybe' and also we wont work with
-- newer versions of happstack here is.  This should be droped when
-- new version is globaly established.
getDataFn' :: (HasRqData m, MonadIO m, ServerMonad m)
           => RqData a -> m (Maybe a)
getDataFn' fun = either (const Nothing) Just `liftM` getDataFn fun

isFieldSet :: (HasRqData m, MonadIO m, ServerMonad m)
           => String -> m Bool
isFieldSet name = isJust `liftM` getField name

getFields :: (HasRqData m, MonadIO m, ServerMonad m)
          => String -> m [String]
getFields name = map BSLU.toString `liftM` fromMaybe [] `liftM` getDataFn' (lookInputList name)

getField :: (HasRqData m, MonadIO m, ServerMonad m)
         => String -> m (Maybe String)
getField name = (listToMaybe . reverse) `liftM` getFields name

getFieldJSON :: (HasRqData m, MonadIO m, ServerMonad m)
         => String -> m (Maybe J.JSValue)
getFieldJSON name = do
  res <- getField name
  case fmap J.decode res of
     Just (J.Ok js) -> return $ Just js
     _ -> return $ Nothing

getField' :: (HasRqData m, MonadIO m, ServerMonad m)
          => String -> m String
getField' name = fromMaybe "" `liftM` getField name

readField :: (HasRqData m, MonadIO m, Read a, ServerMonad m)
          => String -> m (Maybe a)
readField name = (join . liftM maybeRead) `liftM` getField name

getFieldBS :: (HasRqData m, MonadIO m, ServerMonad m)
          => String -> m (Maybe BSLU.ByteString)
getFieldBS name = (listToMaybe . reverse) `liftM` fromMaybe [] `liftM` getDataFn' (lookInputList name)


getFileField :: (HasRqData m, MonadIO m, ServerMonad m)
             => String -> m (Maybe BS.ByteString)
getFileField name = do
  finput <- getDataFn (lookInput name)
  case finput of
    Right (Input contentspec _ _) -> case contentspec of
      Left filepath -> (emptyToNothing . BSL.toStrict) `liftM` liftIO (BSL.readFile filepath)
      Right content -> return . emptyToNothing $ BSL.toStrict content
    _ -> return Nothing

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name = do
  inputs <- lookInputs name
  return [value | Input (Right value) _ _ <- inputs]
