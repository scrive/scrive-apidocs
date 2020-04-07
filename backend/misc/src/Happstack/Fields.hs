module Happstack.Fields where

import Happstack.Server
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Text as T
import qualified Text.JSON as J

-- | Since we sometimes want to get 'Maybe' and also we wont work with
-- newer versions of happstack here is.  This should be droped when
-- new version is globaly established.
getDataFn' :: (HasRqData m, ServerMonad m) => RqData a -> m (Maybe a)
getDataFn' fun = either (const Nothing) Just <$> getDataFn fun

isFieldSet :: (HasRqData m, ServerMonad m) => Text -> m Bool
isFieldSet name = isJust <$> getField name

getFields :: (HasRqData m, ServerMonad m) => Text -> m [Text]
getFields name =
  map (T.pack . BSLU.toString) . fromMaybe [] <$> getDataFn' (lookInputList name)

getField :: (HasRqData m, ServerMonad m) => Text -> m (Maybe Text)
getField name = listToMaybe . reverse <$> getFields name

getFieldJSON :: (HasRqData m, ServerMonad m) => Text -> m (Maybe J.JSValue)
getFieldJSON name = do
  res <- getField name
  case fmap (J.decode . T.unpack) res of
    Just (J.Ok js) -> return $ Just js
    _              -> return Nothing

getField' :: (HasRqData m, ServerMonad m) => Text -> m Text
getField' name = fromMaybe "" <$> getField name

readField :: (HasRqData m, Read a, ServerMonad m) => Text -> m (Maybe a)
readField name = (maybeRead =<<) <$> getField name

getFieldBS :: (HasRqData m, ServerMonad m) => Text -> m (Maybe BSLU.ByteString)
getFieldBS name =
  listToMaybe . reverse . fromMaybe [] <$> getDataFn' (lookInputList name)

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@)
lookInputList :: Text -> RqData [BSL.ByteString]
lookInputList name = do
  inputs <- lookInputs $ T.unpack name
  return [ value | Input (Right value) _ _ <- inputs ]
