module Doc.SignatoryScreenshots
  ( SignatoryScreenshots(..)
  , emptySignatoryScreenshots
  , resolveReferenceScreenshotNames
  , getReferenceScreenshot
  , referencePath
  , validReferenceName
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Text.JSON(Result(..), decode, JSValue(..), fromJSString)
import Text.JSON.FromJSValue
import Text.JSON.Gen (value, runJSONGen)
import Text.JSON.ToJSValue (ToJSValue(..))
import qualified Control.Exception.Lifted as E

import Doc.Screenshot
import KontraPrelude

data SignatoryScreenshots = SignatoryScreenshots
  { first         :: Maybe Screenshot
  , signing       :: Maybe Screenshot
  , reference     :: Maybe (Either String Screenshot)
  } deriving Show

getReferenceScreenshot :: SignatoryScreenshots -> Maybe Screenshot
getReferenceScreenshot s = either (const Nothing) Just =<< reference s

emptySignatoryScreenshots :: SignatoryScreenshots
emptySignatoryScreenshots = SignatoryScreenshots Nothing Nothing Nothing

instance FromJSValue SignatoryScreenshots where
  fromJSValue = do
    first <- fromJSValueField "first"
    signing <- fromJSValueField "signing"
    reference <- fromJSValueField "reference" >>= \case
      Just (JSString s) -> return $ Just $ Left $ fromJSString s
      Just (JSObject s) -> return $ Right <$> fromJSValue (JSObject s)
      _                 -> return Nothing
    return $ Just $ SignatoryScreenshots first signing reference

instance ToJSValue SignatoryScreenshots where
  toJSValue ss = runJSONGen $ do
    f "first" (fmap toJSValue . first)
    f "signing" (fmap toJSValue . signing)
    f "reference" (fmap (either toJSValue toJSValue) . reference)
   where f n v = case v ss of
                   Nothing -> return ()
                   Just s  -> value n s

referencePath :: String -> FilePath
referencePath name = "files/reference_screenshots/" ++ name ++ ".json"

validReferenceName :: String -> Bool
validReferenceName n = n `elem` ["author", "mobile", "standard"]

resolveReferenceScreenshotNames :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m) => SignatoryScreenshots -> m (Maybe SignatoryScreenshots)
resolveReferenceScreenshotNames s =
  case reference s of
    Nothing -> return $ Just s
    Just (Right _) -> return $ Just s
    Just (Left name) | validReferenceName name -> do
        Ok json <- decode <$> (liftIO $ readFile $ referencePath name)
        Just r <- return $ fromJSValue json
        return $ Just $ s{ reference = Just (Right r) }
        `E.catch`
          \E.SomeException{} -> return Nothing
    _ -> return Nothing
