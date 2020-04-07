module Doc.Screenshot
  ( Screenshot(..),
  ) where

import Text.JSON.FromJSValue (FromJSValue(..), fromJSValueField)
import Text.JSON.Gen (runJSONGen, value)
import Text.JSON.ToJSValue (ToJSValue(..))
import qualified Data.ByteString.UTF8 as BS

import MinutesTime
import qualified Data.ByteString.RFC2397 as RFC2397

data Screenshot = Screenshot
 { time  :: UTCTime
 , image :: BS.ByteString
 }
 deriving (Show, Eq, Ord)

instance FromJSValue Screenshot where
  fromJSValue = do
    time'  <- fromJSValueField "time"
    image' <- fromJSValueField "image"
    s      <- fromJSValue
    return $ if isJust time' && isJust image'
      then f time' image'
      else f (fst <$> s) (snd <$> s) -- old array format
    where
      f :: Maybe String -> Maybe String -> Maybe Screenshot
      f t i =
        Screenshot
          <$> (parseTimeISO =<< t)
          <*> (fmap snd . RFC2397.decode . BS.fromString =<< i)

instance ToJSValue Screenshot where
  toJSValue (Screenshot time' image') = runJSONGen $ do
    value "time" . toJSValue $ formatTimeISO time'
    value "image" . toJSValue $ BS.toString (RFC2397.encode "image/jpeg" image')
