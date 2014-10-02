module Doc.Screenshot
  ( Screenshot(..),
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)
import Database.PostgreSQL.PQTypes (Binary(..))
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS
import MinutesTime
import Text.JSON.FromJSValue (FromJSValue(..), fromJSValueField)
import Text.JSON.Gen (value, runJSONGen)
import Text.JSON.ToJSValue (ToJSValue(..))

data Screenshot = Screenshot
 { time  :: MinutesTime
 , image :: Binary BS.ByteString
 }
 deriving (Show, Eq, Ord)

instance FromJSValue Screenshot where
  fromJSValue = do
    time' <- fromJSValueField "time"
    image' <- fromJSValueField "image"
    s <- fromJSValue
    return $ if isJust time' && isJust image'
      then f time' image'
      else f (fst <$> s) (snd <$> s) -- old array format
   where f :: Maybe String -> Maybe String -> Maybe Screenshot
         f t i = Screenshot <$> (parseMinutesTimeISO =<< t)
                             <*> (fmap (Binary . snd) . RFC2397.decode . BS.fromString =<< i)

instance ToJSValue Screenshot where
  toJSValue (Screenshot time' (Binary image')) = runJSONGen $ do
    value "time"  $ toJSValue $ formatMinutesTimeISO time'
    value "image" $ toJSValue $ BS.toString $ RFC2397.encode "image/jpeg" image'
