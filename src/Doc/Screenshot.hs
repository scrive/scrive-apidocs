module Doc.Screenshot
  ( Screenshot(..),
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import DB.Binary (Binary(..))
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS
import MinutesTime (MinutesTime,parseMinutesTimeRealISO, formatMinutesTimeRealISO)
import Text.JSON.FromJSValue (FromJSValue(..), fromJSValueField)
import Text.JSON.Gen (value, runJSONGen)
import Text.JSON.ToJSValue (ToJSValue(..))

data Screenshot = Screenshot
 { time  :: MinutesTime
 , image :: Binary
 }
 deriving (Show, Eq, Ord)

instance FromJSValue Screenshot where
  fromJSValueM = do
    time' <- (parseMinutesTimeRealISO =<<)  `liftM` fromJSValueField "time"
    image' <- (fmap (Binary . snd) . RFC2397.decode . BS.fromString =<<) `liftM` fromJSValueField "image"
    return $ Screenshot <$> time' <*> image'

instance ToJSValue Screenshot where
  toJSValue (Screenshot time' (Binary image')) = runJSONGen $ do
    value "time"  $ toJSValue $ formatMinutesTimeRealISO time'
    value "image" $ toJSValue $ BS.toString $ RFC2397.encode "image/jpeg" image'
