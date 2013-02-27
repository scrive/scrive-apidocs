module Doc.Screenshot
  ( Screenshot(..),
  ) where

import DB.Binary (Binary(..))
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS
import Text.JSON.FromJSValue
import Control.Applicative

data Screenshot = Screenshot {
    mimetype :: String
  , image :: Binary
  }
  deriving (Show, Eq, Ord)

instance FromJSValue Screenshot where
  fromJSValue s = do
    (mt, i) <- RFC2397.decode =<< BS.fromString <$> fromJSValue s
    if (mt `elem` ["image/jpeg", "image/png"])
      then Just $ Screenshot (BS.toString mt) (Binary i)
      else Nothing

