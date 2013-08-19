module Doc.Screenshot
  ( Screenshot(..),
  ) where

import DB.Binary (Binary(..))
import qualified Data.ByteString.RFC2397 as RFC2397
import qualified Data.ByteString.UTF8 as BS
import Text.JSON.FromJSValue
import Control.Applicative

data Screenshot = Screenshot {
    image :: Binary
  }
  deriving (Show, Eq, Ord)

instance FromJSValue Screenshot where
  fromJSValue s = do
    (_mimetype, i) <- RFC2397.decode =<< BS.fromString <$> fromJSValue s
    return $ Screenshot (Binary i)
