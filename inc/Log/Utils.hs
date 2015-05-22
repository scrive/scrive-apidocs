module Log.Utils (
    localRandomID
  ) where

import Data.Aeson
import Log.Class
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Crypto.RNG
import KontraPrelude

localRandomID :: (MonadLog m, CryptoRNG m) => T.Text -> m a -> m a
localRandomID name action = do
  uuid <- randomBytes 8
  localData [name .= (T.decodeUtf8 $ B16.encode uuid)] action
