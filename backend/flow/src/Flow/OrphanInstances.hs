{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StrictData #-}
module Flow.OrphanInstances () where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Time (MonadTime(currentTime))
import Data.Either.Combinators (mapBoth)
import Servant.API
import Servant.HTML.Blaze
import Servant.Server (Handler)
import Text.Blaze.Html5
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

-- TODO cache the time when the handler starts
instance MonadTime Handler where
  currentTime = liftIO currentTime

instance MimeUnrender HTML Html where
  mimeUnrender _ bs = mapBoth show toHtml . T.decodeUtf8' $ BSL.toStrict bs
