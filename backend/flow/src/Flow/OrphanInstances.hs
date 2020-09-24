{-# OPTIONS_GHC -Wno-orphans #-}
module Flow.OrphanInstances () where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Time (MonadTime(currentTime))
import Data.Aeson
import Data.Either.Combinators (mapBoth)
import Data.Unjson
import Servant.API
import Servant.HTML.Blaze
import Servant.Server (Handler)
import Text.Blaze.Html5
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

import Doc.API.V2.JSON.Misc (unjsonAuthenticationToViewMethod)
import Doc.Types.SignatoryLink (AuthenticationToViewMethod)

-- TODO cache the time when the handler starts
instance MonadTime Handler where
  currentTime = liftIO currentTime

instance MimeUnrender HTML Html where
  mimeUnrender _ bs = mapBoth show toHtml . T.decodeUtf8' $ BSL.toStrict bs

instance ToJSON AuthenticationToViewMethod where
  toJSON     = unexpectedError "Unexpected toJSON call for AuthenticationToViewMethod"
  toEncoding = toEncoding . unjsonToJSON unjsonAuthenticationToViewMethod
