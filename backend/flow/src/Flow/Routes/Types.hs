module Flow.Routes.Types
  ( HTML(..)
  , Get302
  , Host
  , AddFlowPrefix
  , flowPath
  , Url(..)
  )
 where

import Data.Aeson
import Data.Either.Combinators (mapLeft)
import Network.HTTP.Media ((//), (/:))
import Servant.API
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ t = BSL.fromStrict $ T.encodeUtf8 t

instance MimeUnrender HTML Text where
  mimeUnrender _ bs = mapLeft show (T.decodeUtf8' $ BSL.toStrict bs)

type Get302 contentTypes a = Verb 'GET 302 contentTypes a

type Host = Text

type AddFlowPrefix api = "experimental" :> "flow" :> api

-- TODO derive this from AddFlowPrefix
flowPath :: Text
flowPath = "experimental/flow"

newtype Url = Url Text
  deriving (Eq, FromJSON, Ord, Show, ToJSON)
