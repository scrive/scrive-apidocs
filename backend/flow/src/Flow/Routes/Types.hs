module Flow.Routes.Types
  ( Get302
  , Host
  , AddFlowPrefix
  , flowPath
  , Version(..)
  )
 where

import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics
import Servant.API

type Get302 contentTypes a = Verb 'GET 302 contentTypes a

type Host = Text

type AddFlowPrefix api = "experimental" :> "flow" :> api

-- TODO derive this from AddFlowPrefix
flowPath :: Text
flowPath = "experimental/flow"

newtype Version = Version
  { buildCommit :: Text
  } deriving (Generic)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance FromJSON Version where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Version where
  toEncoding = genericToEncoding aesonOptions
