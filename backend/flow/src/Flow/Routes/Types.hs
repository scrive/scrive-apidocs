module Flow.Routes.Types
  ( Get302
  , Host
  , AddFlowPrefix
  , flowPath
  , Url(..)
  )
 where

import Data.Aeson
import Servant.API

type Get302 contentTypes a = Verb 'GET 302 contentTypes a

type Host = Text

type AddFlowPrefix api = "experimental" :> "flow" :> api

-- TODO derive this from AddFlowPrefix
flowPath :: Text
flowPath = "experimental/flow"

newtype Url = Url { fromUrl :: Text }
  deriving (Eq, FromJSON, Ord, Show, ToJSON)
