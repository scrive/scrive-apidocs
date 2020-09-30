module Flow.Model.Types.AuthorizationConfiguration
  ( AuthorizationConfiguration(..)
  , AuthProvider(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics

import Flow.Routes.Api (OnfidoMethod)

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }

-- TODO Flow: think about useing `safe-json` package for versioning and DB migrations.
data AuthorizationConfiguration = AuthorizationConfiguration
    { provider :: AuthProvider
    , maxFailures :: Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON AuthorizationConfiguration where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthorizationConfiguration where
  toEncoding = genericToEncoding aesonOptions

data AuthProvider
    = SmsPin
    | Onfido
      { method :: OnfidoMethod
      }
  deriving (Show, Eq, Generic)

instance FromJSON AuthProvider where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthProvider where
  toEncoding = genericToEncoding aesonOptions
