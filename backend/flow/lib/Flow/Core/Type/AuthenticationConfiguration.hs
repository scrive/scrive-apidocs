{-# LANGUAGE TemplateHaskell #-}
module Flow.Core.Type.AuthenticationConfiguration
  ( AuthenticationConfiguration(..)
  , OnfidoMethod(..)
  , AuthenticationProvider(..)
  , AuthenticationProviderOnfidoData(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Optics

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }

data AuthenticationConfiguration = AuthenticationConfiguration
    { provider :: AuthenticationProvider
    , maxFailures :: Int
    }
  deriving (Show, Eq, Generic)

instance ToJSON AuthenticationConfiguration where
  toJSON _ = unexpectedError "AuthenticationConfiguration: toJson not implemented!"
  toEncoding AuthenticationConfiguration {..} =
    pairs $ "max_failures" .= maxFailures <> fieldsByProvider provider
    where
      fieldsByProvider :: AuthenticationProvider -> Series
      fieldsByProvider SmsPin = "provider" .= ("sms_pin" :: Text)
      fieldsByProvider (Onfido AuthenticationProviderOnfidoData {..}) =
        "provider" .= ("onfido" :: Text) <> "method" .= method

instance FromJSON AuthenticationConfiguration where
  parseJSON = withObject "FlowCallbackEventV1Envelope" $ \o -> do
    provider <- o .: "provider" >>= getProvider o
    AuthenticationConfiguration <$> pure provider <*> o .: "max_failures"
    where
      getProvider :: Object -> Text -> Parser AuthenticationProvider
      getProvider _ "sms_pin" = pure SmsPin
      getProvider o "onfido" =
        Onfido . AuthenticationProviderOnfidoData <$> o .:? "method" .!= Document
      getProvider _ type' =
        fail $ "Unknown AuthenticationConfiguration provider type: " <> unpack type'

data AuthenticationProvider
    = SmsPin
    | Onfido AuthenticationProviderOnfidoData
  deriving (Show, Eq, Generic)

newtype AuthenticationProviderOnfidoData = AuthenticationProviderOnfidoData
    { method :: OnfidoMethod
    }
  deriving (Show, Eq, Generic)

data OnfidoMethod
  = Document
  | DocumentAndPhoto
  deriving (Show, Eq, Generic)

instance FromJSON OnfidoMethod where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON OnfidoMethod where
  toEncoding = genericToEncoding aesonOptions

makeFieldLabelsWith noPrefixFieldLabels ''AuthenticationConfiguration
makeFieldLabelsWith noPrefixFieldLabels ''AuthenticationProviderOnfidoData
