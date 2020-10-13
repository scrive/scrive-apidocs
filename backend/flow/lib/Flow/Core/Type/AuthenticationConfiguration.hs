{-# LANGUAGE TemplateHaskell #-}
module Flow.Core.Type.AuthenticationConfiguration
  ( AuthenticationConfiguration(..)
  , AuthenticationProvider(..)
  , AuthenticationProviderOnfidoData(..)
  , OnfidoDocumentType(..)
  , OnfidoMethod(..)
  , defaultOnfidoDocumentTypes
  ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Set (Set)
import Data.Text
import GHC.Generics
import Optics
import qualified Data.Set as Set

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }

data AuthenticationConfiguration = AuthenticationConfiguration
    { provider :: AuthenticationProvider
    , maxFailures :: Int
    }
  deriving (Show, Eq, Generic)

-- TODO FLOW-402: add sms_otp and deprecate sms_pin
instance ToJSON AuthenticationConfiguration where
  toJSON _ = unexpectedError "AuthenticationConfiguration: toJson not implemented!"
  toEncoding AuthenticationConfiguration {..} =
    pairs $ "max_failures" .= maxFailures <> fieldsByProvider provider
    where
      fieldsByProvider :: AuthenticationProvider -> Series
      fieldsByProvider SmsOtp = "provider" .= ("sms_pin" :: Text)
      fieldsByProvider (Onfido AuthenticationProviderOnfidoData {..}) =
        "provider"
          .= ("onfido" :: Text)
          <> ("method" .= method)
          <> ("allowed_document_types" .= allowedDocumentTypes)

-- TODO FLOW-402: add sms_otp and deprecate sms_pin
instance FromJSON AuthenticationConfiguration where
  parseJSON = withObject "FlowCallbackEventV1Envelope" $ \o -> do
    provider <- o .: "provider" >>= getProvider o
    AuthenticationConfiguration <$> pure provider <*> o .: "max_failures"
    where
      getProvider :: Object -> Text -> Parser AuthenticationProvider
      getProvider _ "sms_pin" = pure SmsOtp
      getProvider o "onfido" =
        (\a b -> Onfido $ AuthenticationProviderOnfidoData a b)
          <$> (o .:? "method" .!= Document)
          <*> (o .:? "allowed_document_types" .!= defaultOnfidoDocumentTypes)
      getProvider _ type' =
        fail $ "Unknown AuthenticationConfiguration provider type: " <> unpack type'

data AuthenticationProvider
    = SmsOtp
    | Onfido AuthenticationProviderOnfidoData
  deriving (Show, Eq, Generic)

data OnfidoDocumentType
  = NationalIdentityCard
  | DrivingLicence
  | Passport
  | ResidencePermit
  deriving (Eq, Generic, Ord, Show)

instance FromJSON OnfidoDocumentType where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON OnfidoDocumentType where
  toEncoding = genericToEncoding aesonOptions

defaultOnfidoDocumentTypes :: Set OnfidoDocumentType
defaultOnfidoDocumentTypes =
  Set.fromList [NationalIdentityCard, DrivingLicence, Passport, ResidencePermit]

data AuthenticationProviderOnfidoData = AuthenticationProviderOnfidoData
    { method :: OnfidoMethod
    , allowedDocumentTypes :: Set OnfidoDocumentType
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
