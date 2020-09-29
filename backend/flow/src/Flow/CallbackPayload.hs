module Flow.CallbackPayload
    ( FlowCallbackEventV1Envelope(..)
    , FlowCallbackEventV1(..)
    , RejectedEvent(..)
    , AuthenticationAttemptedResult(..)
    , AuthenticationAttemptedEvent(..)
    , AuthenticationProviderData(..)
    , OnfidoProviderData(..)
    )
  where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Text
import Data.Time
import GHC.Generics

import Flow.Core.Type.Callback
import Flow.EID.AuthConfig
import Flow.Id
import Flow.Names

data FlowCallbackEventV1Envelope = FlowCallbackEventV1Envelope
    { eventCreated :: UTCTime
    , flowInstanceId :: InstanceId
    , event :: FlowCallbackEventV1
    }
  deriving (Eq, Generic, Show)

instance ToJSON FlowCallbackEventV1Envelope where
  toJSON _ = unexpectedError "FlowCallbackEventV1Envelope: ToJson not implemented!"
  toEncoding FlowCallbackEventV1Envelope {..} =
    pairs
      $  ("version" .= V1)
      <> ("flow_instance_id" .= flowInstanceId)
      <> ("event_created" .= eventCreated)
      <> eventPairs event
    where
      eventPairs :: FlowCallbackEventV1 -> Series
      eventPairs Completed = "type" .= ("completed" :: Text)
      eventPairs Failed    = "type" .= ("failed" :: Text)
      eventPairs (Rejected RejectedEvent { userName, mRejectMessage, mDocumentName }) =
        ("type" .= ("flow_rejected" :: Text))
          <> ("user_name" .= userName)
          <> ("message" .= mRejectMessage)
          <> ("document_name" .= mDocumentName)
      eventPairs (AuthenticationAttempted AuthenticationAttemptedEvent {..}) =
        ("type" .= ("authentication_attempted" :: Text))
          <> ("user_name" .= userName)
          <> ("result" .= result)
          <> ("provider" .= provider)
          <> ("provider_data" .= providerData)

instance FromJSON FlowCallbackEventV1Envelope where
  parseJSON = withObject "FlowCallbackEventV1Envelope" $ \o -> do
    event <- o .: "type" >>= getTypeSpecifics o
    FlowCallbackEventV1Envelope
      <$> (o .: "event_created")
      <*> (o .: "flow_instance_id")
      <*> pure event
    where
      getTypeSpecifics :: Object -> Text -> Parser FlowCallbackEventV1
      getTypeSpecifics _ "completed" = pure Completed
      getTypeSpecifics _ "failed"    = pure Failed
      getTypeSpecifics o "flow_rejected" =
        Rejected
          <$> (   RejectedEvent
              <$> (o .: "user_name")
              <*> (o .: "document_name")
              <*> (o .: "message")
              )
      getTypeSpecifics o "authenication_attempted" =
        AuthenticationAttempted
          <$> (   AuthenticationAttemptedEvent
              <$> (o .: "user_name")
              <*> (o .: "result")
              <*> (o .: "provider")
              <*> (o .: "provider_data")
              )
      getTypeSpecifics _ type' = fail $ "Unknown callback event type: " <> unpack type'

data FlowCallbackEventV1
    = Completed
    | Failed
    | Rejected RejectedEvent
    | AuthenticationAttempted AuthenticationAttemptedEvent
  deriving (Eq, Generic, Show)

data RejectedEvent = RejectedEvent
  { userName :: UserName
  , mDocumentName :: Maybe DocumentName
  , mRejectMessage ::  Maybe Text
  }
  deriving (Eq, Generic, Show)

data AuthenticationAttemptedEvent = AuthenticationAttemptedEvent
  { userName :: UserName
  , result :: AuthenticationAttemptedResult
  , provider :: AuthProvider
  , providerData :: AuthenticationProviderData
  } deriving (Eq, Generic, Show)

data AuthenticationAttemptedResult = Success | Failure deriving (Eq, Generic, Show)

data AuthenticationProviderData = OnfidoProviderData_ OnfidoProviderData
  deriving (Eq, Generic, Show)

data OnfidoProviderData = OnfidoProviderData { applicantId :: Text}
  deriving (Eq, Generic, Show)

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }

instance ToJSON AuthenticationAttemptedResult where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON AuthenticationAttemptedResult where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthenticationProviderData where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON AuthenticationProviderData where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON OnfidoProviderData where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON OnfidoProviderData where
  parseJSON = genericParseJSON aesonOptions
