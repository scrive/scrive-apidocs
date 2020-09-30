module Flow.CallbackPayload
    ( FlowCallbackEventV1Envelope(..)
    , FlowCallbackEventV1(..)
    , RejectedEvent(..)
    , AuthenticationAttemptedResult(..)
    , AuthenticationAttemptedEvent(..)
    , AuthenticationProvider(..)
    , AuthenticationProviderOnfido(..)
    )
  where

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Text
import Data.Time
import GHC.Generics

import Flow.Core.Type.Callback
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
      eventPairs (Rejected RejectedEvent {..}) =
        ("type" .= ("flow_rejected" :: Text))
          <> ("user_name" .= userName)
          <> maybe mempty ("message" .=)       rejectMessage
          <> maybe mempty ("document_name" .=) documentName
      eventPairs (AuthenticationAttempted AuthenticationAttemptedEvent {..}) =
        "type"
          .= ("authentication_attempted" :: Text)
          <> "user_name"
          .= userName
          <> "result"
          .= result
          <> encodeProvider provider

      encodeProvider :: AuthenticationProvider -> Series
      encodeProvider (Onfido data') =
        "provider" .= ("onfido" :: Text) <> ("provider_data" `pair` toEncoding data')
      encodeProvider SmsPin = "provider" .= ("sms_pin" :: Text)


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
              <*> (o .:? "document_name")
              <*> (o .:? "message")
              )
      getTypeSpecifics o "authentication_attempted" =
        AuthenticationAttempted
          <$> (   AuthenticationAttemptedEvent
              <$> (o .: "user_name")
              <*> (o .: "result")
              <*> getProvider o
              )
      getTypeSpecifics _ type' = fail $ "Unknown callback event type: " <> unpack type'

      getProvider :: Object -> Parser AuthenticationProvider
      getProvider o = do
        (provider :: Text) <- o .: "provider"
        case provider of
          "onfido"  -> Onfido <$> o .: "provider_data"
          "sms_pin" -> pure SmsPin
          _         -> fail $ "Unknown AuthenticationProvider type: " <> unpack provider

data FlowCallbackEventV1
    = Completed
    | Failed
    | Rejected RejectedEvent
    | AuthenticationAttempted AuthenticationAttemptedEvent
  deriving (Eq, Generic, Show)

data RejectedEvent = RejectedEvent
  { userName :: UserName
  , documentName :: Maybe DocumentName
  , rejectMessage ::  Maybe Text
  }
  deriving (Eq, Generic, Show)

data AuthenticationAttemptedEvent = AuthenticationAttemptedEvent
  { userName :: UserName
  , result :: AuthenticationAttemptedResult
  , provider :: AuthenticationProvider
  } deriving (Eq, Generic, Show)

data AuthenticationAttemptedResult
  = Success
  | Failure
  deriving (Eq, Generic, Show, Enum, Bounded)

data AuthenticationProvider
  = Onfido AuthenticationProviderOnfido
  | SmsPin
  deriving (Eq, Generic, Show)

newtype AuthenticationProviderOnfido = AuthenticationProviderOnfido
  { applicantId :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON AuthenticationProviderOnfido where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON AuthenticationProviderOnfido where
  parseJSON = genericParseJSON aesonOptions

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier     = snakeCase
                              , constructorTagModifier = snakeCase
                              , omitNothingFields      = True
                              }

instance ToJSON AuthenticationAttemptedResult where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON AuthenticationAttemptedResult where
  parseJSON = genericParseJSON aesonOptions
