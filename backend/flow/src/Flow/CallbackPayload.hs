module Flow.CallbackPayload
    ( FlowCallbackEventV1Envelope(..)
    , FlowCallbackEventV1(..)
    , RejectedEvent(..)
    )
  where

import Data.Aeson
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
      $  "version"
      .= V1
      <> "flow_instance_id"
      .= flowInstanceId
      <> "event_created"
      .= eventCreated
      <> eventPairs event
    where
      eventPairs :: FlowCallbackEventV1 -> Series
      eventPairs Completed = "type" .= ("completed" :: Text)
      eventPairs Failed    = "type" .= ("failed" :: Text)
      eventPairs (Rejected RejectedEvent { userName, message }) =
        "type"
          .= ("flow_rejected" :: Text)
          <> "user_name"
          .= userName
          <> "message"
          .= message

instance FromJSON FlowCallbackEventV1Envelope where
  parseJSON = withObject "FlowCallbackEventV1Envelope" $ \o -> do
    event <- o .: "type" >>= getTypeSpecifics o
    FlowCallbackEventV1Envelope
      <$> o
      .:  "event_created"
      <*> o
      .:  "flow_instance_id"
      <*> pure event
    where
      getTypeSpecifics :: Object -> Text -> Parser FlowCallbackEventV1
      getTypeSpecifics _ "completed" = pure Completed
      getTypeSpecifics _ "failed"    = pure Failed
      getTypeSpecifics o "flow_rejected" =
        Rejected <$> (RejectedEvent <$> o .: "user_name" <*> o .: "message")
      getTypeSpecifics _ type' = fail $ "Unknown callback event type: " <> unpack type'

data FlowCallbackEventV1
    = Completed
    | Failed
    | Rejected RejectedEvent
  deriving (Eq, Generic, Show)

data RejectedEvent = RejectedEvent
  { userName :: UserName
  , message ::  Text
  }
  deriving (Eq, Generic, Show)
