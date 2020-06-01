{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Flow.Api
    ( MessageId
    , FlowDSL
    , CreateTemplate(..)
    , GetCreateTemplate(..)
    , GetTemplate(..)
    , PatchTemplate(..)
    , ValidationError(..)
    , InstanceToTemplateMapping(..)
    , InstanceEventDeed(..)
    , InstanceEvent(..)
    , InstanceStage(..)
    , InstanceState(..)
    , GetInstance(..)
    , InstanceAction(..)
    , GetInstanceView(..)
    , StartTemplate(..)
    , FlowAPI
    , apiProxy
    )
  where

import Data.Aeson
import Data.Aeson.Casing
import Data.Map
import Data.Proxy
import Data.Time.Clock
import Data.Word
import GHC.Generics
import Servant.API

import Doc.DocumentID (DocumentID)
import Flow.Id
import User.UserID (UserID)

-- TODO: What to do with MessageId and messages in general?
type MessageId = Text

type FlowDSL = Text

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data CreateTemplate = CreateTemplate
    { name :: Text
    , process :: Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON CreateTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateTemplate where
  toEncoding = genericToEncoding aesonOptions

newtype GetCreateTemplate = GetCreateTemplate { id :: TemplateId }
  deriving (Eq, Generic, Show)

instance FromJSON GetCreateTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetCreateTemplate where
  toEncoding = genericToEncoding aesonOptions


newtype StartTemplate = StartTemplate { id :: InstanceId }
  deriving (Eq, Generic, Show)

instance FromJSON StartTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON StartTemplate where
  toEncoding = genericToEncoding aesonOptions


data GetTemplate = GetTemplate
    { id :: TemplateId
    , name :: Text
    , process :: Text
    , committed :: Maybe UTCTime
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetTemplate where
  toEncoding = genericToEncoding aesonOptions


data PatchTemplate = PatchTemplate
    { name :: Maybe Text
    , process :: Maybe Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON PatchTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON PatchTemplate where
  toEncoding = genericToEncoding aesonOptions


data ValidationError = ValidationError
    { line_number :: Word32
    , column :: Word32
    , error_message :: Text
    }
  deriving (Eq, Generic, Show)

instance FromJSON ValidationError where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON ValidationError where
  toEncoding = genericToEncoding aesonOptions


data InstanceToTemplateMapping = InstanceToTemplateMapping
    { documents :: Map Text DocumentID
    , users :: Map Text UserID
    , messages :: Map Text MessageId
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceToTemplateMapping where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceToTemplateMapping where
  toEncoding = genericToEncoding aesonOptions


data InstanceEventDeed
    = Approval
    | Signature
    | View
  deriving (Eq, Generic, Show)

instance FromJSON InstanceEventDeed where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON InstanceEventDeed where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

data InstanceEvent = InstanceEvent
    { deed :: InstanceEventDeed
    , user :: UserID
    , document :: DocumentID
    , timestamp :: Text -- TODO: do something about this text...
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceEvent where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceEvent where
  toEncoding = genericToEncoding aesonOptions


data InstanceStage = InstanceStage
    { stage :: Text
    , events :: [InstanceEvent]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceStage where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceStage where
  toEncoding = genericToEncoding aesonOptions


data InstanceState = InstanceState
    { events :: [InstanceEvent]
    , history :: [InstanceStage]
    , current :: InstanceStage
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceState where
  toEncoding = genericToEncoding aesonOptions


data GetInstance = GetInstance
    { id :: InstanceId
    , template :: TemplateId
    , templateParameters :: InstanceToTemplateMapping
    , state :: InstanceState
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstance where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstance where
  toEncoding = genericToEncoding aesonOptions


-- Maybe there should be a timestamp as well?
-- Though the timestamp doesn't make sense in case of POST event.
data InstanceAction = InstanceAction
    { deed :: InstanceEventDeed
    , document :: DocumentID
    }
  deriving (Eq, Generic, Show)


instance FromJSON InstanceAction where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceAction where
  toEncoding = genericToEncoding aesonOptions

data GetInstanceView = GetInstanceView
    { id :: InstanceId
    , state :: InstanceState
    , actions :: [InstanceAction]
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstanceView where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstanceView where
  toEncoding = genericToEncoding aesonOptions


-- brittany-disable-next-binding
type FlowAPI
    = AuthProtect "oauth" :> "flow" :>
        -- Configuration
        ("templates" :> ReqBody '[JSON] CreateTemplate :> PostCreated '[JSON] GetCreateTemplate
        :<|> "templates" :> Capture "template_id" TemplateId :> DeleteNoContent '[JSON] NoContent
        :<|> "templates" :> Capture "template_id" TemplateId :> Get '[JSON] GetTemplate
        :<|> "templates" :> Capture "template_id" TemplateId :> ReqBody '[JSON] PatchTemplate :> Patch '[JSON] GetTemplate
        -- Control
        :<|> "templates" :> Capture "template_id" TemplateId :> "commit" :> PostNoContent '[JSON] NoContent
        :<|> "templates" :> Capture "template_id" TemplateId :> "start"
            :> ReqBody '[JSON] InstanceToTemplateMapping :> PostCreated '[JSON] StartTemplate
        :<|> "instances" :> Capture "instance_id" InstanceId :> Get '[JSON] GetInstance
        )
    :<|> "templates" :> "validate" :> ReqBody '[JSON] FlowDSL :> Post '[JSON] [ValidationError]
--    -- Progress
--    :<|> "instances" :> Capture "instance_id" InstanceId :> "view" :> Get '[JSON] GetInstanceView

apiProxy :: Proxy FlowAPI
apiProxy = Proxy
