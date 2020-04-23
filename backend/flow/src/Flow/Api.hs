{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Flow.Api where

import Data.Aeson
import Data.Aeson.Casing
import Data.Int
import Data.Map
import Data.Proxy
import Data.Time.Clock
import Data.UUID
import Data.Word
import GHC.Generics
import Database.PostgreSQL.PQTypes hiding (JSON)
import Servant.API


data IdKind
    = InstanceId
    | TemplateId
    | MessageId

newtype Id (a :: IdKind) = Id UUID
  deriving (Eq, Show, Generic)

instance FromHttpApiData (Id a) where
    parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
    toUrlPiece (Id a) = toUrlPiece a

instance FromJSON (Id a) where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON (Id a) where
    toEncoding = genericToEncoding aesonOptions

type InstanceId = Id 'InstanceId
type MessageId = Id 'MessageId
type TemplateId = Id 'TemplateId

type DocumentId = Int64
type UserGroupId = Int64
type UserId = Int64

-- TODO: fix this ids
instance FromSQL TemplateId where
  type PQBase TemplateId = PQBase UUID
  fromSQL mbase = Id <$> fromSQL mbase

instance ToSQL TemplateId where
  type PQDest TemplateId = PQDest UUID
  toSQL (Id id) = toSQL id

instance PQFormat TemplateId where
  pqFormat = pqFormat @UUID

instance FromSQL MessageId where
  type PQBase MessageId = PQBase UUID
  fromSQL mbase = Id <$> fromSQL mbase

instance ToSQL MessageId where
  type PQDest MessageId = PQDest UUID
  toSQL (Id id) = toSQL id

instance PQFormat MessageId where
  pqFormat = pqFormat @UUID

type FlowDSL = Text

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data CreateTemplate = CreateTemplate
    { name :: Text
    , process :: Text
    , user :: Maybe UserId
    , userGroup :: Maybe UserGroupId
    }
  deriving (Eq, Generic, Show)

instance FromJSON CreateTemplate where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateTemplate where
    toEncoding = genericToEncoding aesonOptions


data GetCreateTemplate = GetCreateTemplate
    { id :: TemplateId
    , name :: Text
    , user :: UserId
    , userGroup :: UserGroupId
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetCreateTemplate where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON GetCreateTemplate where
    toEncoding = genericToEncoding aesonOptions


data GetTemplate = GetTemplate
    { id :: TemplateId
    , name :: Text
    , user :: UserId
    , userGroup :: UserGroupId
    , process :: Text
    , committed :: Maybe UTCTime
    , deleted :: Maybe UTCTime
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetTemplate where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON GetTemplate where
    toEncoding = genericToEncoding aesonOptions


data PatchTemplate = PatchTemplate
    { name :: Maybe Text
    , user :: Maybe UserId
    , userGroup :: Maybe UserGroupId
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
    { documents :: Map Text DocumentId
    , users :: Map Text UserId
    , messages :: MessageId
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
    parseJSON =
        genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON InstanceEventDeed where
    toEncoding =
        genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

data InstanceEvent = InstanceEvent
    { deed :: InstanceEventDeed
    , user :: UserId
    , document :: DocumentId
    , timestamp :: Text -- TODO: do something about this text...
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceEvent where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceEvent where
    toEncoding = genericToEncoding aesonOptions


data InstanceStage = InstanceStage
    { stage :: Text
    , events :: InstanceEvent
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceStage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceStage where
    toEncoding = genericToEncoding aesonOptions


data InstanceState = InstanceState
    { events :: InstanceEvent
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
    , document :: DocumentId
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


type FlowAPI
    -- Configuration
    = AuthProtect "oauth" :>
        ("template" :> ReqBody '[JSON] CreateTemplate :> PostCreated '[JSON] GetCreateTemplate
        :<|> "template" :> Capture "template_id" TemplateId :> Delete '[JSON] NoContent
        :<|> "template" :> Capture "template_id" TemplateId :> Get '[JSON] GetTemplate
        :<|> "template" :> Capture "template_id" TemplateId :> ReqBody '[JSON] PatchTemplate :> Patch '[JSON] GetTemplate
        )
--    :<|> "template" :> Capture "template_id" TemplateId :> "commit" :> Post '[JSON] NoContent
--    :<|> "template" :> "validate" :> ReqBody '[JSON] FlowDSL :> Post '[JSON] [ValidationError]
--    -- Control
--    :<|> "template" :> Capture "template_id" TemplateId :> "start"
--        :> ReqBody '[JSON] InstanceToTemplateMapping :> PostCreated '[JSON] GetInstance
--    :<|> "instance" :> Capture "instance_id" InstanceId :> Get '[JSON] GetInstance
--    -- Progress
--    :<|> "instance" :> Capture "instance_id" InstanceId :> "view" :> Get '[JSON] GetInstanceView
--    :<|> "instance" :> Capture "instance_id" InstanceId :> "event" :> Post '[JSON] InstanceAction

apiProxy :: Proxy FlowAPI
apiProxy = Proxy
