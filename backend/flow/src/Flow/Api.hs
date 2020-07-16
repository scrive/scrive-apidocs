{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Flow.Api
    ( CreateTemplate(..)
    , GetCreateTemplate(..)
    , GetTemplate(..)
    , PatchTemplate(..)
    , InstanceToTemplateMapping(..)
    , InstanceEventAction(..)
    , InstanceEvent(..)
    , InstanceStage(..)
    , InstanceState(..)
    , GetInstance(..)
    , InstanceAuthorAction(..)
    , InstanceUserAction(..)
    , GetInstanceView(..)
    , StartTemplate(..)
    , InstanceUserState(..)
    , DocumentOverview(..)
    , DocumentState(..)
    , FlowAPI
    , apiProxy
    )
  where

import Data.Aeson
import Data.Aeson.Casing
import Data.Map
import Data.Proxy
import Data.Time.Clock
import GHC.Generics
import Servant.API

import Doc.DocumentID (DocumentID)
import Flow.HighTongue
import Flow.Id
import Flow.Message
import Flow.Process
import Folder.Types
import User.UserID (UserID)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data CreateTemplate = CreateTemplate
    { name :: Text
    , process :: Process
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
    , process :: Process
    , committed :: Maybe UTCTime
    , folderId :: FolderID
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetTemplate where
  toEncoding = genericToEncoding aesonOptions


data PatchTemplate = PatchTemplate
    { name :: Maybe Text
    , process :: Maybe Process
    }
  deriving (Eq, Generic, Show)

instance FromJSON PatchTemplate where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON PatchTemplate where
  toEncoding = genericToEncoding aesonOptions


-- TODO use name newtypes (UserName, etc.)
data InstanceToTemplateMapping = InstanceToTemplateMapping
    { documents :: Map Text DocumentID
    , users :: Map Text UserID
    , messages :: Map Text Message
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceToTemplateMapping where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceToTemplateMapping where
  toEncoding = genericToEncoding aesonOptions


data InstanceEventAction
    = Approve
    | Sign
    | View
    | Reject
  deriving (Eq, Ord, Generic, Show)

instance FromJSON InstanceEventAction where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON InstanceEventAction where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

data InstanceEvent = InstanceEvent
    { action :: InstanceEventAction
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
    { availableActions :: [InstanceAuthorAction]
    , history :: [InstanceStage]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceState where
  toEncoding = genericToEncoding aesonOptions


data GetInstance = GetInstance
    { id :: InstanceId
    , templateId :: TemplateId
    , templateParameters :: InstanceToTemplateMapping
    , state :: InstanceState
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstance where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstance where
  toEncoding = genericToEncoding aesonOptions


data InstanceAuthorAction = InstanceAuthorAction
    { actionType :: InstanceEventAction
    , actionUser :: UserID
    , actionDocument :: DocumentID
    }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON InstanceAuthorAction where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON InstanceAuthorAction where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase


-- Maybe there should be a timestamp as well?
-- Though the timestamp doesn't make sense in case of POST event.
data InstanceUserAction = InstanceUserAction
    { actionType :: InstanceEventAction
    , actionDocument :: DocumentID
    }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON InstanceUserAction where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON InstanceUserAction where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase


newtype InstanceUserState = InstanceUserState
    { documents :: [DocumentOverview]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceUserState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceUserState where
  toEncoding = genericToEncoding aesonOptions


data DocumentOverview = DocumentOverview
    { documentId    :: DocumentID
    , documentState :: DocumentState
    }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON DocumentOverview where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON DocumentOverview where
  toEncoding = genericToEncoding aesonOptions


data DocumentState
    = Started
    | Signed
    | Approved
    | Viewed
    | Rejected
  deriving (Eq, Generic, Ord, Show)

instance FromJSON DocumentState where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON DocumentState where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }


data GetInstanceView = GetInstanceView
    { id      :: InstanceId
    , state   :: InstanceUserState
    , actions :: [InstanceUserAction]
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstanceView where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstanceView where
  toEncoding = genericToEncoding aesonOptions


-- brittany-disable-next-binding
type FlowAPI
  = "experimental" :> "flow" :>
    ( AuthProtect "oauth-or-cookies" :>
        -- Configuration
        ("templates" :> ReqBody '[JSON] CreateTemplate :> PostCreated '[JSON] GetCreateTemplate
        :<|> "templates" :> Capture "template_id" TemplateId :> DeleteNoContent '[JSON] NoContent
        :<|> "templates" :> Capture "template_id" TemplateId :> Get '[JSON] GetTemplate
        :<|> "templates" :> Capture "template_id" TemplateId :> ReqBody '[JSON] PatchTemplate :> Patch '[JSON] GetTemplate
        :<|> "templates" :> Get '[JSON] [GetTemplate]
        -- Control
        :<|> "templates" :> Capture "template_id" TemplateId :> "commit" :> PostNoContent '[JSON] NoContent
        :<|> "templates" :> Capture "template_id" TemplateId :> "start"
            :> ReqBody '[JSON] InstanceToTemplateMapping :> PostCreated '[JSON] StartTemplate
        -- Progress
        :<|> "instances" :> Capture "instance_id" InstanceId :> Get '[JSON] GetInstance
        :<|> "instances" :> Capture "instance_id" InstanceId :> "view" :> Get '[JSON] GetInstanceView
        :<|> "instances" :> Get '[JSON] [GetInstance]
        )
    :<|> "templates" :> "validate" :> ReqBody '[JSON] Process :> Post '[JSON] [ValidationError]
    )

apiProxy :: Proxy FlowAPI
apiProxy = Proxy
