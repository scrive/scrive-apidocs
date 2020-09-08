{-# LANGUAGE TemplateHaskell #-}
module Flow.Routes.Api
    ( CreateTemplate(..)
    , GetCreateTemplate(..)
    , GetTemplate(..)
    , PatchTemplate(..)
    , CreateInstance(..)
    , InstanceEventAction(..)
    , InstanceEvent(..)
    , InstanceState(..)
    , GetInstance(..)
    , InstanceAuthorAction(..)
    , InstanceUserAction(..)
    , GetInstanceView(..)
    , InstanceUserState(..)
    , InstanceUserDocument(..)
    , DocumentState(..)
    , FlowApi
    , Status(..)
    , InstanceApi
    , TemplateApi
    , apiProxy
    )
  where

import Data.Aeson
import Data.Aeson.Casing
import Data.Map (Map)
import Data.Proxy
import Data.Time.Clock
import GHC.Generics
import Optics.TH
import Servant.API

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
import Flow.HighTongue
import Flow.Id
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Process
import Flow.Routes.Types
import Folder.Types

-- brittany-disable-next-binding
type TemplateApi
  = -- Configuration
         ReqBody '[JSON] CreateTemplate :> PostCreated '[JSON] GetCreateTemplate
    :<|> Capture "template_id" TemplateId :> DeleteNoContent '[JSON] NoContent
    :<|> Capture "template_id" TemplateId :> Get '[JSON] GetTemplate
    :<|> Capture "template_id" TemplateId :> ReqBody '[JSON] PatchTemplate
                     :> Patch '[JSON] GetTemplate
    :<|> Get '[JSON] [GetTemplate]
    -- Control
    :<|> Capture "template_id" TemplateId :> "commit"
                     :> PostNoContent '[JSON] NoContent
    :<|> Capture "template_id" TemplateId :> "start"
                     :> ReqBody '[JSON] CreateInstance
                     :> PostCreated '[JSON] GetInstance

-- brittany-disable-next-binding
type InstanceApi
  = -- Progress
         Capture "instance_id" InstanceId :> Get '[JSON] GetInstance
    :<|> Get '[JSON] [GetInstance]

-- brittany-disable-next-binding
type AllApis
  = AuthProtect "account" :>
      (    "templates" :> TemplateApi
      :<|> "instances" :> InstanceApi
      )
    :<|>
      AuthProtect "instance-user"
        :> "instances"
        :> Capture "instance_id" InstanceId
        :> "view"
        :> Header "Host" Host
        :> IsSecure
        :> Get '[JSON] GetInstanceView
    :<|>
    -- No authentication
      "templates" :> "validate" :> ReqBody '[JSON] Process :> PostNoContent '[JSON] NoContent
    :<|>
      "version" :> Get '[JSON] Version

type FlowApi = AddFlowPrefix AllApis

apiProxy :: Proxy FlowApi
apiProxy = Proxy

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


data CreateInstance = CreateInstance
    { title :: Maybe Text
    , templateParameters :: InstanceKeyValues
    , callback :: Maybe Callback
    }
  deriving (Eq, Generic, Show)

instance FromJSON CreateInstance where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON CreateInstance where
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
    , user :: FlowUserId
    , document :: DocumentID
    , timestamp :: Text -- TODO: do something about this text...
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceEvent where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceEvent where
  toEncoding = genericToEncoding aesonOptions


newtype InstanceState = InstanceState
    { availableActions :: [InstanceAuthorAction]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceState where
  toEncoding = genericToEncoding aesonOptions


data GetInstance = GetInstance
    { id :: InstanceId
    , templateId :: TemplateId
    , title :: Maybe Text
    , templateParameters :: InstanceKeyValues
    , state :: InstanceState
    , accessLinks :: Map UserName Url
    , status :: Status
    , started :: UTCTime
    , lastEvent :: UTCTime
    , callback :: Maybe Callback
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstance where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstance where
  toEncoding = genericToEncoding aesonOptions


data InstanceAuthorAction = InstanceAuthorAction
    { actionType :: InstanceEventAction
    , actionUser :: FlowUserId
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
    , actionSignatoryId :: SignatoryLinkID
    , actionLink :: Url
    }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON InstanceUserAction where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON InstanceUserAction where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase


newtype InstanceUserState = InstanceUserState
    { documents :: [InstanceUserDocument]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InstanceUserState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceUserState where
  toEncoding = genericToEncoding aesonOptions


data InstanceUserDocument = InstanceUserDocument
    { documentId    :: DocumentID
    , documentState :: DocumentState
    , signatoryId   :: SignatoryLinkID
    }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON InstanceUserDocument where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceUserDocument where
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


data Status
    = InProgress
    | Completed
    | Failed
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Status where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }


data GetInstanceView = GetInstanceView
    { id      :: InstanceId
    , title :: Maybe Text
    , state   :: InstanceUserState
    , actions :: [InstanceUserAction]
    , status :: Status
    , started :: UTCTime
    , lastEvent :: UTCTime
    }
  deriving (Eq, Generic, Show)

instance FromJSON GetInstanceView where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON GetInstanceView where
  toEncoding = genericToEncoding aesonOptions


-- TODO: Add all optics...
makeFieldLabelsWith noPrefixFieldLabels ''InstanceUserDocument
makeFieldLabelsWith noPrefixFieldLabels ''InstanceUserState
makeFieldLabelsWith noPrefixFieldLabels ''InstanceUserAction
makeFieldLabelsWith noPrefixFieldLabels ''GetInstance
makeFieldLabelsWith noPrefixFieldLabels ''GetInstanceView
