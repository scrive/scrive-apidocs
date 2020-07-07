{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
module Flow.Model.Types.Internal
    ( Template(..)
    , InsertTemplate(..)
    , UpdateTemplate(..)
    , Instance(..)
    , InsertInstance(..)
    , Event(..)
    , InsertEvent(..)
    , FullInstance(..)
    , InstanceSession(..)
    , InstanceKeyValues(..)
    )
 where

import Data.Aeson
import Data.Aeson.Casing
import Data.Map (Map)
import Data.Time.Clock
import GHC.Generics (Generic)
import Optics.TH

import Auth.Session.SessionID
import Doc.DocumentID
import Flow.Id
import Flow.Machinize
import Flow.Message
import Flow.Model.Types.FlowUserId
import Flow.Names
import Flow.Process
import Folder.Types (FolderID)
import User.UserID (UserID)

data Template = Template
    { id :: TemplateId
    , userId :: UserID
    , folderId :: FolderID
    , name :: Text
    , process :: Process
    , created :: UTCTime
    , committed :: Maybe UTCTime
    , deleted :: Maybe UTCTime
    }

data InsertTemplate = InsertTemplate
    { name :: Text
    , process :: Process
    , userId :: UserID
    , folderId :: FolderID
    }
  deriving (Show, Eq, Generic)

data UpdateTemplate = UpdateTemplate
    { id :: TemplateId
    , name :: Maybe Text
    , process :: Maybe Process
    , committed :: Maybe UTCTime
    }
  deriving (Eq, Generic, Show)


data Instance = Instance
    { id :: InstanceId
    , templateId :: TemplateId
    -- TODO currentStage :: StageName
    , currentState :: Text
    , created :: UTCTime
    }

data InsertInstance = InsertInstance
    { templateId :: TemplateId
    -- TODO currentStage :: StageName
    , currentState :: Text
    }

data Event = Event
    { id :: EventId
    , instanceId :: InstanceId
    , userName :: UserName
    , documentName :: DocumentName
    , userAction :: UserAction
    , created :: UTCTime
    }

data InsertEvent = InsertEvent
    { instanceId :: InstanceId
    , userName :: UserName
    , documentName :: DocumentName
    , userAction :: UserAction
    }

-- TODO we should also add key/values here
data FullInstance = FullInstance
    { flowInstance :: Instance
    , template :: Template
    , aggregatorEvents :: [Event]
    }

data InstanceSession = InstanceSession
    { sessionId :: SessionID
    , instanceId :: InstanceId
    , userName :: UserName
    }

data InstanceKeyValues = InstanceKeyValues
  { documents :: Map DocumentName DocumentID
  , users     :: Map UserName FlowUserId
  , messages  :: Map MessageName Message
  } deriving (Eq, Generic, Show)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance FromJSON InstanceKeyValues where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON InstanceKeyValues where
  toEncoding = genericToEncoding aesonOptions



makeFieldLabelsWith noPrefixFieldLabels ''Template
makeFieldLabelsWith noPrefixFieldLabels ''InsertTemplate
makeFieldLabelsWith noPrefixFieldLabels ''UpdateTemplate
makeFieldLabelsWith noPrefixFieldLabels ''Instance
makeFieldLabelsWith noPrefixFieldLabels ''InsertInstance
makeFieldLabelsWith noPrefixFieldLabels ''Event
makeFieldLabelsWith noPrefixFieldLabels ''InsertEvent
makeFieldLabelsWith noPrefixFieldLabels ''FullInstance
makeFieldLabelsWith noPrefixFieldLabels ''InstanceSession
makeFieldLabelsWith noPrefixFieldLabels ''InstanceKeyValues
