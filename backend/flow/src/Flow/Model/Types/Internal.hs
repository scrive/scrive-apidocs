{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
    )
 where

import Data.Time.Clock
import GHC.Generics (Generic)
import Optics.TH

import Auth.Session.SessionID
import Flow.Id
import Flow.Machinize
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
    , currentState :: Text
    , created :: UTCTime
    }

data InsertInstance = InsertInstance
    { templateId :: TemplateId
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

makeFieldLabelsWith noPrefixFieldLabels ''Template
makeFieldLabelsWith noPrefixFieldLabels ''InsertTemplate
makeFieldLabelsWith noPrefixFieldLabels ''UpdateTemplate
makeFieldLabelsWith noPrefixFieldLabels ''Instance
makeFieldLabelsWith noPrefixFieldLabels ''InsertInstance
makeFieldLabelsWith noPrefixFieldLabels ''Event
makeFieldLabelsWith noPrefixFieldLabels ''InsertEvent
makeFieldLabelsWith noPrefixFieldLabels ''FullInstance
makeFieldLabelsWith noPrefixFieldLabels ''InstanceSession
