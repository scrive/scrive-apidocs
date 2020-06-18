{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
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
    )
 where

import Data.Time.Clock
import GHC.Generics (Generic)
import Optics.TH

import Flow.Id
import Flow.Machinize
import Folder.Types (FolderID)
import User.UserID (UserID)

data Template = Template
    { id :: TemplateId
    , userId :: UserID
    , folderId :: FolderID
    , name :: Text
    , process :: Text
    , created :: UTCTime
    , committed :: Maybe UTCTime
    , deleted :: Maybe UTCTime
    }

data InsertTemplate = InsertTemplate
    { name :: Text
    , process :: Text
    , userId :: UserID
    , folderId :: FolderID
    }
  deriving (Show, Eq, Generic)

data UpdateTemplate = UpdateTemplate
    { id :: TemplateId
    , name :: Maybe Text
    , process :: Maybe Text
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
    , userName :: Text
    , documentName :: Text
    , userAction :: UserAction
    , created :: UTCTime
    }

data InsertEvent = InsertEvent
    { instanceId :: InstanceId
    , userName :: Text
    , documentName :: Text
    , userAction :: UserAction
    }

-- TODO we should also add key/values here
data FullInstance = FullInstance
    { flowInstance :: Instance
    , template :: Template
    , aggregatorEvents :: [Event]
    }


makeFieldLabelsWith noPrefixFieldLabels ''Template
makeFieldLabelsWith noPrefixFieldLabels ''InsertTemplate
makeFieldLabelsWith noPrefixFieldLabels ''UpdateTemplate
makeFieldLabelsWith noPrefixFieldLabels ''Instance
makeFieldLabelsWith noPrefixFieldLabels ''InsertInstance
makeFieldLabelsWith noPrefixFieldLabels ''Event
makeFieldLabelsWith noPrefixFieldLabels ''InsertEvent
makeFieldLabelsWith noPrefixFieldLabels ''FullInstance
