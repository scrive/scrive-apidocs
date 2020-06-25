{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Flow.Model.Types
    ( Template(Template)
    , InsertTemplate(InsertTemplate)
    , UpdateTemplate(UpdateTemplate)
    , Instance(Instance)
    , InsertInstance(InsertInstance)
    , Event(Event)
    , InsertEvent(InsertEvent)
    , FullInstance(..)
    , StoreValue(..)
    , StoreValueType(..)
    , fetchInstance
    , fetchTemplate
    , fetchEvent
    , storeValueTypeToText
    , toEventInfo
    , toInsertEvent
    , instanceToAggregator
    )
 where

import Data.Time.Clock
import GHC.Generics (Generic)
import qualified Data.Set as Set

import Doc.DocumentID (DocumentID)
import Flow.Aggregator
import Flow.Id
import Flow.Machinize
import Flow.Message
import Flow.Model.Types.Internal
import Flow.Names
import Flow.Process
import Folder.Types (FolderID)
import User.UserID (UserID)

data StoreValue
    = StoreDocumentId DocumentID
    | StoreUserId UserID
    | StoreEmail Text
    | StorePhoneNumber Text
    | StoreMessage Message
  deriving (Show, Eq, Generic)

data StoreValueType
    = Document
    | User
    | Email
    | PhoneNumber
    | Message


storeValueTypeToText :: StoreValueType -> Text
storeValueTypeToText Document    = "document"
storeValueTypeToText User        = "user"
storeValueTypeToText Email       = "email"
storeValueTypeToText PhoneNumber = "phone_number"
storeValueTypeToText Message     = "message"

-- TODO: Maybe use uncurryN functions?
fetchTemplate
  :: (TemplateId, UserID, FolderID, Text, Process, UTCTime, Maybe UTCTime, Maybe UTCTime)
  -> Template
fetchTemplate (id, userId, folderId, name, process, created, committed, deleted) =
  Template { .. }

fetchInstance :: (InstanceId, TemplateId, Text, UTCTime) -> Instance
fetchInstance (id, templateId, currentState, created) = Instance { .. }

fetchEvent :: (EventId, InstanceId, UserName, DocumentName, UserAction, UTCTime) -> Event
fetchEvent (id, instanceId, userName, documentName, userAction, created) = Event { .. }

toEventInfo :: Event -> EventInfo
toEventInfo Event {..} = EventInfo userAction userName documentName

toInsertEvent :: InstanceId -> EventInfo -> InsertEvent
toInsertEvent instanceId EventInfo {..} =
  InsertEvent instanceId eventInfoUser eventInfoDocument eventInfoAction

instanceToAggregator :: FullInstance -> AggregatorState
instanceToAggregator FullInstance {..} = aggregator
  where
    eventInfos = Set.fromList $ fmap toEventInfo aggregatorEvents
    aggregator = AggregatorState eventInfos $ flowInstance ^. #currentState
