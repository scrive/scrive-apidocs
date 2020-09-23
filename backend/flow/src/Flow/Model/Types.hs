module Flow.Model.Types
    ( Role(..)
    , DocRoleFor(..)
    , InstanceKeyValues(InstanceKeyValues)
    , Template(Template)
    , InsertTemplate(InsertTemplate)
    , UpdateTemplate(UpdateTemplate)
    , Instance(Instance)
    , InsertInstance(..)
    , Event(Event)
    , InsertEvent(InsertEvent)
    , FullInstance(..)
    , InstanceSession(InstanceSession)
    , InstanceAccessToken(InstanceAccessToken)
    , UserAuthConfig(UserAuthConfig)
    , fetchInstance
    , fetchTemplate
    , fetchEvent
    , toEventInfo
    , toInsertEvent
    , instanceToAggregator
    , fetchInstanceSession
    , fetchInstanceAccessToken
    , fetchUserAuthConfig
    )
 where

import Data.Aeson
import Data.Aeson.Casing
import Data.Int
import Data.Time.Clock
import GHC.Generics
import qualified Data.Set as Set

import Auth.MagicHash
import Auth.Session.SessionID
import Flow.Aggregator
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
import Flow.EID.AuthConfig
import Flow.Id
import Flow.Machinize
import Flow.Model.Types.Internal
import Flow.Names
import Flow.Process
import Folder.Types (FolderID)
import User.UserID (UserID)

-- | A role a user can have.
data Role = Viewer | Approver | SigningParty
  deriving (Eq, Generic, Ord, Show)

instance ToJSON Role where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

-- | This type specifies which role a user has when acting
-- on a specific document.
--
-- Some fields are polymorphic so that we can use this type
-- with both abstract Flow variables as well as concrete IDs.
data DocRoleFor u d = DocRoleFor
  { role     :: Role
  , user     :: u
  , document :: d
  } deriving (Eq, Generic, Ord, Show)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance (ToJSON u, ToJSON d) => ToJSON (DocRoleFor u d) where
  toEncoding = genericToEncoding aesonOptions

-- TODO: Maybe use uncurryN functions?
fetchTemplate
  :: (TemplateId, UserID, FolderID, Text, Process, UTCTime, Maybe UTCTime, Maybe UTCTime)
  -> Template
fetchTemplate (id, userId, folderId, name, process, created, committed, deleted) =
  Template { .. }

fetchInstance
  :: ( InstanceId
     , TemplateId
     , Maybe Text
     , StageName
     , UTCTime
     , UTCTime
     , Maybe Url
     , Maybe CallbackVersion
     )
  -> Instance
fetchInstance (id, templateId, title, currentState, started, lastEvent, maybeCallbackUrl, maybeCallbackVersion)
  = let
      callback = case (maybeCallbackUrl, maybeCallbackVersion) of
        (Just url, Just version) -> Just Callback { .. }
        (Nothing , Nothing     ) -> Nothing
        _ ->
          unexpectedError "callback url and version have to be both Just or both Nothing"
    in  Instance { .. }

fetchEvent
  :: (EventId, InstanceId, UserName, Maybe DocumentName, UserAction, UTCTime) -> Event
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

fetchInstanceSession :: (SessionID, InstanceId, UserName) -> InstanceSession
fetchInstanceSession (sessionId, instanceId, userName) = InstanceSession { .. }

fetchInstanceAccessToken
  :: (InstanceAccessTokenId, InstanceId, UserName, MagicHash) -> InstanceAccessToken
fetchInstanceAccessToken (id, instanceId, userName, hash) = InstanceAccessToken { .. }

fetchUserAuthConfig
  :: ( InstanceId
     , UserName
     , Maybe AuthProvider
     , Maybe Int32
     , Maybe AuthProvider
     , Maybe Int32
     )
  -> UserAuthConfig
fetchUserAuthConfig (instanceId, userName, mViewProvider, mViewMaxFailures, mViewArchivedProvider, mViewArchivedMaxFailures)
  = UserAuthConfig
    { instanceId
    , userName
    , authToView         = mkAuthConfig mViewProvider mViewMaxFailures
    , authToViewArchived = mkAuthConfig mViewArchivedProvider mViewArchivedMaxFailures
    }
  where
    mkAuthConfig mProvider mMaxFailures = do
      provider    <- mProvider
      maxFailures <- fromIntegral <$> mMaxFailures
      pure $ AuthConfig { .. }
