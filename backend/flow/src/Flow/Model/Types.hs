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
    , EventDetails(..)
    , UserAuthenticationConfiguration(..)
    , UserAuthenticationConfigurationData(..)
    , fetchInstance
    , fetchTemplate
    , fetchEvent
    , toEventInfo
    , toInsertEvent
    , instanceToAggregator
    , fetchInstanceSession
    , fetchInstanceAccessToken
    , fetchUserAuthConfig
    , getAuthenticationKindAndConfiguration
    )
 where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString (ByteString)
import Data.Either.Extra
import Data.Text
import Data.Time.Clock
import Database.PostgreSQL.PQTypes
import GHC.Generics

import Auth.MagicHash
import Auth.Session.SessionID
import Doc.Types.SignatoryLink
import Flow.Aggregator
import Flow.Core.Type.AuthenticationConfiguration
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
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
  :: ( EventId
     , InstanceId
     , UserName
     , Maybe DocumentName
     , UserAction
     , UTCTime
     , Maybe (JSONB Value)
     )
  -> Event
fetchEvent (id, instanceId, userName, documentName, userAction, created, detailsJson) =
  Event { .. }
  where
    eventDetails :: Maybe EventDetails
    eventDetails = do
      parsed <- mParsed
      case parsed of
        Success details -> return details
        Error   _       -> Nothing

    mParsed :: Maybe (Result EventDetails)
    mParsed = fromJSON @EventDetails . unJSONB <$> detailsJson

toEventInfo :: Event -> EventInfo
toEventInfo Event { userAction, userName, documentName, eventDetails } =
  EventInfo userAction userName documentName eventDetails

toInsertEvent :: InstanceId -> EventInfo -> InsertEvent
toInsertEvent instanceId EventInfo { eventInfoUser, eventInfoDocument, eventInfoAction, eventInfoDetails }
  = InsertEvent instanceId
                eventInfoUser
                eventInfoDocument
                eventInfoAction
                eventInfoDetails

instanceToAggregator :: FullInstance -> AggregatorState
instanceToAggregator FullInstance {..} = aggregator
  where
    eventInfos :: [EventInfo]
    eventInfos = fmap toEventInfo aggregatorEvents

    aggregator :: AggregatorState
    aggregator = AggregatorState eventInfos $ flowInstance ^. #currentState

fetchInstanceSession :: (SessionID, InstanceId, UserName) -> InstanceSession
fetchInstanceSession (sessionId, instanceId, userName) = InstanceSession { .. }

fetchInstanceAccessToken
  :: (InstanceAccessTokenId, InstanceId, UserName, MagicHash) -> InstanceAccessToken
fetchInstanceAccessToken (id, instanceId, userName, hash) = InstanceAccessToken { .. }

fetchUserAuthConfig
  :: (InstanceId, UserName, JSONB ByteString) -> UserAuthenticationConfiguration
fetchUserAuthConfig (instanceId, userName, configurationDataByteString) =
  UserAuthenticationConfiguration
    { instanceId
    , userName
    , configurationData =
      fromEither
      . left
          (\e ->
            unexpectedError
              $  "Can't decode UserAuthenticationConfigurationData: "
              <> pack e
          )
      . eitherDecodeStrict'
      $ unJSONB configurationDataByteString
    }

-- TODO: Add a type alias or a record instead of passing this tuple around.
getAuthenticationKindAndConfiguration
  :: FullInstance
  -> UserAuthenticationConfiguration
  -> Maybe (AuthenticationKind, AuthenticationConfiguration)
getAuthenticationKindAndConfiguration fullInstance uac =
  if isComplete $ instanceToAggregator fullInstance
    then confData ^. #authenticationToViewArchived >>= \conf ->
      pure (AuthenticationToViewArchived, conf)
    else confData ^. #authenticationToView >>= \conf -> pure (AuthenticationToView, conf)
  where confData = uac ^. #configurationData

