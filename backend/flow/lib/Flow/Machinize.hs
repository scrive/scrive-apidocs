{-# LANGUAGE DeriveAnyClass #-}
module Flow.Machinize
    ( EventInfo(..)
    , LowAction(..)
    , UserAction(..)
    , ExpectEvent(..)
    , EventDetails(..)
    , RejectionDetails(..)
    , expectToSuccess
    , expectToFailure
    , toExpectEvent
    , getRejectMessageFromEvent
    )
  where

import Control.Monad.Catch
import Data.Aeson hiding (pairs)
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics

import Flow.HighTongue
import Flow.Names

aesonOptions :: Options
aesonOptions = aesonPrefix snakeCase

-- | Types of actions performed by the user.
data UserAction
  -- | User provided data for a field `FieldName`.
  -- Currently not used.
  = Field FieldName
  -- | User approved a document.
  | Approval
  -- | User signed a document.
  | Signature
  -- | User viewed a document.
  | View
  -- | User rejected a document (used in both signing and approval actions).
  | DocumentRejection
  -- | User rejected an entire flow instance
  | FlowRejection
  -- | User had not provided a response within the time limit.
  -- Currently not used.
  | Timeout
  deriving (Eq, Ord, Show, Generic)

instance FromJSON UserAction where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON UserAction where
  toJSON = genericToJSON aesonOptions

instance PQFormat UserAction where
  pqFormat = pqFormat @Text

instance FromSQL UserAction where
  type PQBase UserAction = PQBase Text
  fromSQL mbase = fromSQL mbase >>= (either throwM pure . decodeUserAction)

instance ToSQL UserAction where
  type PQDest UserAction = PQDest Text
  toSQL = toSQL . encodeUserAction

encodeUserAction :: UserAction -> Text
encodeUserAction = \case
  -- TODO support fields
  Field _           -> "field"
  Approval          -> "approval"
  Signature         -> "signature"
  View              -> "view"
  DocumentRejection -> "rejection"
  FlowRejection     -> "flow_rejection"
  Timeout           -> "timeout"

newtype UnknownUserAction = UnknownUserAction Text
  deriving Show
  deriving anyclass Exception

decodeUserAction :: Text -> Either UnknownUserAction UserAction
decodeUserAction = \case
  "field"          -> Right . Field $ unsafeName ""
  "approval"       -> Right Approval
  "signature"      -> Right Signature
  "view"           -> Right View
  "rejection"      -> Right DocumentRejection
  "flow_rejection" -> Right FlowRejection
  "timeout"        -> Right Timeout
  x                -> Left $ UnknownUserAction x

-- | A record of an event relevant to a Flow process.
-- What happened, who did it, and which document was involved?
data EventInfo = EventInfo
    { eventInfoAction     :: UserAction
    , eventInfoUser     :: UserName
    , eventInfoDocument :: Maybe DocumentName
    , eventInfoDetails :: Maybe EventDetails
    }
  deriving (Eq, Show, Generic)

instance ToJSON EventInfo where
  toEncoding = genericToEncoding aesonOptions

newtype RejectionDetails = RejectionDetails
  { rejectMessage :: Text }
  deriving (Eq, Show, Generic)

-- Possible variants of details when inserting a flow event
-- into database. We currently do not validate which user action
-- should match with which event details.
newtype EventDetails
  = RejectionEventDetails RejectionDetails
  -- More types to be added here
  deriving (Eq, Show, Generic)

getRejectMessageFromEvent :: EventInfo -> Maybe Text
getRejectMessageFromEvent EventInfo { eventInfoDetails } = case eventInfoDetails of
  (Just (RejectionEventDetails (RejectionDetails message))) -> Just message
  _ -> Nothing

instance FromJSON RejectionDetails where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON RejectionDetails where
  toEncoding = genericToEncoding aesonOptions

instance FromJSON EventDetails where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON EventDetails where
  toEncoding = genericToEncoding aesonOptions

-- | Abstract actions triggered by the engine when transitioning
-- between states.
data LowAction
  -- | An action specified in the DSL by the user, e.g. `Notify`.
  = Action SystemAction
  -- | An action representing a failure of the Flow process.
  | Fail
  -- | A reject action is triggered when either a document or a flow instance
  --   is rejected. This results in all pending documents accessible by the
  --   user being rejected.
  | Reject
  -- | An action representing the closing of 0 or more documents.
  | CloseAll
  deriving (Eq, Show, Generic)

instance FromJSON LowAction where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON LowAction where
  toJSON = genericToJSON aesonOptions

data ExpectEvent = ExpectEvent
  { expectAction     :: UserAction
  , expectUser       :: UserName
  , expectDocument   :: Maybe DocumentName
  }
  deriving (Eq, Ord, Show, Generic)

toExpectEvent :: EventInfo -> ExpectEvent
toExpectEvent EventInfo { eventInfoAction, eventInfoUser, eventInfoDocument } =
  ExpectEvent eventInfoAction eventInfoUser eventInfoDocument

-- | Translate `Expect`, a description of expected user actions,
-- into collection of events to match against, assuming all
-- actions are performed successfully.
expectToSuccess :: Expect -> [ExpectEvent]
expectToSuccess = \case
  ReceivedData { expectFields, expectUsers, expectDocuments } ->
    ExpectEvent
      <$> (Field <$> expectFields)
      <*> expectUsers
      <*> (Just <$> expectDocuments)

  ApprovedBy { expectUsers, expectDocuments } ->
    ExpectEvent Approval <$> expectUsers <*> (Just <$> expectDocuments)

  ViewedBy { expectUsers, expectDocuments } ->
    ExpectEvent View <$> expectUsers <*> (Just <$> expectDocuments)

  SignedBy { expectUsers, expectDocuments } ->
    ExpectEvent Signature <$> expectUsers <*> (Just <$> expectDocuments)

-- | Translate `Expect`, a description of expected user actions,
-- into a list of `Rejection` events, one for each possible failure.
expectToFailure :: Expect -> [ExpectEvent]
expectToFailure = \case
  ReceivedData{} -> []

  -- If a user can approve a document, they can also reject the document
  -- or the entire flow instance
  ApprovedBy { expectUsers, expectDocuments } ->
    ExpectEvent DocumentRejection <$> expectUsers <*> (Just <$> expectDocuments)

  ViewedBy{} -> []

  -- If a user can sign a document, they can also reject the document
  -- or the entire flow instance
  SignedBy { expectUsers, expectDocuments } ->
    ExpectEvent DocumentRejection <$> expectUsers <*> (Just <$> expectDocuments)
