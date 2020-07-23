{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
module Flow.Machinize
    ( EventInfo(..)
    , LowAction(..)
    , UserAction(..)
    , expectToSuccess
    , expectToFailure
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
  | Rejection
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
  Field _   -> "field"
  Approval  -> "approval"
  Signature -> "signature"
  View      -> "view"
  Rejection -> "rejection"
  Timeout   -> "timeout"

newtype UnknownUserAction = UnknownUserAction Text
  deriving Show
  deriving anyclass Exception

decodeUserAction :: Text -> Either UnknownUserAction UserAction
decodeUserAction = \case
  "field"     -> Right . Field $ unsafeName ""
  "approval"  -> Right Approval
  "signature" -> Right Signature
  "view"      -> Right View
  "rejection" -> Right Rejection
  "timeout"   -> Right Timeout
  x           -> Left $ UnknownUserAction x

-- | A record of an event relevant to a Flow process.
-- What happened, who did it, and which document was involved?
data EventInfo = EventInfo
    { eventInfoAction     :: UserAction
    , eventInfoUser     :: UserName
    , eventInfoDocument :: DocumentName
    }
  deriving (Eq, Ord, Show, Generic)

-- | Abstract actions triggered by the engine when transitioning
-- between states.
data LowAction
  -- | An action specified in the DSL by the user, e.g. `Notify`.
  = Action SystemAction
  -- | An action representing a failure of the Flow process.
  | Fail
  -- | An action representing the closing of 0 or more documents.
  | CloseAll
  deriving (Eq, Show, Generic)

instance FromJSON LowAction where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON LowAction where
  toJSON = genericToJSON aesonOptions

-- | Translate `Expect`, a description of expected user actions,
-- into collection of events to match against, assuming all
-- actions are performed successfully.
expectToSuccess :: Expect -> [EventInfo]
expectToSuccess = \case
  ReceivedData {..} ->
    EventInfo <$> (Field <$> expectFields) <*> expectUsers <*> expectDocuments
  ApprovedBy {..} -> EventInfo Approval <$> expectUsers <*> expectDocuments
  ViewedBy {..}   -> EventInfo View <$> expectUsers <*> expectDocuments
  SignedBy {..}   -> EventInfo Signature <$> expectUsers <*> expectDocuments

-- | Translate `Expect`, a description of expected user actions,
-- into a list of `Rejection` events, one for each possible failure.
expectToFailure :: Expect -> [EventInfo]
expectToFailure = \case
  ReceivedData {..} -> []
  ApprovedBy {..}   -> EventInfo Rejection <$> expectUsers <*> expectDocuments
  ViewedBy {..}     -> []
  SignedBy {..}     -> EventInfo Rejection <$> expectUsers <*> expectDocuments
