{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Flow.Machinize
    ( Deed(..)
    , EventInfo(..)
    , LowAction(..)
    , Machine
    , State
    , Edge
    , linear
    )
  where

import Data.Aeson hiding (pairs)
import Data.Aeson.Casing
import Data.List.Extra (snoc)
import Data.Set (Set)
import GHC.Generics
import qualified Data.Set as Set

import Flow.HighTongue
import Flow.Transducer

aesonOptions :: Options
aesonOptions = aesonPrefix snakeCase

-- | Types of actions performed by the user.
data Deed
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

instance FromJSON Deed where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Deed where
  toJSON = genericToJSON aesonOptions

-- | A record of an event relevant to a Flow process.
-- What happened, who did it, and which document was involved?
data EventInfo = EventInfo
    { eventInfoDeed     :: Deed
    , eventInfoUser     :: UserName
    , eventInfoDocument :: DocumentName
    }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON EventInfo where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON EventInfo where
  toJSON = genericToJSON aesonOptions

-- | Abstract actions triggered by the engine when transitioning
-- between states.
data LowAction
  -- | An action specified in the DSL by the user, e.g. `Notify`.
  = Action Action
  -- | An action representing a failure of the Flow process.
  | Fail
  deriving (Eq, Show, Generic)

instance FromJSON LowAction where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON LowAction where
  toJSON = genericToJSON aesonOptions

-- | A state machine reacting to user events and triggering
-- corresponding actions. Transitions are guarded by sets of events
-- -- it only fires when all the required events arrive.
type Machine = Transducer (Set EventInfo) [LowAction]
type State = TransducerState (Set EventInfo) [LowAction]
type Edge = TransducerEdge (Set EventInfo) [LowAction]

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

-- | Create an `Edge` from the given stage to the next one, while
-- aggregating all the events to wait for.
mkSuccessEdge :: (Stage, StateId) -> (StateId, Edge)
mkSuccessEdge (stage, next) = (stateId, edge)
  where
    stateId = stageName stage
    events  = Set.fromList $ Set.toList (stageExpect stage) >>= expectToSuccess
    edge    = TransducerEdge events (map Action $ stageActions stage) next

-- | Translate `Expect`, a description of expected user actions,
-- into a collection of events to match against, assuming all
-- actions fail.
expectToFailure :: Expect -> [EventInfo]
expectToFailure = \case
  ReceivedData {..} -> []
  ApprovedBy {..}   -> EventInfo Rejection <$> expectUsers <*> expectDocuments
  ViewedBy {..}     -> []
  SignedBy {..}     -> EventInfo Rejection <$> expectUsers <*> expectDocuments

-- | Create `Edge`s from the given stage to the Failure state, one per
-- every negative event (rejection of approval or signing).
mkFailureEdges :: StateId -> [LowAction] -> Stage -> [(StateId, Edge)]
mkFailureEdges failureStateId failureLowActions stage = map mkEdge
  $ concatMap expectToFailure expects
  where
    stateId = stageName stage
    expects = Set.toList $ stageExpect stage
    mkEdge e =
      (stateId, TransducerEdge (Set.singleton e) failureLowActions failureStateId)

-- TODO add end state
-- | Compile the high level description of the Flow process into a state machine.
--
-- Each stage corresponds to a state and the flow is linear from one state to the next,
-- conditioned of seing the expected events.
--
-- We also generate some internal states.
-- State `final` is where the process goes to after the final steps specified by the user.
-- State `failure` represents the failure of the process. We generate edges to it for every
-- possible negative user action, e.g. when they refuse to approve or sign a document.
linear :: HighTongue -> Either Error Machine
linear HighTongue {..} = createTransducer initialState ["final"] edges
  where
    edges = map mkSuccessEdge pairs <> concatMap (mkFailureEdges "failure" [Fail]) stages
    initialState = stageName $ head stages
    nextStates   = snoc (map stageName $ tail stages) "final"
    pairs        = zip stages nextStates
