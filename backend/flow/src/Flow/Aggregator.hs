{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
module Flow.Aggregator
    ( AggregatorState(..)
    , AggregatorError(..)
    , AggregatorStep(..)
    , aggregate
    , aggregateAndStep
    , getAllowedEvents
    , makeNewState
    , prepareAggregatorState
    , runAggregatorStep
    )
  where

import Control.Arrow (left)
import Control.Exception (Exception)
import Data.Set (Set)
import GHC.Generics
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Flow.Machinize
import Flow.Transducer

data AggregatorState = AggregatorState
    { receivedEvents :: Set EventInfo
    , currentState :: StateId
    }
  deriving (Show, Eq, Generic)

newtype AggregatorConstans = AggregatorConstans
    { machine :: Machine
    }
  deriving (Show, Eq, Generic)

newtype AggregatorMarshallingException = AggregatorMarshallingException String
  deriving Show
  deriving anyclass Exception

data AggregatorStep
    = NeedMoreEvents
    | StateChange [LowAction]
    | FinalState [LowAction]
  deriving (Eq, Show, Generic)

data AggregatorError
    = UnknownEventInfo
    | DuplicateEvent
    | TransducerError Flow.Transducer.Error
  deriving (Eq, Show, Generic)

type AggregatorT = E.ExceptT AggregatorError (R.ReaderT Machine (S.State AggregatorState))

put :: AggregatorState -> AggregatorT ()
put = S.put

-- | Our state machine is labeled by a set of events;
-- more precisely, it requires that several events be received
-- before moving to another state (for example that several people sign a document).
--
-- The purpose of this function is to validate the incoming event
-- (possibly reporting it as unknown or duplicate),
-- add it to the set of received events, and finally
-- decide whether any edge can now fire (it is checked
-- that only one edge can fire when constructing the state machine).
--
-- If a firing edge is found we return Just the set of events to fire that edge
-- (this will be a subset of all received events in general),
-- otherwise return Nothing.
aggregate :: EventInfo -> AggregatorT (Maybe (Set EventInfo))
aggregate event = do
  state@AggregatorState {..} <- S.get
  machine <- R.ask
  machineState <- E.liftEither . left TransducerError $ getState machine currentState
  when (Set.notMember event $ getAllowedEvents machineState)
    $ E.throwError UnknownEventInfo
  -- TODO: Think hard about various scenarios where events may be duplicate.
  -- For example typos can lead to multiple events of the same type.
  when (Set.member event receivedEvents) $ E.throwError DuplicateEvent
  let newReceivedEvents = Set.insert event receivedEvents
  put $ state { receivedEvents = newReceivedEvents }
  pure . List.find (`Set.isSubsetOf` newReceivedEvents) $ getEdgeInputs machineState

-- | This functions combines `aggregate` (see above) and `findEdge` (see `Flow.Transducer`).
-- This means that it tries combine the new `event` with the ones received previously
-- and then find an edge that should fire upon receiving the combined set of events.
--
-- If such an edge is found we follow it which gives us
-- a new state to move to and an "output label" (transducer terminology), which we use
-- to trigger actions (e.g. sending notifications).
--
-- Current state is updated accordingly and we return the output label, as well as whether
-- a final state was reached.
aggregateAndStep :: EventInfo -> AggregatorT AggregatorStep
aggregateAndStep event = do
  AggregatorState {..}    <- S.get
  machine@Transducer {..} <- R.ask
  maybeEdgeInput          <- aggregate event
  case maybeEdgeInput of
    Nothing        -> pure NeedMoreEvents
    Just edgeInput -> do
      TransducerEdge {..} <- either (E.throwError . TransducerError) pure
        $ findEdge machine currentState edgeInput
      put $ makeNewState edgeNextState
      pure $ if isFinalState machine edgeNextState
        then FinalState edgeOutputLabel
        else StateChange edgeOutputLabel

makeNewState :: StateId -> AggregatorState
makeNewState newState =
  AggregatorState { currentState = newState, receivedEvents = mempty }

prepareAggregatorState :: Machine -> AggregatorState
prepareAggregatorState Transducer {..} = makeNewState initialState

getAllowedEvents :: State -> Set EventInfo
getAllowedEvents TransducerState {..} = Set.unions $ Map.keys stateEdges

getEdgeInputs :: State -> [Set EventInfo]
getEdgeInputs TransducerState {..} = Map.keys stateEdges

runAggregatorStep
  :: EventInfo
  -> AggregatorState
  -> Machine
  -> (Either AggregatorError AggregatorStep, AggregatorState)
runAggregatorStep event aggregatorState machine = do
  flip S.runState aggregatorState
    . flip R.runReaderT machine
    . E.runExceptT
    $ aggregateAndStep event
