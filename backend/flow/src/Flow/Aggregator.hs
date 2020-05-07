{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Flow.Aggregator where

import Data.Set (Set)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as S
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Flow.Machinize
import Flow.Transducer

data AggregatorState = AggregatorState
    { machine :: Machine
    , stateId :: StateId
    , receivedEvents :: Set EventInfo
    , allowedEvents :: Set EventInfo
    , edgeInputs :: [Set EventInfo]
    }
  deriving (Show, Eq)

data AggregatorResult
    = NeedMoreEvents
    | UnknownEventInfo
    | DuplicateEvent
    | TransducerError Flow.Transducer.Error
  deriving (Eq, Show)

type AggregatorT = E.ExceptT AggregatorResult (S.State AggregatorState)

aggregate :: EventInfo -> AggregatorT (Set EventInfo)
aggregate event = do
  state@AggregatorState {..} <- S.get
  when (Set.notMember event allowedEvents) $ E.throwError UnknownEventInfo
  -- TODO: Think hard about various scenarios where events may be duplicate.
  -- For example typos can lead to multiple events of the same type.
  when (Set.member event receivedEvents) $ E.throwError DuplicateEvent
  let newReceivedEvents = Set.insert event receivedEvents
  S.put $ state { receivedEvents = newReceivedEvents }
  maybe (E.throwError NeedMoreEvents) pure
    $ List.find (`Set.isSubsetOf` newReceivedEvents) edgeInputs

aggregateAndStep :: EventInfo -> AggregatorT Edge
aggregateAndStep event = do
  AggregatorState {..} <- S.get
  edgeInput            <- aggregate event
  either (E.throwError . TransducerError) pure $ step machine stateId edgeInput

prepareAggregatorState :: Machine -> State -> AggregatorState
prepareAggregatorState machine TransducerState {..} = AggregatorState
  { machine
  , stateId        = _stateId
  , receivedEvents = Set.empty
  , allowedEvents  = foldl Set.union Set.empty $ Map.keys _stateEdges
  , edgeInputs     = Map.keys _stateEdges
  }

runAggregatorStep
  :: EventInfo -> AggregatorState -> (Either AggregatorResult Edge, AggregatorState)
runAggregatorStep event aggregatorState =
  flip S.runState aggregatorState . E.runExceptT $ aggregateAndStep event
