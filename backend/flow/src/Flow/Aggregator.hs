-- This module provides an implementation of the Flow DSL
-- as a simple, nearly-linear, state machine that handles incoming events
-- and reports errors, moves through the stages, and triggers actions accordingly.
--
-- Additional information: https://scriveab.atlassian.net/wiki/spaces/EN/pages/1573355521/Flow+DSL
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
module Flow.Aggregator
    ( AggregatorState(..)
    , AggregatorError(..)
    , AggregatorStep(..)
    , aggregateAndStep
    , runAggregatorStep
    , remainingStages
    , failureStageName
    , finalStageName
    , makeNewState
    )
  where

import Control.Exception (Exception)
import Data.Set (Set)
import GHC.Generics
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as S
import qualified Data.Set as Set

import Flow.HighTongue
import Flow.Machinize
import Flow.Names

data AggregatorState = AggregatorState
    { receivedEvents :: Set EventInfo
    , currentStage :: StageName
    }
  deriving (Show, Eq, Generic)

newtype AggregatorMarshallingException = AggregatorMarshallingException String
  deriving Show
  deriving anyclass Exception

data AggregatorStep
    = NeedMoreEvents
    | StateChange [LowAction]
  deriving (Eq, Show, Generic)

data AggregatorError
    = UnknownEventInfo
    | DuplicateEvent
    | UnknownStage
  deriving (Eq, Show, Generic)

type AggregatorT = E.ExceptT AggregatorError (S.State AggregatorState)

put :: AggregatorState -> AggregatorT ()
put = S.put

remainingStages :: StageName -> [Stage] -> Maybe (Stage, [Stage])
remainingStages name stages = case dropWhile (\Stage {..} -> stageName /= name) stages of
  []       -> Nothing
  (s : ss) -> Just (s, ss)

failureStageName :: StageName
failureStageName = unsafeName "__FAILURE__"

finalStageName :: StageName
finalStageName = unsafeName "__FINAL__"

-- | This function looks at the events that have been received and the current state of
-- the Aggregator. If the requirements have been met, then the Aggregator is stepped to
-- the next stage as defined in the DSL.
--
-- When the next stage is entered, its actions are triggered (e.g. sending notifications).
--
-- There are also events which can cause us to enter a failure state. In which case, the
-- flow will be terminated. Currently, these failure events are caused by rejections.
--
-- Unexpected events will not be processed but will not cause failure of the flow.
aggregateAndStep :: HighTongue -> EventInfo -> AggregatorT AggregatorStep
aggregateAndStep HighTongue {..} event = do
  AggregatorState {..} <- S.get
  (stage, nextStages)  <- maybe (E.throwError UnknownStage) pure
    $ remainingStages currentStage stages
  let successEvents = Set.fromList $ Set.toList (stageExpect stage) >>= expectToSuccess
      failureEvents = Set.fromList $ Set.toList (stageExpect stage) >>= expectToFailure
      knownEvents   = Set.union successEvents failureEvents

  when (Set.notMember event knownEvents) $ E.throwError UnknownEventInfo
  when (Set.member event receivedEvents) $ E.throwError DuplicateEvent

  let newReceivedEvents = Set.insert event receivedEvents

  if Set.member event failureEvents
    then do
      put $ makeNewState failureStageName
      pure $ StateChange [Fail]
    else do
      if successEvents == newReceivedEvents
        then case nextStages of
          [] -> do
            put $ makeNewState finalStageName
            pure $ StateChange [CloseAll]
          (Stage {..} : _) -> do
            put $ makeNewState stageName
            pure . StateChange $ fmap Action stageActions
        else do
          S.modify $ \s -> s { receivedEvents = newReceivedEvents }
          pure NeedMoreEvents

makeNewState :: StageName -> AggregatorState
makeNewState newState =
  AggregatorState { currentStage = newState, receivedEvents = mempty }

runAggregatorStep
  :: EventInfo
  -> AggregatorState
  -> HighTongue
  -> (Either AggregatorError AggregatorStep, AggregatorState)
runAggregatorStep event aggregatorState highTongue = do
  flip S.runState aggregatorState . E.runExceptT $ aggregateAndStep highTongue event
