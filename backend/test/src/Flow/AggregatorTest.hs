{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

module Flow.AggregatorTest (tests) where

import Data.Either.Combinators
import Data.Text.Encoding
import Data.Yaml
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base hiding (Test)
import Text.RawString.QQ
import qualified Data.Set as Set

import Flow.Aggregator
import Flow.HighTongue
import Flow.Machinize
import Flow.OrphanTestInstances ()

process :: Text
process = [r|
  dsl-version: "0.1.0"
  stages:
    - initial:
        actions:
          - notify:
              users: [user1]
              message: get-data
        expect:
          approved-by:
            users: [approver1]
            documents: [doc1, doc2, doc3]
    - foo:
        actions:
          - notify:
              users: [user1, user2]
              message: get-data2
        expect:
          approved-by:
            users: [approver2]
            documents: [doc5]
  |]

tongue :: Either ParseException HighTongue
tongue = decodeEither' $ encodeUtf8 process

compiled :: HighTongue
compiled = fromRight' tongue

prepareAggregatorState :: HighTongue -> Maybe AggregatorState
prepareAggregatorState HighTongue {..} = makeNewState . stageName <$> listToMaybe stages

initialAggregatorState :: AggregatorState
initialAggregatorState = fromJust $ prepareAggregatorState compiled

testDSLCompiles :: Assertion
testDSLCompiles = do
  assertBool "DSL compiles" $ isRight tongue

testUnknownEvent :: Assertion
testUnknownEvent = do
  runAggregatorStep event initialAggregatorState compiled
    @?= (Left UnknownEventInfo, initialAggregatorState)
  where
    event :: EventInfo
    event = EventInfo Signature "who??" "what???"

testNeedMoreEventsEvent1 :: Assertion
testNeedMoreEventsEvent1 = do
  let (result, AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [event]
  where
    event :: EventInfo
    event = EventInfo Approval "approver1" "doc1"

testDuplicateEventEvent :: Assertion
testDuplicateEventEvent = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  receivedEvents @?= Set.fromList [event]
  -- Event 2
  let (result, _nextState2@AggregatorState {..}) =
        runAggregatorStep event nextState1 compiled
  receivedEvents @?= Set.fromList [event]
  result @?= Left DuplicateEvent
  where
    event :: EventInfo
    event = EventInfo Approval "approver1" "doc1"

testNeedMoreEventsEvent2 :: Assertion
testNeedMoreEventsEvent2 = do
  let (_, state) = runAggregatorStep event1 initialAggregatorState compiled
      (result, AggregatorState {..}) = runAggregatorStep event2 state compiled
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [event1, event2]
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" "doc1"
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" "doc2"

testFullTransition :: Assertion
testFullTransition = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event1 initialAggregatorState compiled
  receivedEvents @?= Set.fromList [event1]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event2 nextState1 compiled
  receivedEvents @?= Set.fromList [event1, event2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= Set.empty
--   currentState @?= "foo"
  result @?= Right (StateChange [action])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" "doc1"
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" "doc2"
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" "doc3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ "get-data2"

testFullTransitionDifferentOrder :: Assertion
testFullTransitionDifferentOrder = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event2 initialAggregatorState compiled
  receivedEvents @?= Set.fromList [event2]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event1 nextState1 compiled
  receivedEvents @?= Set.fromList [event1, event2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= Set.empty
  result @?= Right (StateChange [action])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" "doc1"
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" "doc2"
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" "doc3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ "get-data2"

testFullTransitionMultipleStates :: Assertion
testFullTransitionMultipleStates = do
  -- Stage 1
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event2 initialAggregatorState compiled
  receivedEvents @?= Set.fromList [event2]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event1 nextState1 compiled
  receivedEvents @?= Set.fromList [event1, event2]
  -- Event 3
  let (result, nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= Set.empty
  result @?= Right (StateChange [action])
  -- Stage 2
  -- Event 4
  let (result, _nextState4@AggregatorState {..}) =
        runAggregatorStep event4 nextState3 compiled
  receivedEvents @?= Set.empty
  result @?= Right (StateChange [CloseAll])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" "doc1"
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" "doc2"
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" "doc3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ "get-data2"
    event4 :: EventInfo
    event4 = EventInfo Approval "approver2" "doc5"

testFailure :: Assertion
testFailure = do
  let (result, nextState@AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  receivedEvents @?= Set.empty
  result @?= Right (StateChange [Fail])
  let (result, _nextState2@AggregatorState {..}) =
        runAggregatorStep event nextState compiled
  result @?= Left UnknownStage
  where
    event :: EventInfo
    event = EventInfo Rejection "approver1" "doc1"

testUnknownStage :: Assertion
testUnknownStage = do
  let aggState = AggregatorState { currentStage = "FUBAR", receivedEvents = mempty }
      (result, AggregatorState {..}) = runAggregatorStep event aggState compiled
  result @?= Left UnknownStage
  receivedEvents @?= mempty
  where
    event :: EventInfo
    event = EventInfo Approval "foo1" "bar1"

tests :: Test
tests = testGroup
  "Aggregator"
  [ testCase "Test DSL string compiles"         testDSLCompiles
  , testCase "Test rejection of unwanted event" testUnknownEvent
  , testCase "Test need more events 1"          testNeedMoreEventsEvent1
  , testCase "Test duplicate event"             testDuplicateEventEvent
  , testCase "Test need more events 2"          testNeedMoreEventsEvent2
  , testCase "Test full transition"             testFullTransition
  , testCase "Test full transition with different order" testFullTransitionDifferentOrder
  , testCase "Test correct transition over many states" testFullTransitionMultipleStates
  , testCase "Test unknown stage"               testUnknownStage
  , testCase "Test failure"                     testFailure
  ]
