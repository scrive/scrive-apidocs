{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}
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
  dsl-version: "0.2.0"
  stages:
    - initial:
        actions:
          - notify:
              users: [user1]
              methods:
                email: get-data
        expect:
          approved-by:
            users: [approver1]
            documents: [doc1, doc2, doc3]
    - foo:
        actions:
          - notify:
              users: [user1, user2]
              methods:
                email: get-data2
                sms: get-data3
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
    event = EventInfo Signature "who??" (Just "what???") Nothing

testNeedMoreEventsEvent1 :: Assertion
testNeedMoreEventsEvent1 = do
  let (result, AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  result @?= Right NeedMoreEvents
  receivedEvents @?= [event]
  where
    event :: EventInfo
    event = EventInfo Approval "approver1" (Just "doc1") Nothing

testDuplicateEventEvent :: Assertion
testDuplicateEventEvent = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  receivedEvents @?= [event]
  -- Event 2
  let (result, _nextState2@AggregatorState {..}) =
        runAggregatorStep event nextState1 compiled
  receivedEvents @?= [event]
  result @?= Left DuplicateEvent
  where
    event :: EventInfo
    event = EventInfo Approval "approver1" (Just "doc1") Nothing

testNeedMoreEventsEvent2 :: Assertion
testNeedMoreEventsEvent2 = do
  let (_, state) = runAggregatorStep event1 initialAggregatorState compiled
      (result, AggregatorState {..}) = runAggregatorStep event2 state compiled
  result @?= Right NeedMoreEvents
  Set.fromList (toExpectEvent <$> receivedEvents)
    @?= Set.fromList (toExpectEvent <$> [event1, event2])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" (Just "doc1") Nothing
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" (Just "doc2") Nothing

testFullTransition :: Assertion
testFullTransition = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event1 initialAggregatorState compiled
  receivedEvents @?= [event1]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event2 nextState1 compiled

  Set.fromList (toExpectEvent <$> receivedEvents)
    @?= Set.fromList (toExpectEvent <$> [event1, event2])

  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= []
--   currentState @?= "foo"
  result @?= Right (StateChange [action])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" (Just "doc1") Nothing
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" (Just "doc2") Nothing
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" (Just "doc3") Nothing
    emailMessage :: Maybe MessageName
    emailMessage = Just "get-data2"
    smsMessage :: Maybe MessageName
    smsMessage = Just "get-data3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ Methods emailMessage smsMessage

testFullTransitionDifferentOrder :: Assertion
testFullTransitionDifferentOrder = do
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event2 initialAggregatorState compiled
  receivedEvents @?= [event2]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event1 nextState1 compiled
  receivedEvents @?= [event1, event2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= []
  result @?= Right (StateChange [action])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" (Just "doc1") Nothing
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" (Just "doc2") Nothing
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" (Just "doc3") Nothing
    emailMessage :: Maybe MessageName
    emailMessage = Just "get-data2"
    smsMessage :: Maybe MessageName
    smsMessage = Just "get-data3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ Methods emailMessage smsMessage

testFullTransitionMultipleStates :: Assertion
testFullTransitionMultipleStates = do
  -- Stage 1
  -- Event 1
  let (_, nextState1@AggregatorState {..}) =
        runAggregatorStep event2 initialAggregatorState compiled
  receivedEvents @?= [event2]
  -- Event 2
  let (_, nextState2@AggregatorState {..}) = runAggregatorStep event1 nextState1 compiled
  receivedEvents @?= [event1, event2]
  -- Event 3
  let (result, nextState3@AggregatorState {..}) =
        runAggregatorStep event3 nextState2 compiled
  receivedEvents @?= []
  result @?= Right (StateChange [action])
  -- Stage 2
  -- Event 4
  let (result, _nextState4@AggregatorState {..}) =
        runAggregatorStep event4 nextState3 compiled
  receivedEvents @?= []
  result @?= Right (StateChange [CloseAll])
  where
    event1 :: EventInfo
    event1 = EventInfo Approval "approver1" (Just "doc1") Nothing
    event2 :: EventInfo
    event2 = EventInfo Approval "approver1" (Just "doc2") Nothing
    event3 :: EventInfo
    event3 = EventInfo Approval "approver1" (Just "doc3") Nothing
    emailMessage :: Maybe MessageName
    emailMessage = Just "get-data2"
    smsMessage :: Maybe MessageName
    smsMessage = Just "get-data3"
    action :: LowAction
    action = Action . Notify ["user1", "user2"] $ Methods emailMessage smsMessage
    event4 :: EventInfo
    event4 = EventInfo Approval "approver2" (Just "doc5") Nothing

testFailure :: Assertion
testFailure = do
  let (result, nextState@AggregatorState {..}) =
        runAggregatorStep event initialAggregatorState compiled
  receivedEvents @?= []
  result @?= Right (StateChange [Reject])
  let (result, _nextState2@AggregatorState {..}) =
        runAggregatorStep event nextState compiled
  result @?= Left UnknownStage
  where
    event :: EventInfo
    event = EventInfo DocumentRejection "approver1" (Just "doc1") Nothing

testUnknownStage :: Assertion
testUnknownStage = do
  let aggState = AggregatorState { currentStage = "FUBAR", receivedEvents = mempty }
      (result, AggregatorState {..}) = runAggregatorStep event aggState compiled
  result @?= Left UnknownStage
  receivedEvents @?= mempty
  where
    event :: EventInfo
    event = EventInfo Approval "foo1" (Just "bar1") Nothing

tests :: Test
tests = testGroup
  "Flow Aggregator"
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
