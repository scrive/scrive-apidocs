{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module Flow.AggregatorTest where

import Data.Either.Combinators
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base ((@?=), Assertion)
import qualified Data.Set as Set

import Flow.Aggregator
import Flow.HighTongue
import Flow.Machinize
import Flow.Transducer

machine' :: Machine
machine' = fromRight' $ createTransducer
  "initial"
  ["end"]
  [ ("initial", TransducerEdge inputSet1 [lowAction1] "foo")
  , ("initial", TransducerEdge inputSet2 [lowAction2] "bar")
  , ("foo"    , TransducerEdge inputSet3 [lowAction3] "end")
  , ("bar"    , TransducerEdge inputSet3 [] "end")
  ]

lowAction1 :: LowAction
lowAction1 = Action $ Notify ["user25"] "foo bar baz"

lowAction2 :: LowAction
lowAction2 = Action $ Notify ["user26"] "foo bar baz"

lowAction3 :: LowAction
lowAction3 = Action $ Notify ["user27"] "foo bar baz"

inputSet1 :: Set.Set EventInfo
inputSet1 = Set.fromList
  [ EventInfo Signature "user1" "document1"
  , EventInfo Signature "user2" "document1"
  , EventInfo Approval  "user2" "document2"
  ]

inputSet2 :: Set.Set EventInfo
inputSet2 = Set.fromList
  [EventInfo Signature "user1" "document3", EventInfo Signature "user2" "document3"]

inputSet3 :: Set.Set EventInfo
inputSet3 = Set.fromList [EventInfo Signature "user1" "document1"]

input1 :: EventInfo
input1 = EventInfo Signature "user1" "document1"

input2 :: EventInfo
input2 = EventInfo Signature "user2" "document1"

input3 :: EventInfo
input3 = EventInfo Approval "user2" "document2"

input4 :: EventInfo
input4 = EventInfo (Field "Im on seafood diet") "I see food" " and I eat it"

input5 :: EventInfo
input5 = EventInfo Signature "user2" "document3"

initialAggregatorState :: AggregatorState
initialAggregatorState = prepareAggregatorState machine'

testUnknownEvent :: Assertion
testUnknownEvent =
  runAggregatorStep input4 initialAggregatorState machine'
    @?= (Left UnknownEventInfo, initialAggregatorState)

testNeedMoreEventsEvent1 :: Assertion
testNeedMoreEventsEvent1 = do
  let (result, AggregatorState {..}) =
        runAggregatorStep input1 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input1]

testNeedMoreEventsEvent2 :: Assertion
testNeedMoreEventsEvent2 = do
  let (result, AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]

testFullTransition :: Assertion
testFullTransition = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input1 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input1]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) =
        runAggregatorStep input2 nextState1 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input1, input2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep input3 nextState2 machine'
  receivedEvents @?= Set.empty
  currentState @?= "foo"
  result @?= Right (StateChange [lowAction1])

testFullTransitionDifferentOrder :: Assertion
testFullTransitionDifferentOrder = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) =
        runAggregatorStep input3 nextState1 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input3, input2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) =
        runAggregatorStep input1 nextState2 machine'
  receivedEvents @?= Set.empty
  currentState @?= "foo"
  result @?= Right (StateChange [lowAction1])

testFullTransitionInterleavedEvents :: Assertion
testFullTransitionInterleavedEvents = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) =
        runAggregatorStep input5 nextState1 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input5]
  -- Event 3
  let (result, nextState3@AggregatorState {..}) =
        runAggregatorStep input3 nextState2 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input3, input5]
  -- Event 4
  let (result, _nextState4@AggregatorState {..}) =
        runAggregatorStep input1 nextState3 machine'
  receivedEvents @?= Set.empty
  currentState @?= "foo"
  result @?= Right (StateChange [lowAction1])

testFullTransitionMultipleStates :: Assertion
testFullTransitionMultipleStates = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) =
        runAggregatorStep input5 nextState1 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input5]
  -- Event 3
  let (result, nextState3@AggregatorState {..}) =
        runAggregatorStep input3 nextState2 machine'
  result @?= Right NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input3, input5]
  -- Event 4
  let (result, nextState4@AggregatorState {..}) =
        runAggregatorStep input1 nextState3 machine'
  receivedEvents @?= Set.empty
  currentState @?= "foo"
  result @?= Right (StateChange [lowAction1])
  let (result, _nextState5@AggregatorState {..}) =
        runAggregatorStep input1 nextState4 machine'
  receivedEvents @?= Set.empty
  currentState @?= "end"
  result @?= Right (FinalState [lowAction3])

tests :: Test
tests = testGroup
  "Aggregator"
  [ testCase "Test rejection of unwanted event" testUnknownEvent
  , testCase "Test need more events 1"          testNeedMoreEventsEvent1
  , testCase "Test need more events 2"          testNeedMoreEventsEvent2
  , testCase "Test full transition"             testFullTransition
  , testCase "Test full transition with different order" testFullTransitionDifferentOrder
  , testCase "Test full transition with interleaved events"
             testFullTransitionInterleavedEvents
  , testCase "Test correct transion over multiple states" testFullTransitionMultipleStates
  ]


