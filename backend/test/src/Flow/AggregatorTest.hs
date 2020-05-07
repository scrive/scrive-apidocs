{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Flow.AggregatorTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base ((@?=), Assertion)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Flow.Aggregator
import Flow.Machinize
import Flow.Transducer

initialState' :: State
initialState' = TransducerState
  "initial"
  (Map.fromList
    [ (inputSet1, TransducerEdge inputSet1 [] "foo")
    , (inputSet2, TransducerEdge inputSet2 [] "bar")
    ]
  )

inputSet1 :: Set.Set EventInfo
inputSet1 = Set.fromList
  [ EventInfo Signature "user1" "document1"
  , EventInfo Signature "user2" "document1"
  , EventInfo Approval  "user2" "document2"
  ]

inputSet2 :: Set.Set EventInfo
inputSet2 = Set.fromList
  [EventInfo Signature "user1" "document3", EventInfo Signature "user2" "document3"]

machine' :: Machine
machine' = Transducer
  { _states       =
    Map.fromList
      [ ("initial", initialState')
      , ( "foo"
        , TransducerState
          "foo"
          (Map.fromList [(inputSet3, TransducerEdge inputSet3 [] "end")])
        )
      , ( "bar"
        , TransducerState
          "bar"
          (Map.fromList [(inputSet3, TransducerEdge inputSet3 [] "end")])
        )
      ]
  , _initialState = "initial"
  , _endStates    = ["end"]
  }
  where inputSet3 = Set.fromList [EventInfo Signature "user1" "document1"]

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
initialAggregatorState = prepareAggregatorState machine' initialState'

testUnknownEvent :: Assertion
testUnknownEvent =
  runAggregatorStep input4 initialAggregatorState
    @?= (Left UnknownEventInfo, initialAggregatorState)

testNeedMoreEventsEvent1 :: Assertion
testNeedMoreEventsEvent1 = do
  let (result, AggregatorState {..}) = runAggregatorStep input1 initialAggregatorState
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input1]

testNeedMoreEventsEvent2 :: Assertion
testNeedMoreEventsEvent2 = do
  let (result, AggregatorState {..}) = runAggregatorStep input2 initialAggregatorState
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]

testFullTransition :: Assertion
testFullTransition = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input1 initialAggregatorState
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input1]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) = runAggregatorStep input2 nextState1
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input1, input2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) = runAggregatorStep input3 nextState2
  result @?= Right (TransducerEdge inputSet1 [] "foo")

testFullTransitionDifferentOrder :: Assertion
testFullTransitionDifferentOrder = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) = runAggregatorStep input3 nextState1
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input3, input2]
  -- Event 3
  let (result, _nextState3@AggregatorState {..}) = runAggregatorStep input1 nextState2
  result @?= Right (TransducerEdge inputSet1 [] "foo")

testFullTransitionInterleavedEvents :: Assertion
testFullTransitionInterleavedEvents = do
    -- Event 1
  let (result, nextState1@AggregatorState {..}) =
        runAggregatorStep input2 initialAggregatorState
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input2]
  -- Event 2
  let (result, nextState2@AggregatorState {..}) = runAggregatorStep input5 nextState1
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input5]
  -- Event 3
  let (result, nextState3@AggregatorState {..}) = runAggregatorStep input3 nextState2
  result @?= Left NeedMoreEvents
  receivedEvents @?= Set.fromList [input2, input3, input5]
  -- Event 4
  let (result, _nextState4@AggregatorState {..}) = runAggregatorStep input1 nextState3
  result @?= Right (TransducerEdge inputSet1 [] "foo")

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
  ]


