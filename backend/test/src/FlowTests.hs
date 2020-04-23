module FlowTests where

import Test.Framework

import TestKontra

import qualified Flow.AggregatorTest as Aggregator

flowTests :: TestEnvSt -> Test
flowTests _env = testGroup "Flow"
    [ Aggregator.tests
    ]
