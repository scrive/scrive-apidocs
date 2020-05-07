module FlowTests where

import Test.Framework

import TestKontra
import qualified Flow.AggregatorTest as Aggregator
import qualified Flow.IntegrationTest as Integration

flowTests :: TestEnvSt -> Test
flowTests env = testGroup "Flow" [Aggregator.tests, Integration.tests env]
