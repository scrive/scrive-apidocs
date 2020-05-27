module FlowTests where

import Test.Framework

import TestKontra
import qualified Flow.AggregatorTest as Aggregator
import qualified Flow.IntegrationTest as Integration
import qualified Flow.MachinizeTest as Machinize
import qualified Flow.TransducerTest as Transducer
import qualified Flow.VariableCollectorTest as VariableCollector

flowTests :: TestEnvSt -> Test
flowTests env = testGroup
  "Flow"
  [ Aggregator.tests
  , Integration.tests env
  , Machinize.tests
  , VariableCollector.tests
  , Transducer.tests
  ]
