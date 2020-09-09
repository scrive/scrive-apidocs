module FlowTests where

import Test.Framework

import TestKontra
import qualified Flow.AggregatorTest as Aggregator
import qualified Flow.AuthenticationTest as Authentication
import qualified Flow.DocumentCheckerTest as DocumentChecker
import qualified Flow.EIDTest as EID
import qualified Flow.HighTongueTest as HighTongue
import qualified Flow.IntegrationTest as Integration
import qualified Flow.VariableCollectorTest as VariableCollector

flowTests :: TestEnvSt -> Test
flowTests env = testGroup
  "Flow"
  [ Aggregator.tests
  , Authentication.tests env
  , DocumentChecker.tests
  , EID.tests env
  , HighTongue.tests
  , Integration.tests env
  , VariableCollector.tests
  ]
