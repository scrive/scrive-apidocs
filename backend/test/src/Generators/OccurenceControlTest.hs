module Generators.OccurenceControlTest
  ( occurenceControlTests
  ) where

import Test.Framework

import Generators.OccurenceControl
import TestingUtil
import TestKontra

occurenceControlTests :: TestEnvSt -> Test
occurenceControlTests env = testGroup
  "Generators.OccurenceControl"
  [ testThat "Simple generator has an acceptable event occurence" env testSimpleGenerator
  , testThat "List generator has an acceptable event occurence"   env testListGenerator
  ]

testSimpleGenerator :: TestEnv ()
testSimpleGenerator = do
  -- Generate a triple of booleans representing whether the event has occured
  -- or not.
  let generator = (,,) <$> decide' pure <*> decide' pure <*> decide' pure
      checkOcc (x, y, z) = x || y || z
  testGeneratorHelper 0.1 generator checkOcc

testListGenerator :: TestEnv ()
testListGenerator = do
  -- Generate a list between 2 and 10 booleans which represent the event
  -- occurence.
  let generator = listOC 2 10 $ decide' pure
      checkOcc  = any identity
  testGeneratorHelper 0.1 generator checkOcc
