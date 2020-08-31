{-# LANGUAGE QuasiQuotes #-}
module Flow.HighTongueTest (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base (Assertion)
import Text.RawString.QQ

import Flow.HighTongue
import Flow.Process
import TestingUtil (assertLeft, assertRight)

tests :: Test
tests = testGroup
  "HighTongue"
  [ testCase "Parsing succeeds when dsl-version is supported" testSupportedDSLVersion
  , testCase "Parsing fails when dsl-version is missing"      testMissingDSLVersion
  , testCase "Parsing fails when dsl-version is unsupported"  testUnsupportedDSLVersion
  , testCase "Parsing fails when duplicate stage names"       testDuplicateStageNameErrors
  ]

testSupportedDSLVersion :: Assertion
testSupportedDSLVersion = assertRight $ decodeHighTongue process
  where
    -- TODO: use a version from HighTongue.supportedVersions in "process"
    process = unsafeProcess [r|
          dsl-version: "0.1.0"
          stages:
            - sign:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]

testUnsupportedDSLVersion :: Assertion
testUnsupportedDSLVersion = assertLeft $ decodeHighTongue process
  where
    process = unsafeProcess [r|
          dsl-version: "666"
          stages:
            - sign:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]

testMissingDSLVersion :: Assertion
testMissingDSLVersion = assertLeft $ decodeHighTongue process
  where
    process = unsafeProcess [r|
          stages:
            - sign:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]

testDuplicateStageNameErrors :: Assertion
testDuplicateStageNameErrors = assertLeft $ decodeHighTongue process
  where
    -- TODO: use a version from HighTongue.supportedVersions in "process"
    process = unsafeProcess [r|
          dsl-version: "0.1.0"
          stages:
            - dup:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
            - nondup:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
            - dup:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]
