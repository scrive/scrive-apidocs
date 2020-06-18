{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}

module Flow.HighTongueTest where

import Data.Text.Encoding
import Data.Yaml
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base (Assertion)
import Text.RawString.QQ

import Flow.HighTongue
import TestingUtil (assertLeft, assertRight)

tests :: Test
tests = testGroup
  "HighTongue"
  [ testCase "Parsing succeeds when dsl-version is supported" testSupportedDSLVersion
  , testCase "Parsing fails when dsl-version is missing"      testMissingDSLVersion
  , testCase "Parsing fails when dsl-version is unsupported"  testUnsupportedDSLVersion
  ]

testSupportedDSLVersion :: Assertion
testSupportedDSLVersion = assertRight tongue
  where
    tongue = decodeEither' $ encodeUtf8 process :: Either ParseException HighTongue
    -- TODO: use a version from HighTongue.supportedVersions in "process"
    process = [r|
          dsl-version: "1"
          stages:
            - sign:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]

testUnsupportedDSLVersion :: Assertion
testUnsupportedDSLVersion = assertLeft tongue
  where
    tongue = decodeEither' $ encodeUtf8 process :: Either ParseException HighTongue
    process = [r|
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
testMissingDSLVersion = assertLeft tongue
  where
    tongue = decodeEither' $ encodeUtf8 process :: Either ParseException HighTongue
    process = [r|
          stages:
            - sign:
                actions: []
                expect:
                  signed-by:
                    users: [user1]
                    documents: [document1]
          |]
