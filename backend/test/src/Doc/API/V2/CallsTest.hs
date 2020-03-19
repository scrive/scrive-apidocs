module Doc.API.V2.CallsTest (apiV2CallsTests) where

import Test.Framework

import Doc.API.V2.Calls.DocumentGetCallsTest
import Doc.API.V2.Calls.DocumentPostCallsTest
import Doc.API.V2.Calls.SetAuthenticationCallsTest
import Doc.API.V2.Calls.SignatoryCallsTest
import TestKontra

apiV2CallsTests :: TestEnvSt -> Test
apiV2CallsTests env = testGroup
  "APIv2Calls"
  [ apiV2DocumentPostCallsTests env
  , apiV2DocumentGetCallsTests env
  , apiV2SetAuthenticationTests env
  , apiV2SignatoryCallsTests env
  ]
