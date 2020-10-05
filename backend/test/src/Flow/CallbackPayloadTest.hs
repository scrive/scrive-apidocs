{-# OPTIONS_GHC -Wno-orphans #-}
module Flow.CallbackPayloadTest (tests) where

import Data.Aeson hiding (Success)
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck hiding (Failure, Success)

import Flow.CallbackPayload
import Flow.Id
import Flow.Names
import Flow.OrphanTestInstances ()

tests :: Test
tests = testGroup
  "Flow callback payload"
  [ testCase "serialization completed"                  testCompleted
  , testCase "serialization failed"                     testFailed
  , testCase "serialization rejected"                   testRejected
  , testCase "serialization rejected empty"             testRejectedEmpty
  , testCase "serialization authentication onfido success" testAuthenticationOnfidoSuccess
  , testCase "serialization authentication onfido failure" testAuthenticationOnfidoFailure
  , testCase "serialization authentication sms failure" testAuthenticationSmsPinFailure
  , testProperty "serialization deserialization match"
                 testSerializationDeserializationMatch
  ]

envelope :: FlowCallbackEventV1 -> FlowCallbackEventV1Envelope
envelope event = FlowCallbackEventV1Envelope
  { flowInstanceId = Id $ fromWords 0 0 0 0
  , eventCreated   = UTCTime (fromGregorian 2020 10 10) $ secondsToDiffTime 10
  , event          = event
  }

testCompleted :: Assertion
testCompleted = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"completed\"}"
    data' = envelope Completed

testFailed :: Assertion
testFailed = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"failed\"}"
    data' = envelope Failed

testRejected :: Assertion
testRejected = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"flow_rejected\",\"user_name\":\"user\",\"message\":\"nice message\",\"document_name\":\"doc\"}"
    data' = envelope . Rejected $ RejectedEvent (unsafeName "user")
                                                (Just $ unsafeName "doc")
                                                (Just "nice message")

testRejectedEmpty :: Assertion
testRejectedEmpty = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"flow_rejected\",\"user_name\":\"user\"}"
    data' = envelope . Rejected $ RejectedEvent (unsafeName "user") Nothing Nothing

testAuthenticationOnfidoSuccess :: Assertion
testAuthenticationOnfidoSuccess = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"authentication_attempted\",\"user_name\":\"user\",\"result\":\"success\",\"provider\":\"onfido\",\"provider_data\":{\"applicant_id\":\"nice-id\"}}"
    data' =
      envelope
        . AuthenticationAttempted
        . AuthenticationAttemptedEvent (unsafeName "user") Success
        . Onfido
        $ AuthenticationProviderOnfido "nice-id"

testAuthenticationOnfidoFailure :: Assertion
testAuthenticationOnfidoFailure = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"authentication_attempted\",\"user_name\":\"user\",\"result\":\"failure\",\"provider\":\"onfido\",\"provider_data\":{\"applicant_id\":\"nice-id\"}}"
    data' =
      envelope
        . AuthenticationAttempted
        . AuthenticationAttemptedEvent (unsafeName "user") Failure
        . Onfido
        $ AuthenticationProviderOnfido "nice-id"

testAuthenticationSmsPinFailure :: Assertion
testAuthenticationSmsPinFailure = encode data' @?= expected
  where
    expected
      = "{\"version\":1,\"flow_instance_id\":\"00000000-0000-0000-0000-000000000000\",\"event_created\":\"2020-10-10T00:00:10Z\",\"type\":\"authentication_attempted\",\"user_name\":\"user\",\"result\":\"failure\",\"provider\":\"sms_pin\"}"
    data' =
      envelope
        . AuthenticationAttempted
        . AuthenticationAttemptedEvent (unsafeName "user") Failure
        $ SmsOtp

testSerializationDeserializationMatch :: FlowCallbackEventV1Envelope -> Bool
testSerializationDeserializationMatch event = Just event == decode (encode event)

instance Arbitrary FlowCallbackEventV1Envelope where
  arbitrary =
    FlowCallbackEventV1Envelope
      <$> pure (UTCTime (fromGregorian 2020 10 10) $ secondsToDiffTime 10)
      <*> (   (\w1 w2 w3 w4 -> Id $ fromWords w1 w2 w3 w4)
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          )
      <*> arbitrary

instance Arbitrary FlowCallbackEventV1 where
  arbitrary = oneof
    [ pure Completed
    , pure Failed
    , Rejected <$> arbitrary
    , AuthenticationAttempted <$> arbitrary
    ]

fewUserNames :: Gen UserName
fewUserNames = elements $ fmap unsafeName ["1729", "name", "kwa-1"]

fewMessages :: Gen (Maybe Text)
fewMessages = elements
  [Just "messaage1", Just "message withe line break\n and some other part", Nothing]

fewDocuments :: Gen (Maybe DocumentName)
fewDocuments = elements
  $ fmap (fmap unsafeName) [Just "doc", Just "doc with space", Just "doc-12", Nothing]

instance Arbitrary RejectedEvent where
  arbitrary = RejectedEvent <$> fewUserNames <*> fewDocuments <*> fewMessages

instance Arbitrary AuthenticationAttemptedEvent where
  arbitrary =
    AuthenticationAttemptedEvent
      <$> fewUserNames
      <*> chooseEnum (minBound, maxBound)
      <*> arbitrary

fewApplicantIds :: Gen Text
fewApplicantIds = elements ["1729-asdf-xsdr", "", "very nice applicatn id"]

instance Arbitrary AuthenticationProvider where
  arbitrary =
    oneof [Onfido . AuthenticationProviderOnfido <$> fewApplicantIds, pure SmsOtp]
