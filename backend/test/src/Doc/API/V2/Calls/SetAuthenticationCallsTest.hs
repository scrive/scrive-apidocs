module Doc.API.V2.Calls.SetAuthenticationCallsTest (apiV2SetAuthenticationTests) where

import Happstack.Server
import Test.Framework
import qualified Data.Text as T

import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Types.SignatoryLink
  ( AuthenticationToSignMethod(..), AuthenticationToViewMethod(..)
  , SignatoryRole(..)
  )
import TestingUtil
import TestKontra
import User.Lang (defaultLang)

apiV2SetAuthenticationTests :: TestEnvSt -> Test
apiV2SetAuthenticationTests env = testGroup
  "APIv2DocumentPostCalls"
  [ testThat "API v2 Set signatory authentication-to-view"
             env
             testDocApiV2SetSignatoryAuthenticationToView
  , testThat "API v2 Set signatory authentication-to-view-archived"
             env
             testDocApiV2SetSignatoryAuthenticationToViewArchived
  , testThat "API v2 Set signatory authentication-to-sign"
             env
             testDocApiV2SetSignatoryAuthenticationToSign
  , testThat "API v2 Set viewer authentication-to-view"
             env
             testDocApiV2SetViewerAuthenticationToView
  , testThat "API v2 Set viewer authentication-to-view-archived"
             env
             testDocApiV2SetViewerAuthenticationToViewArchived
  , testThat "API v2 Set viewer authentication-to-sign"
             env
             testDocApiV2SetViewerAuthenticationToSign
  , testThat "API v2 Set approver authentication-to-view"
             env
             testDocApiV2SetApproverAuthenticationToView
  , testThat "API v2 Set approver authentication-to-view-archived"
             env
             testDocApiV2SetApproverAuthenticationToViewArchived
  , testThat "API v2 Set approver authentication-to-sign"
             env
             testDocApiV2SetApproverAuthenticationToSign
  ]

testDocApiV2SetSignatoryAuthenticationToView :: TestEnv ()
testDocApiV2SetSignatoryAuthenticationToView = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 1 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack no_mobile <> "5")] 409

  let getAuthToView     = getMockDocSigLinkAuthToViewMethod 1
      getPersonalNumber = getMockDocSigLinkPersonalNumber 1
      getMobileNumber   = getMockDocSigLinkMobileNumber 1

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetSignatoryAuthenticationToViewArchived :: TestEnv ()
testDocApiV2SetSignatoryAuthenticationToViewArchived = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 1 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack no_mobile <> "5")] 409

  let getAuthToViewArchived = getMockDocSigLinkAuthToViewArchivedMethod 1
      getPersonalNumber     = getMockDocSigLinkPersonalNumber 1
      getMobileNumber       = getMockDocSigLinkMobileNumber 1

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetSignatoryAuthenticationToSign :: TestEnv ()
testDocApiV2SetSignatoryAuthenticationToSign = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 1 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      sms_pin       = "sms_pin"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10      = "1234567890"
      se_ssn_12      = "123456789012"
      se_ssn_invalid = "1234"
      param_mobile x = ("mobile_number", inText x)
      valid_mobile    = "+4612345678"
      _invalid_mobile = "45678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToSign did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToSign did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_auth "god_is_witness"]                    400
  setInvalidAuth [param_auth se_bankid, param_ssn se_ssn_invalid] 409
  -- FIXME this works, but is it supposed to?
  -- setInvalidAuth [param_auth sms_pin, param_mobile invalid_mobile] 409

  let getAuthToSign     = getMockDocSigLinkAuthToSignMethod 1
      getPersonalNumber = getMockDocSigLinkPersonalNumber 1
      getMobileNumber   = getMockDocSigLinkMobileNumber 1

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSEEmpty <- setAuth [param_auth se_bankid]
    assertEqual "Authentication to sign should be set"
                SEBankIDAuthenticationToSign
                (getAuthToSign mockDocSEEmpty)
    assertEqual "Personal number should not be set"
                ""
                (T.pack $ getPersonalNumber mockDocSEEmpty)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to sign should be set"
                SEBankIDAuthenticationToSign
                (getAuthToSign mockDocSE10)
    assertEqual "Personal number should be set (10 digit SE)"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to sign should be set"
                SEBankIDAuthenticationToSign
                (getAuthToSign mockDocSE12)
    assertEqual "Personal number should be set (12 digit SE)"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- mockDocTestRequestHelper
      ctx
      POST
      [param_auth standard_auth]
      (docApiV2SigSetAuthenticationToSign did slid)
      200
    assertEqual "Authentication to sign should be set"
                StandardAuthenticationToSign
                (getAuthToSign mockDocStandard1)
    assertEqual "Personal number should STILL be set (12 digit SE)"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocStandard1)

  -- Valid SMS PIN
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSMSEmpty <- setAuth [param_auth sms_pin]
    assertEqual "Authentication to sign should be set"
                SMSPinAuthenticationToSign
                (getAuthToSign mockDocSMSEmpty)
    assertEqual "Mobile number should not be set" "" (getMobileNumber mockDocSMSEmpty)
    assertEqual "Personal number should STILL be set (12 digit SE)"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSMSEmpty)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSMS <- setAuth [param_auth sms_pin, param_mobile valid_mobile]
    assertEqual "Authentication to sign should be set"
                SMSPinAuthenticationToSign
                (getAuthToSign mockDocSMS)
    assertEqual "Mobile number should be set"
                valid_mobile
                (T.pack $ getMobileNumber mockDocSMS)
    assertEqual "Personal number should STILL be set (12 digit SE)"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSMS)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to sign should be set"
                StandardAuthenticationToSign
                (getAuthToSign mockDocStandard2)
    assertEqual "Mobile number should STILL be set"
                valid_mobile
                (T.pack $ getMobileNumber mockDocStandard2)
    assertEqual "Personal number should STILL be set (12 digit SE)"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetViewerAuthenticationToView :: TestEnv ()
testDocApiV2SetViewerAuthenticationToView = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang

  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleViewer
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack no_mobile <> "5")] 409

  let getAuthToView     = getMockDocSigLinkAuthToViewMethod 2
      getPersonalNumber = getMockDocSigLinkPersonalNumber 2
      getMobileNumber   = getMockDocSigLinkMobileNumber 2

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetViewerAuthenticationToViewArchived :: TestEnv ()
testDocApiV2SetViewerAuthenticationToViewArchived = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleViewer
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack no_mobile <> "5")] 409

  let getAuthToViewArchived = getMockDocSigLinkAuthToViewArchivedMethod 2
      getPersonalNumber     = getMockDocSigLinkPersonalNumber 2
      getMobileNumber       = getMockDocSigLinkMobileNumber 2

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetViewerAuthenticationToSign :: TestEnv ()
testDocApiV2SetViewerAuthenticationToSign = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleViewer
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      se_bankid = "se_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10       = "1234567890"
      se_ssn_invalid  = "1234"
      _invalid_mobile = "45678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToSign did slid)
        expectedCode

  -- Some invalid requests
  setInvalidAuth [param_auth "god_is_witness"]                    400
  setInvalidAuth [param_auth se_bankid, param_ssn se_ssn_invalid] 409

  -- Viewers should not be allowed to set authentication to sign
  setInvalidAuth [param_auth se_bankid, param_ssn se_ssn_10]      409

testDocApiV2SetApproverAuthenticationToView :: TestEnv ()
testDocApiV2SetApproverAuthenticationToView = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleApprover
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did


  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToView did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ no_mobile <> "5")] 409

  let getAuthToView     = getMockDocSigLinkAuthToViewMethod 2
      getPersonalNumber = getMockDocSigLinkPersonalNumber 2
      getMobileNumber   = getMockDocSigLinkMobileNumber 2

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToView mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToView mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToView mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetApproverAuthenticationToViewArchived :: TestEnv ()
testDocApiV2SetApproverAuthenticationToViewArchived = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleApprover
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      standard_auth = "standard"
      se_bankid     = "se_bankid"
      no_bankid     = "no_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10 = "1234567890"
      se_ssn_12 = "123456789012"
      no_ssn    = "12345678901"
      param_mobile x = ("mobile_number", inText x)
      no_mobile = "+4712345678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        expectedCode
      setAuth params = mockDocTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToViewArchived did slid)
        200

  -- Some invalid requests
  setInvalidAuth [param_ssn se_ssn_10]                     400
  setInvalidAuth [param_auth "god_is_witness"]             400
  setInvalidAuth [param_auth no_bankid]                    409
  setInvalidAuth [param_auth no_bankid, param_ssn se_ssn_10] 409
  setInvalidAuth [param_auth se_bankid]                    409
  setInvalidAuth [param_auth se_bankid, param_ssn no_ssn]  409
  setInvalidAuth [param_auth no_bankid, param_mobile "-1"] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ init no_mobile)] 409
  setInvalidAuth [param_auth no_bankid, param_mobile (T.pack $ no_mobile <> "5")] 409

  let getAuthToViewArchived = getMockDocSigLinkAuthToViewArchivedMethod 2
      getPersonalNumber     = getMockDocSigLinkPersonalNumber 2
      getMobileNumber       = getMockDocSigLinkMobileNumber 2

  -- Valid SE BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE10 <- setAuth [param_auth se_bankid, param_ssn se_ssn_10]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE10)
    assertEqual "SE-10 Personal number should be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocSE10)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard1 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard1)
    assertEqual "SE-10 Personal number should STILL be set"
                se_ssn_10
                (T.pack $ getPersonalNumber mockDocStandard1)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocSE12 <- setAuth [param_auth se_bankid, param_ssn se_ssn_12]
    assertEqual "Authentication to view should be set"
                SEBankIDAuthenticationToView
                (getAuthToViewArchived mockDocSE12)
    assertEqual "SE-12 Personal number should be set"
                se_ssn_12
                (T.pack $ getPersonalNumber mockDocSE12)

  -- Valid NO BankID
  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNO <- setAuth [param_auth no_bankid, param_ssn no_ssn]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNO)
    assertEqual "NO Personal number should be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNO)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOMobile <- setAuth [param_auth no_bankid, param_mobile (T.pack no_mobile)]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOMobile)
    assertEqual "NO Mobile number should be set"
                no_mobile
                (getMobileNumber mockDocNOMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocNOEmptyMobile <- setAuth [param_auth no_bankid, param_mobile ""]
    assertEqual "Authentication to view should be set"
                NOBankIDAuthenticationToView
                (getAuthToViewArchived mockDocNOEmptyMobile)
    assertEqual "NO Mobile number should be empty"
                ""
                (getMobileNumber mockDocNOEmptyMobile)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocNOEmptyMobile)

  do -- Just to ensure limited scope so we don't test against the wrong thing
    mockDocStandard2 <- setAuth [param_auth standard_auth]
    assertEqual "Authentication to view should be set"
                StandardAuthenticationToView
                (getAuthToViewArchived mockDocStandard2)
    assertEqual "NO Personal number should STILL be set"
                no_ssn
                (T.pack $ getPersonalNumber mockDocStandard2)

testDocApiV2SetApproverAuthenticationToSign :: TestEnv ()
testDocApiV2SetApproverAuthenticationToSign = do
  user    <- instantiateRandomUser
  ctx     <- set #maybeUser (Just user) <$> mkContext defaultLang
  mockDoc <- do
    newMockDoc <- testDocApiV2New' ctx
    let did = getMockDocId newMockDoc
        newParty =
          setMockDocSigLinkSignatoryRole SignatoryRoleApprover
            $ setMockDocSigLinkDeliveryMethod "pad" defaultMockSigLink

    void $ testDocApiV2AddParties ctx [newParty] did
    testDocApiV2Start' ctx did

  let did  = getMockDocId mockDoc
      slid = getMockDocSigLinkId 2 mockDoc

  let param_auth x = ("authentication_type", inText x)
      se_bankid = "se_bankid"
      param_ssn x = ("personal_number", inText x)
      se_ssn_10       = "1234567890"
      se_ssn_invalid  = "1234"
      _invalid_mobile = "45678"
      setInvalidAuth params expectedCode = void $ jsonTestRequestHelper
        ctx
        POST
        params
        (docApiV2SigSetAuthenticationToSign did slid)
        expectedCode

  -- Some invalid requests
  setInvalidAuth [param_auth "god_is_witness"]                    400
  setInvalidAuth [param_auth se_bankid, param_ssn se_ssn_invalid] 409

  -- Viewers should not be allowed to set authentication to sign
  setInvalidAuth [param_auth se_bankid, param_ssn se_ssn_10]      409
