module Doc.API.V2.Calls.CallsTestUtils
  ( testDocApiV2New'
  , testDocApiV2Start'
  , testDocApiV2StartNew
  , testDocApiV2Get'
  , testDocApiV2Update'
  , testDocApiV2AddParties
  ) where

import Happstack.Server

import Context
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Mock.TestUtils
import Doc.DocumentID (DocumentID)
import Doc.Types.DocumentStatus (DocumentStatus(..))
import TestingUtil
import TestKontra

testDocApiV2New' :: Context -> TestEnv MockDoc
testDocApiV2New' ctx = mockDocTestRequestHelper
  ctx
  POST
  [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  docApiV2New
  201

testDocApiV2Start' :: Context -> DocumentID -> TestEnv MockDoc
testDocApiV2Start' ctx documentId = do
  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start documentId) 200
  assertEqual "Document status should match after 'start' call"
              Pending
              (getMockDocStatus mockDoc)
  return mockDoc

testDocApiV2StartNew :: Context -> TestEnv MockDoc
testDocApiV2StartNew ctx = do
  mockDoc <- testDocApiV2New' ctx
  testDocApiV2Start' ctx (getMockDocId mockDoc)

testDocApiV2Get' :: Context -> DocumentID -> TestEnv MockDoc
testDocApiV2Get' ctx documentId =
  mockDocTestRequestHelper ctx GET [] (docApiV2Get documentId) 200

testDocApiV2Update' :: Context -> MockDoc -> TestEnv MockDoc
testDocApiV2Update' ctx mockDoc = mockDocTestRequestHelper
  ctx
  POST
  [("document", mockDocToInput mockDoc)]
  (docApiV2Update $ getMockDocId mockDoc)
  200

testDocApiV2AddParties :: Context -> [MockSigLink] -> DocumentID -> TestEnv MockDoc
testDocApiV2AddParties ctx sigLinks documentId = do
  mockDoc <- testDocApiV2Get' ctx documentId
  let updatedMockDoc = addSigLinksToMockDoc sigLinks mockDoc
  testDocApiV2Update' ctx updatedMockDoc
