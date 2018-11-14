module Doc.API.V2.Calls.CallsTestUtils
  ( testDocApiV2New'
  , testDocApiV2Start'
  ) where

import Happstack.Server

import Context
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Types.DocumentStatus (DocumentStatus(..))
import TestingUtil
import TestKontra

testDocApiV2New' :: Context -> TestEnv MockDoc
testDocApiV2New' ctx =
  mockDocTestRequestHelper ctx POST
  [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  docApiV2New 201

testDocApiV2Start' :: Context -> TestEnv MockDoc
testDocApiV2Start' ctx = do
  did     <- getMockDocId <$> testDocApiV2New' ctx
  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
  assertEqual "Document status should match after 'start' call"
    Pending (getMockDocStatus mockDoc)
  return mockDoc
