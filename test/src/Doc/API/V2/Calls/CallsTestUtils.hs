module Doc.API.V2.Calls.CallsTestUtils
  ( testDocApiV2New'
  , testDocApiV2Start'
  ) where

import Data.Default
import Happstack.Server

import Context
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Mock.TestUtils
import Doc.Data.DocumentStatus (DocumentStatus(..))
import Doc.DocumentID (DocumentID)
import KontraPrelude
import TestingUtil
import TestKontra
import User.Model (User)

testDocApiV2New' :: Context -> TestEnv MockDoc
testDocApiV2New' ctx = mockDocTestRequestHelper ctx POST [("file", inFile "test/pdfs/simple.pdf")] docApiV2New 201

testDocApiV2Start' :: TestEnv (User, Context, DocumentID, MockDoc)
testDocApiV2Start' = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext def
  did <- getMockDocId <$> testDocApiV2New' ctx
  mockDoc <- mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
  assertEqual "Document status should match after 'start' call" Pending (getMockDocStatus mockDoc)
  return (user, ctx, did, mockDoc)
