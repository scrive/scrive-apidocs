module APICommonsTest (apiCommonsTest) where

import Control.Monad.IO.Class
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.UTF8 as BS hiding (length)
import TestingUtil
import Text.JSON
import Util.JSON
import Doc.DocStateData
import API.APICommons
import Test.QuickCheck
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo 
import qualified Data.ByteString as BS
import qualified  Codec.Binary.Base64 as BASE64
import MinutesTime
import Doc.DocStateCommon
import TestKontra

apiCommonsTest :: TestEnvSt -> Test
apiCommonsTest env = testGroup "API Commons Test" [
  testCase "Tag JSON 1" testTagJSON,
  testThat "Tag JSON Random" env testTagJSONRandom,
  testCase "Involved JSON 1" testInvolvedJSON,
  testCase "File JSON" testFileJSON,
  testCase "Document JSON" testDocumentJSON
  ]

testTagJSON :: Assertion
testTagJSON = do
  let tag = DocumentTag "hello" "goodbye"
      js  = api_document_tag tag
  testJSONStringLookup "value" js $ tagvalue tag
  testJSONStringLookup "name" js $ tagname tag

testTagJSONRandom :: TestEnv ()
testTagJSONRandom = doNTimes 100 $ do
  tag <- rand 20 arbitrary
  let js  = api_document_tag tag
  testJSONStringLookup "value" js $ tagvalue tag
  testJSONStringLookup "name" js $ tagname tag

testInvolvedJSON :: Assertion
testInvolvedJSON = do
  let sl = signatoryLinkExample1
      js = api_signatory sl
  testJSONStringLookup "email"      js $ getEmail          sl
  testJSONStringLookup "fstname"    js $ getFirstName      sl
  testJSONStringLookup "sndname"    js $ getLastName       sl
  testJSONStringLookup "company"    js $ getCompanyName    sl
  testJSONStringLookup "companynr"  js $ getCompanyNumber  sl
  testJSONStringLookup "personalnr" js $ getPersonalNumber sl
  testJSONStringLookup "seen" js $ showMinutesTimeForAPI $ fromSeconds 0
  testJSONStringLookup "sign" js $ showMinutesTimeForAPI $ fromSeconds 0
  case js of
    JSObject obj -> case getJSONField "fields" obj of
      Nothing -> assertFailure ("fields is missing, should be in: " ++ show js)
      Just (JSArray [phone]) -> do
        testJSONStringLookup "name" phone "phone"
        testJSONStringLookup "value" phone "504-302-3742"
      Just _ -> assertFailure ("fields should be an array with one element in: " ++ show js)
    _ -> assertFailure $ "Expected Object bug got: " ++ show js

testFileJSON :: Assertion
testFileJSON = do
  let base64data = BASE64.encode (BS.unpack $ BS.fromString "abc")
  let js = api_file "file1" (BS.fromString "abc")
  testJSONStringLookup "name" js "file1"
  testJSONStringLookup "content" js  base64data

testDocumentJSON :: Assertion
testDocumentJSON = do
  let doc = blankDocument { documenttitle = "Cool Contract", documentallowedidtypes = [EmailIdentification] }
      jsv = api_document (Just []) doc
  testJSONStringLookup "title" jsv "Cool Contract"
  testJSONStringLookup "document_id" jsv "0"
  testJSONStringLookup "mdate" jsv $ showMinutesTimeForAPI $ fromSeconds 0
  case jsv of
    JSObject js -> do
      case getJSONField "state" js of
        Just (JSRational _ s) -> assertBool ("Document status was not between 0 and 10: " ++ show s ++ " in " ++ show js) $ 0 <= s && s < 10
        Just _ -> assertFailure ("Expected a number in state, got: " ++ show js)
        Nothing -> assertFailure $ "state not found in " ++ show js
      case getJSONField "authorization" js of
        Just (JSRational _ s) -> assertBool ("Document allowed id type was not 1: " ++ show s ++ " in " ++ show js) $ s == 1
        Just _ -> assertFailure ("Expected a number in authorization, got: " ++ show js)
        Nothing -> assertFailure $ "authorization not found in " ++ show js
      testFieldExists "files" js
      testFieldExists "involved" js
      testFieldExists "tags" js
    _ -> assertFailure $ "Expected JSObject but got " ++ show jsv

testFieldExists :: String -> JSObject JSValue -> Assertion
testFieldExists k js = case getJSONField k js of
  Nothing -> assertFailure $ "Could not find field " ++ show k ++ " in " ++ show js
  _ -> assertSuccess

testJSONStringLookup :: MonadIO m => String -> JSValue -> String -> m ()
testJSONStringLookup k js e =
    case getJSONStringFieldSafe k js of
    Right v -> assertBool (k ++ " did not match; expected: " ++ show e ++ ", got: " ++ show v ++ " in json " ++ show js) $ v == e
    Left m -> assertFailure m
