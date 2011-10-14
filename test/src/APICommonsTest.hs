module APICommonsTest (apiCommonsTest) where
--import Control.Applicative
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

apiCommonsTest :: Test
apiCommonsTest = testGroup "API Commons Test" [
  testCase "Tag JSON 1" testTagJSON,
  testCase "Tag JSON Random" testTagJSONRandom,
  testCase "Involved JSON 1" testInvolvedJSON,
  testCase "File JSON" testFileJSON
  ]

testTagJSON :: Assertion
testTagJSON = do
  let tag = DocumentTag (BS.fromString "hello") (BS.fromString "goodbye")
      js  = api_document_tag tag
  testJSONStringLookup "value" js $ BS.toString $ tagvalue tag
  testJSONStringLookup "name" js $ BS.toString $ tagname tag
    
testTagJSONRandom :: Assertion
testTagJSONRandom = doNTimes 100 $ do
  tag <- rand 20 arbitrary
  let js  = api_document_tag tag
  testJSONStringLookup "value" js $ BS.toString $ tagvalue tag
  testJSONStringLookup "name" js $ BS.toString $ tagname tag
  
testInvolvedJSON :: Assertion
testInvolvedJSON = do
  let sl = signatoryLinkExample1
      js = api_signatory sl
  testJSONStringLookup "email"      js $ BS.toString $ getEmail          sl
  testJSONStringLookup "fstname"    js $ BS.toString $ getFirstName      sl
  testJSONStringLookup "sndname"    js $ BS.toString $ getLastName       sl
  testJSONStringLookup "company"    js $ BS.toString $ getCompanyName    sl
  testJSONStringLookup "companynr"  js $ BS.toString $ getCompanyNumber  sl
  testJSONStringLookup "personalnr" js $ BS.toString $ getPersonalNumber sl
  testJSONStringLookup "seen" js "1969-12-31 21:00"
  testJSONStringLookup "sign" js "1969-12-31 21:00"
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
  let js = api_file (BS.fromString "file1") (BS.fromString "abc")
  testJSONStringLookup "name" js "file1"
  testJSONStringLookup "content" js  base64data
  
testJSONStringLookup :: String -> JSValue -> String -> Assertion
testJSONStringLookup k js e =
    case getJSONStringFieldSafe k js of
    Right v -> assertBool (k ++ " did not match; expected: " ++ show e ++ ", got: " ++ show v ++ " in json " ++ show js) $ v == e
    Left m -> assertFailure m
