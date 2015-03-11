{-# LANGUAGE OverloadedStrings #-}
module DocJSONTest (docJSONTests) where

import DocJSONTypes

import Control.Applicative
import Control.Monad.IO.Class
import Happstack.Server
import Test.Framework
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Unjson as U
import Data.Text (Text)
import Data.Unjson
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BS

import Context
import Doc.API.V1.Calls
import TestingUtil
import TestKontra as T
import Utils.Default
import Doc.DocumentID (unsafeDocumentID)

docJSONTests :: TestEnvSt -> Test
docJSONTests env = testGroup "DocJSON"
  [ testThat "Test JSON structure for API V1 'createfromfile' and 'ready'" env testFromFileAndReadySimple
  , testThat "Test JSON structure for API V1 'createfromtemplate' and 'ready'" env testFromTemplateAndReadySimple
  , testThat "Test JSON structure for API V1 is consistent after 'update'" env testUpdateFields
  ]

testFromFileAndReadySimple :: TestEnv ()
testFromFileAndReadySimple = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) docUnjsonDef

  reqReady <- mkRequest POST []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready (unsafeDocumentID $ fromInt64AsString $ docid doc)
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_ready.json" (rsBody resReady) docUnjsonDef
  return ()

testFromTemplateAndReadySimple :: TestEnv ()
testFromTemplateAndReadySimple = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ docid doc

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      value = setDocKey "template" (Bool True) docJSON
      strDoc = encode value

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  _ <- testJSONWith "test/json/test_createFromFile.json" (rsBody resUpdate) docUnjsonDef

  reqFromTemplate <- mkRequestWithHeaders POST [] []
  (resFromTemplate, _) <- runTestKontra reqFromTemplate ctx $ apiCallV1CreateFromTemplate did
  assertEqual "We should get a 201 response" 201 (rsCode resFromTemplate)
  docFromTemplate <- testJSONWith "test/json/test_createFromFile.json" (rsBody resFromTemplate) docUnjsonDef
  let newdid = unsafeDocumentID $ fromInt64AsString $ docid docFromTemplate

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready newdid
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_ready.json" (rsBody resReady) docUnjsonDef
  return ()

testUpdateFields :: TestEnv ()
testUpdateFields = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ docid doc

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      value  = setDocKey "invitationmessage"   (String "424242") docJSON
      value' = setDocKey "confirmationmessage" (String "363636") value
      value'' = setDocKey "apicallbackurl" (String "242424") value'
      strDoc = encode value''

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  doc' <- testJSONWith "test/json/test_updateFields.json" (rsBody resUpdate) docUnjsonDef

  assertEqual "Invitation message should be the same" "<p>424242</p>" (docinvitetext doc')
  assertEqual "Confirmation message should be the same" "<p>363636</p>" (docconfirmtext doc')
  assertEqual "API Callback should be the same" (Just "242424") (docapicallbackurl doc')

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready did
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_updateFieldsReady.json" (rsBody resReady) docUnjsonDef
  return ()

-- | Takes a 'FilePath' to a JSON file with the desired structure (with 'null'
-- considered as a type), a 'ByteString' with the JSON to test, and a
-- 'UnjsonDef' which the ByteString should parse with.
--
-- Checks that the ByteString parses, and also that it matches the JSON given
-- in 'FilePath' (using (==) and 'removeValues').
testJSONWith :: FilePath -> BS.ByteString -> UnjsonDef Doc -> TestEnv Doc
testJSONWith fp jsonBS useUnjsonDef = do
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
      -- FIXME Can we use the 'a' in U.Result to test stuff?
      U.Result doc problems = parse useUnjsonDef value
  -- Using Unjson we check several things about the JSON in 'jsonBS':
  -- 1) That the correct fields exist
  -- 2) That these are of the correct type
  -- 3) That some JSON String types have the correct "underlying" type or set
  --    of values (e.g. An Int64 inside JSON String, or a datatype inside a
  --    JSON String)
  assertEqual "There should be no problems in Result" [] problems
  -- The second check is similar, but not as powerful or helpful as Unjson, but
  -- what it does that Unjson cannot do is:
  -- 1) Check that no extra fields exist
  -- 2) Check that Null fields match, regardless of what their type "should" be
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)
  return doc

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null

getDocKey :: Text -> Value -> Maybe Value
getDocKey k (Object doc) = H.lookup k doc
getDocKey _ _ = Nothing

setDocKey :: Text -> Value -> Value -> Value
setDocKey k n v = overDocKey k (const n) v

overDocKey :: Text -> (Value -> Value) -> Value -> Value
overDocKey k f (Object doc) = Object $ H.adjust f k doc
overDocKey _ _ v = v
