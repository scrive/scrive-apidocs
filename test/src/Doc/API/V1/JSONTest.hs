{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V1.JSONTest (apiV1JSONTests) where

import Doc.API.V1.JSONTypes

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

apiV1JSONTests :: TestEnvSt -> Test
apiV1JSONTests env = testGroup "JSONAPIV1"
  [ testThat "Test JSON structure for API V1 'createfromfile' and 'ready'" env testFromFileAndReadySimple
  , testThat "Test JSON structure for API V1 'createfromfile' and 'update' with result is the same just createfromfile" env testFromFileAndUpdate
  , testThat "Test JSON structure for API V1 'createfromtemplate' and 'ready'" env testFromTemplateAndReadySimple
  , testThat "Test JSON structure for API V1 is consistent after 'update'" env testUpdateFields
  , testThat "Test JSON structure for API V1 is consistent after 'update' based on #EMAIL, #FSTNAME, etc fields" env testUpdateWithReplacementFields
  ]

testFromFileAndReadySimple :: TestEnv ()
testFromFileAndReadySimple = do
  (Just user)  <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) v1_docUnjsonDef

  reqReady <- mkRequest POST []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready (unsafeDocumentID $ fromInt64AsString $ v1_docid doc)
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_ready.json" (rsBody resReady) v1_docUnjsonDef
  return ()

testFromFileAndUpdate :: TestEnv ()
testFromFileAndUpdate = do
  (Just user)  <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) v1_docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ v1_docid doc

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS $ rsBody resDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  _ <- testJSONWith "test/json/test_createFromFile.json" (rsBody resUpdate) v1_docUnjsonDef

  return ()


testFromTemplateAndReadySimple :: TestEnv ()
testFromTemplateAndReadySimple = do
  (Just user)  <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) v1_docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ v1_docid doc

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      value = setDocKey "template" (Bool True) docJSON
      strDoc = encode value

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  _ <- testJSONWith "test/json/test_createFromFileAndSavedAsTemplate.json" (rsBody resUpdate) v1_docUnjsonDef

  reqFromTemplate <- mkRequestWithHeaders POST [] []
  (resFromTemplate, _) <- runTestKontra reqFromTemplate ctx $ apiCallV1CreateFromTemplate did
  assertEqual "We should get a 201 response" 201 (rsCode resFromTemplate)
  docFromTemplate <- testJSONWith "test/json/test_createFromFile.json" (rsBody resFromTemplate) v1_docUnjsonDef
  let newdid = unsafeDocumentID $ fromInt64AsString $ v1_docid docFromTemplate

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready newdid
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_ready.json" (rsBody resReady) v1_docUnjsonDef
  return ()

testUpdateFields :: TestEnv ()
testUpdateFields = do
  (Just user) <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) v1_docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ v1_docid doc

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      newSetup  = setDocKey "invitationmessage"   (String "424242") .
                  setDocKey "confirmationmessage" (String "363636") .
                  setDocKey "apicallbackurl" (String "242424")
      strDoc = encode $ newSetup docJSON

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  doc' <- testJSONWith "test/json/test_updateFields.json" (rsBody resUpdate) v1_docUnjsonDef

  assertEqual "Invitation message should be the same" "<p>424242</p>" (v1_docinvitetext doc')
  assertEqual "Confirmation message should be the same" "<p>363636</p>" (v1_docconfirmtext doc')
  assertEqual "API Callback should be the same" (Just "242424") (v1_docapicallbackurl doc')

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready did
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  _ <- testJSONWith "test/json/test_updateFieldsReady.json" (rsBody resReady) v1_docUnjsonDef
  return ()


testUpdateWithReplacementFields :: TestEnv ()
testUpdateWithReplacementFields = do
  (Just user) <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  doc <- testJSONWith "test/json/test_createFromFile.json" (rsBody resDoc) v1_docUnjsonDef
  let did = unsafeDocumentID $ fromInt64AsString $ v1_docid doc
  jsonFileBS <- liftIO $ B.readFile "test/json/test_updateWithReplacement.json"

  reqUpdate1 <- mkRequestWithHeaders POST [("json", inTextBS jsonFileBS)] []
  (resUpdate1, _) <- runTestKontra reqUpdate1 ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate1)
  _ <- testJSONWith "test/json/test_updateWithReplacement.json" (rsBody resUpdate1) v1_docUnjsonDef

  let Just docJSON = decode (rsBody resUpdate1) :: Maybe Value
      replaceValues  = setDocValuesBySimpleReplacement "#TITLE" "New title" .
                       setDocValuesBySimpleReplacement "#FSTNAME" "Mariusz" .
                       setDocValuesBySimpleReplacement "#LSTNAME" "Rak" .
                       setDocValuesBySimpleReplacement "#EMAIL" " mariusz@scrive.com " .
                       setDocValuesBySimpleReplacement "#COMPANY" "Scrive" .
                       setDocValuesBySimpleReplacement "#PHONE" "+48694528309" .
                       setDocValuesBySimpleReplacement "#PRICE" " MIL<&>LION "
      strDoc = encode $ replaceValues docJSON

  reqUpdate2 <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate2, _) <- runTestKontra reqUpdate2 ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate2)
  _ <- testJSONWith "test/json/test_updateWithReplacementAfterReplacement.json" (rsBody resUpdate2) v1_docUnjsonDef
  return ()

-- | Takes a 'FilePath' to a JSON file with the desired structure (with 'null'
-- considered as a type), a 'ByteString' with the JSON to test, and a
-- 'UnjsonDef' which the ByteString should parse with.
--
-- Checks that the ByteString parses, and also that it matches the JSON given
-- in 'FilePath' (using (==) and 'removeValues').
testJSONWith :: FilePath -> BS.ByteString -> UnjsonDef V1_Doc -> TestEnv V1_Doc
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
  assertEqual ("JSON structure and values should match if we will remove dynamic values (like documentid or mtime) " ++ fp)
              (removeDynamicValues jsonFile) (removeDynamicValues value)
  return doc

removeValues :: Value -> Value
removeValues (Object m) = Object (H.map removeValues m)
removeValues (Array v)  = Array  (V.map removeValues v)
removeValues (String _) = String ""
removeValues (Number _) = Number 0
removeValues (Bool _)   = Bool False
removeValues Null       = Null


removeDynamicValues :: Value -> Value
removeDynamicValues (Object m) = Object $ H.map removeDynamicValues $ filterOutDynamicKeys m
  where
    filterOutDynamicKeys hm = H.filterWithKey (\k _ -> not $ k `elem` dynamicKeys) hm
    dynamicKeys = ["id", "accesstoken", "time", "ctime", "ntime", "userid", "timeouttime", "objectversion"]
removeDynamicValues (Array v)  = Array  (V.map removeDynamicValues v)
removeDynamicValues v = v

setDocKey :: Text -> Value -> Value -> Value
setDocKey k n v = overDocKey k (const n) v

overDocKey :: Text -> (Value -> Value) -> Value -> Value
overDocKey k f (Object doc) = Object $ H.adjust f k doc
overDocKey _ _ v = v


-- Deap replacement. There are some integrations that work with replacemente of #EMAIL etc. This operation
setDocValuesBySimpleReplacement :: Text -> Text -> Value -> Value
setDocValuesBySimpleReplacement v nv (Object m) = Object $ H.map (setDocValuesBySimpleReplacement v nv) m
setDocValuesBySimpleReplacement v nv (Array a)  = Array $ V.map (setDocValuesBySimpleReplacement v nv) a
setDocValuesBySimpleReplacement v nv (String v') = if (v == v')
                                                   then String nv
                                                   else String v'
setDocValuesBySimpleReplacement _ _ jv = jv