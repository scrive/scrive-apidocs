{-# LANGUAGE OverloadedStrings #-}
module Doc.API.V1.JSONTest (apiV1JSONTests) where


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
import qualified Data.ByteString.Lazy.Char8 as BSC
import Context
import Doc.API.V1.Calls
import TestingUtil
import TestKontra as T
import Utils.Default
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID, unsafeSignatoryLinkID)

apiV1JSONTests :: TestEnvSt -> Test
apiV1JSONTests env = testGroup "JSONAPIV1"
  [ testThat "Test 1: JSON structure for API V1 'createfromfile' and 'ready'" env testFromFileAndReadySimple
  , testThat "Test 2: JSON structure for API V1 'createfromfile' and 'update' with result is the same just createfromfile" env testFromFileAndUpdate
  , testThat "Test 3: JSON structure for API V1 'createfromtemplate' and 'ready'" env testFromTemplateAndReadySimple
  , testThat "Test 4: JSON structure for API V1 is consistent after 'update'" env testUpdateFields
  , testThat "Test 5: JSON structure for API V1 is consistent after 'update' based on #EMAIL, #FSTNAME, etc fields" env testUpdateWithReplacementFields
  , testThat "Test 6: JSON structure for API V1 'update' with small subset of Documen JSON still works " env testUpdateWithSubset
  , testThat "Test 7: JSON structure for API V1 'update' with all features I know " env testUpdateWithAllFeatures
  , testThat "Test 8: JSON structure for API V1 'list' with 3 documents " env testList
  ]


{- Test 1 -}
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
  doc <- testJSONWith "test/json/test_1_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)

  reqReady <- mkRequest POST []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready did
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  testJSONWith "test/json/test_1_ready.json" (rsBody resReady)

{- Test 2 -}
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
  testJSONWith "test/json/test_2_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)


  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS $ rsBody resDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  testJSONWith "test/json/test_2_create.json" (rsBody resUpdate)

{- Test 3 -}
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
  testJSONWith "test/json/test_3_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      value = setDocKey "template" (Bool True) docJSON
      strDoc = encode value

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  testJSONWith "test/json/test_3_savedAsTemplate.json" (rsBody resUpdate)

  reqFromTemplate <- mkRequestWithHeaders POST [] []
  (resFromTemplate, _) <- runTestKontra reqFromTemplate ctx $ apiCallV1CreateFromTemplate did
  assertEqual "We should get a 201 response" 201 (rsCode resFromTemplate)
  --When creating from template - result should be same as a template - before it was saved as template
  testJSONWith "test/json/test_3_create.json" (rsBody resFromTemplate)
  newdid <- getDocumentID (rsBody resFromTemplate)

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready newdid
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  testJSONWith "test/json/test_3_ready.json" (rsBody resReady)

{- Test 4 -}
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
  testJSONWith "test/json/test_4_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)

  let Just docJSON = decode (rsBody resDoc) :: Maybe Value
      newSetup  = setDocKey "invitationmessage"   (String "424242") .
                  setDocKey "confirmationmessage" (String "363636") .
                  setDocKey "apicallbackurl" (String "242424")
      strDoc = encode $ newSetup docJSON

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS strDoc)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  testJSONWith "test/json/test_4_update.json" (rsBody resUpdate)

  invMsg <- getField "invitationmessage"
  cnfMsg <- getField "confirmationmessage"
  apiClbkUrl <- getField "apicallbackurl"
  assertEqual "Invitation message should be the same" "<p>424242</p>" invMsg
  assertEqual "Confirmation message should be the same" "<p>363636</p>" cnfMsg
  assertEqual "API Callback should be the same" "242424" apiClbkUrl

  reqReady <- mkRequestWithHeaders POST [] []
  (resReady, _) <- runTestKontra reqReady ctx $ apiCallV1Ready did
  assertEqual "We should get a 202 response" 202 (rsCode resReady)
  testJSONWith"test/json/test_4_ready.json" (rsBody resReady)

{- Test 5 -}
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
  testJSONWith "test/json/test_5_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)
  jsonFileBS <- liftIO $ B.readFile "test/json/test_5_update.json"

  reqUpdate1 <- mkRequestWithHeaders POST [("json", inTextBS jsonFileBS)] []
  (resUpdate1, _) <- runTestKontra reqUpdate1 ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate1)
  testJSONWith "test/json/test_5_update.json" (rsBody resUpdate1)

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
  testJSONWith "test/json/test_5_update_result.json" (rsBody resUpdate2)
  return ()

{- Test 6 -}
testUpdateWithSubset :: TestEnv ()
testUpdateWithSubset = do
  (Just user) <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [] []
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  testJSONWith "test/json/test_6_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)
  jsonFileBS <- liftIO $ B.readFile "test/json/test_6_update.json"

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS jsonFileBS)] []
  (resUpdate, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did
  assertEqual "We should get a 200 response" 200 (rsCode resUpdate)
  testJSONWith "test/json/test_6_update_result.json" (rsBody resUpdate)
  return ()

{- Test 7 -}
testUpdateWithAllFeatures :: TestEnv ()
testUpdateWithAllFeatures = do
  (Just user) <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []

  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  testJSONWith "test/json/test_7_create.json" (rsBody resDoc)
  did <- getDocumentID (rsBody resDoc)

  jsonFileBS <- liftIO $ B.readFile "test/json/test_7_update.json"

  reqUpdate <- mkRequestWithHeaders POST [("json", inTextBS jsonFileBS)] []
  (_, _) <- runTestKontra reqUpdate ctx $ apiCallV1Update did

  reqUploadAttachments <- mkRequestWithHeaders POST [
                                        ("attachment_0", inFile "test/pdfs/telia.pdf")
                                      , ("attachment_1", inFile "test/pdfs/visa-application.pdf")
                          ] []
  (resUploadAttachments, _) <- runTestKontra reqUploadAttachments ctx $ apiCallV1SetAuthorAttachemnts did
  assertEqual "We should get a 200 response" 200 (rsCode resUploadAttachments)

  reqGet <- mkRequestWithHeaders GET [] []
  (resFinalDoc, _) <- runTestKontra reqGet ctx $ apiCallV1Get did

  assertEqual "We should get a 200 response" 200 (rsCode resFinalDoc)
  testJSONWith "test/json/test_7_update_result_with_attachments.json" (rsBody resFinalDoc)
  return ()

{- Test 8 -}
testList :: TestEnv ()
testList = do
  (Just user) <- addNewUser "Jonathan" "Jounty" "jonathan@scrive.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                        -- FIXME make this random-ish file
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ] []

  (resDoc1, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc1)
  did1 <- getDocumentID (rsBody resDoc1)

  (resDoc2, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc2)
  did2 <- getDocumentID (rsBody resDoc2)

  (resDoc3, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc3)
  did3 <- getDocumentID (rsBody resDoc3)
  did3sig <- head <$> getSignatoryLinksID (rsBody resDoc3)

  (resDoc4, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc4)
  did4 <- getDocumentID (rsBody resDoc4)
  did4sig <- head <$> getSignatoryLinksID (rsBody resDoc4)

  reqList1 <- mkRequestWithHeaders GET [] []
  (resList1, _) <- runTestKontra reqList1 ctx $ apiCallV1List
  testJSONWith "test/json/test_7_list_of_documents.json" (rsBody resList1)


  reqReady1 <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqReady1 ctx $ apiCallV1Ready did1

  reqReady2 <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqReady2 ctx $ apiCallV1Ready did2

  reqReady3 <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqReady3 ctx $ apiCallV1Ready did3

  reqReady4 <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqReady4 ctx $ apiCallV1Ready did4

  reqCancel <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqCancel ctx $ apiCallV1Cancel did2

  reqSign <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqSign ctx $ apiCallV1Sign did3 did3sig

  reqReject <- mkRequestWithHeaders POST [] []
  _ <- runTestKontra reqReject ctx $ apiCallV1Reject did4 did4sig

  reqList2 <- mkRequestWithHeaders GET [] []
  (resList2, _) <- runTestKontra reqList2 ctx $ apiCallV1List
  testJSONWith "test/json/test_7_list_of_documents.json" (rsBody resList2)


-- Compare JSON sesults from API calls
testJSONWith :: FilePath -> BS.ByteString -> TestEnv ()
testJSONWith fp jsonBS = do
  liftIO $ BSC.putStrLn $ jsonBS
  jsonFileBS <- liftIO $ B.readFile fp
  let Just value    = decode jsonBS
      Just jsonFile = decode jsonFileBS
  assertEqual ("JSON structure and types (including 'null') should match that in " ++ fp)
              (removeValues jsonFile) (removeValues value)
  assertEqual ("JSON structure and values should match if we will remove dynamic values (like documentid or mtime) " ++ fp)
              (removeDynamicValues jsonFile) (removeDynamicValues value)
  return ()

-- So utils fro getting common properties from Document JSON
getDocumentID :: BS.ByteString -> TestEnv DocumentID
getDocumentID jsonBS = do
  jsonFileBS <- liftIO $ B.readFile fp
  let (Just v) = decode jsonBS
  return $ unsafeDocumentID <$> getID  v

getSignatoryLinksID :: BS.ByteString -> TestEnv [SignatoryLinkID]
getSignatoryLinksID jsonBS = do
  let (Just (Object m)) = decode jsonBS
  let (Just (Array s)) = H.lookup "signatories" m
  mapM getID s

getID :: Value -> TestEnv Int64
getID (Object m) = case (maybeRead =<< H.lookup "id" m) of
    Just i -> return i
    _ -> assertFailure "Error while parsing id (not a number)"
getID _ = assertFailure "Error while parsing id (not found)"

getField :: String -> BS.ByteString -> TestEnv Int64
getField key jsonBS = do
    let (Just (Object m)) = decode jsonBS
    case (H.lookup key m) of
      Just (String s) -> return s
      _ -> assertFailure "Error while looking for a field " ++ key ++ " (not a string)"
getField key _ = assertFailure "Error while looking for a field " ++ key


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
    dynamicKeys = ["id", "accesstoken", "time", "ctime", "mtime", "userid", "timeouttime", "objectversion", "title"]
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