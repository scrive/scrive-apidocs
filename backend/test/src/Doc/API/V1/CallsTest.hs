module Doc.API.V1.CallsTest (apiV1CallsTests) where

import Control.Monad.Trans
import Happstack.Server
import Network.URI
import Test.Framework
import Test.QuickCheck
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Text as T

import Context
import DB
import Doc.Action
import Doc.API.V1.Calls
import Doc.API.V1.DocumentToJSON
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model
import Doc.SignatoryScreenshots
import EID.Authentication.Model
import EID.CGI.GRP.Types ()
import KontraError
import OAuth.Model
import Session.Model
import TestingUtil
import TestKontra as T
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

apiV1CallsTests :: TestEnvSt -> Test
apiV1CallsTests env = testGroup "CallsAPIV1" $
  map (\d -> testThat (d <> " updates correctly") env (void $ testUpdateDoc d)) jsonDocs
  <> [
    testThat "settings auto reminder works" env testSetAutoReminder
  , testThat "change main file works" env testChangeMainFile
  , testThat "change main file moves placements" env testChangeMainFileMovePlacements
  , testThat "change main file moves placements with negative index" env testChangeMainFileMovePlacementsWithNegativeIndex
  , testThat "Changing authentication to view method works" env testChangeAuthenticationToViewMethod
  , testThat "Changing authentication to sign method works" env testChangeAuthenticationToSignMethod
  , testThat "Changing authentication to sign method without changing existing info works" env testChangeAuthenticationToSignMethodWithEmptyAuthenticationValue
  , testThat "Creating doc with access credentials via API works" env testOAuthCreateDoc
  , testThat "Creating doc with personal access credentials via API works" env testPersonalAccessCredentialsCreateDoc
  , testThat "Save flag in update call works" env (testUpdateDocToSaved False)
  , testThat "Save flag works when using API (OAuth)" env (testUpdateDocToSaved True)
  , testThat "Closing a document has the right Evidence Attachments" env testCloseEvidenceAttachments
  ]

jsonDocs :: [String]
jsonDocs = [
    inTestDir "json/document1.json"
  , inTestDir "json/document2.json"
  ]

testUpdateDoc :: String -> TestEnv Context
testUpdateDoc updateJsonPath = do
  cont <- liftIO $ readFile updateJsonPath

  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  do
     req <- mkRequest POST [ ("expectedType", inText "text")
                           , ("file", inFile $ inTestDir "pdfs/simple.pdf")]
     (_, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile
     return ()

  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "Just one doc" 1 (length docs)
  let doc = head docs

  do
     req <- mkRequest POST [("json_is_missing", inText $ T.pack cont)]
     (rsp,_) <- runTestKontra req ctx $ apiCallV1Update $ documentid doc
     assertEqual "Status code when json field is missing" 400 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [ ("json", inText $ T.pack cont)
                           , ("objectversion", inText "3412342341")]
     (rsp,_) <- runTestKontra req ctx $ apiCallV1Update $ documentid doc
     assertEqual "Status code for invalid objectversion is 409" 409 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [("json", inText $ T.pack cont)]
     void $ runTestKontra req ctx $ apiCallV1Update $ documentid doc
     return ()

  do
     req <- mkRequest POST []
     (rsp, _) <- runTestKontra req ctx $ apiCallV1Ready $ documentid doc
     let rspString = BS.toString $ rsBody rsp
         Ok (JSObject response) = decode rspString
         Just (JSString sts) = lookup "status" $ fromJSObject response

     assertEqual "Status is pending" "Pending" (fromJSString sts)

  return ctx

testOAuthCreateDoc :: TestEnv ()
testOAuthCreateDoc = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  -- Create OAuth API tokens
  let uid = userid user
  void $ dbUpdate $ CreateAPIToken uid
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser uid
  time <- rand 10 arbitrary
  Just (tok, sec) <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest
      { tcCallback   = fromJust $ parseURI "http://www.google.com/"
      , tcAPIToken   = apitoken
      , tcAPISecret  = apisecret
      , tcPrivileges = [APIDocCreate]
      }
    ) time
  Just (_, ver) <- dbUpdate $ VerifyCredentials tok uid time
  Just (t, s) <- dbUpdate $ RequestAccessToken
    (OAuthTokenRequest
      { trAPIToken = apitoken
      , trAPISecret = apisecret
      , trTempToken = tok
      , trTempSecret = sec
      , trVerifier = ver
      }
    ) time
  let authStr = "oauth_signature_method=\"PLAINTEXT\""
             <> ",oauth_consumer_key=\"" <> show apitoken <> "\""
             <> ",oauth_token=\"" <> show t <>"\""
             <> ",oauth_signature=\"" <> show apisecret <> "&" <> show s <> "\""
  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                      , ("file", inFile $ inTestDir "pdfs/simple.pdf")
                                      ]
                                      [("authorization", [T.pack authStr])]
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  let rds = BS.toString $ rsBody resDoc
      Ok (JSObject rdsr) = decode rds
      Just (JSBool newSaved) = lookup "saved" $ fromJSObject rdsr
      Just (JSString docidStr) = lookup "id" $ fromJSObject rdsr
  did <- liftIO $ readIO (fromJSString docidStr)
  doc <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "The OAuth API created document should be saved (JSON)" True newSaved
  assertEqual "The OAuth API created document should be saved (DB)" False (documentunsaveddraft doc)

  (resReady, _) <- runTestKontra reqDoc ctx $ apiCallV1Ready did
  assertEqual "Ready should fail with 403" 403 (rsCode resReady)

testPersonalAccessCredentialsCreateDoc :: TestEnv ()
testPersonalAccessCredentialsCreateDoc = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  -- Get the personal access token
  let uid = userid user
  void $ dbUpdate $ DeletePersonalToken uid
  void $ dbUpdate $ CreatePersonalToken uid
  Just (OAuthAuthorization{..}) <- dbQuery $ GetPersonalToken uid

  let authStr = "oauth_signature_method=\"PLAINTEXT\""
             <> ",oauth_consumer_key=\"" <> show oaAPIToken <> "\""
             <> ",oauth_token=\"" <> show oaAccessToken <>"\""
             <> ",oauth_signature=\"" <> show oaAPISecret <> "&" <> show oaAccessSecret <> "\""
  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                      , ("file", inFile $ inTestDir "pdfs/simple.pdf")
                                      ]
                                      [("authorization", [T.pack authStr])]
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  let rds = BS.toString $ rsBody resDoc
      Ok (JSObject rdsr) = decode rds
      Just (JSBool newSaved) = lookup "saved" $ fromJSObject rdsr
      Just (JSString docidStr) = lookup "id" $ fromJSObject rdsr
  did <- liftIO $ readIO (fromJSString docidStr)
  doc <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "The OAuth API created document should be saved (JSON)" True newSaved
  assertEqual "The OAuth API created document should be saved (DB)" False (documentunsaveddraft doc)

testSetAutoReminder :: TestEnv ()
testSetAutoReminder = do
  ctx <- testUpdateDoc $ head jsonDocs
  let Just user = get ctxmaybeuser ctx
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  req <- mkRequest POST [("days", inText "3")]
  (res, _) <- runTestKontra req ctx $ apiCallV1SetAutoReminder (documentid doc)

  assertEqual "response code is 202" 202 (rsCode res)

testUpdateDocToSaved :: Bool -> TestEnv ()
testUpdateDocToSaved useOAuth = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  authStr <- if useOAuth then do
    void $ dbUpdate $ DeletePersonalToken (userid user)
    void $ dbUpdate $ CreatePersonalToken (userid user)
    Just (OAuthAuthorization{..}) <- dbQuery $ GetPersonalToken (userid user)

    return $ Just $ "oauth_signature_method=\"PLAINTEXT\""
               <> ",oauth_consumer_key=\"" <> show oaAPIToken <> "\""
               <> ",oauth_token=\"" <> show oaAccessToken <>"\""
               <> ",oauth_signature=\"" <> show oaAPISecret <> "&" <> show oaAccessSecret <> "\""
  else return Nothing

  reqDocJSON <- mkRequest GET []
  let mkDocJSON d = runTestKontra reqDocJSON ctx $ documentJSONV1 (Just user) True True Nothing d

  reqDoc <- if useOAuth
    then mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                   , ("file", inFile $ inTestDir "pdfs/simple.pdf")
                                   ]
                                   [("authorization", [T.pack $ fromJust authStr])]
    else mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (resDoc, _) <- runTestKontra reqDoc ctx $ apiCallV1CreateFromFile
  assertEqual "We should get a 201 response" 201 (rsCode resDoc)
  let rds = BS.toString $ rsBody resDoc
      Ok (JSObject rdsr) = decode rds
      Just (JSBool newSaved) = lookup "saved" $ fromJSObject rdsr
      Just (JSString docidStr) = lookup "id" $ fromJSObject rdsr
  did <- liftIO $ readIO (fromJSString docidStr)
  doc <- dbQuery $ GetDocumentByDocumentID did
  when useOAuth $ do
    assertEqual "The API created document should be saved (JSON)" True newSaved
    assertEqual "The API created document should be saved (DB)" False (documentunsaveddraft doc)
  unless useOAuth $ do
    assertEqual "The created document should not be saved (JSON)" False newSaved
    assertEqual "The created document should not be saved (DB)" True (documentunsaveddraft doc)

  -- Make document saved using update API call and check if it works
  (resDocSavedJSON, _) <- mkDocJSON doc{documentunsaveddraft = False}
  reqDocSaved <- mkRequest POST [("json", inText $ T.pack $ encode resDocSavedJSON)]
  (resDocSaved, _) <- runTestKontra reqDocSaved ctx $ apiCallV1Update did
  assertEqual "Updating document did not return HTTP 200" 200 (rsCode resDocSaved)
  let rdss = BS.toString $ rsBody resDocSaved
      Ok (JSObject rdssr) = decode rdss
      Just (JSBool saved) = lookup "saved" $ fromJSObject rdssr
  assertEqual "Document should be saved (JSON)" True saved
  docSaved <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document should be saved (DB)" False (documentunsaveddraft docSaved)

  -- Try to make document not saved using update API call and make sure it
  -- cannot happen
  (resUnsaveJSON, _) <- mkDocJSON docSaved{documentunsaveddraft = True}
  reqUnsave <- mkRequest POST [("json", inText $ T.pack $ encode resUnsaveJSON)]
  (resUnsave, _) <- runTestKontra reqUnsave ctx $ apiCallV1Update did
  assertEqual "Updating document did not return HTTP 200" 200 (rsCode resUnsave)
  let rus = BS.toString $ rsBody resUnsave
      Ok (JSObject rusr) = decode rus
      Just (JSBool unsaved) = lookup "saved" $ fromJSObject rusr
  assertEqual "Document should still be saved (JSON)" True unsaved
  docUnsaved <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document should still be saved (DB)" False (documentunsaveddraft docUnsaved)

testChangeAuthenticationToViewMethod :: TestEnv ()
testChangeAuthenticationToViewMethod = do
  ctx <- testUpdateDoc $ head jsonDocs
  let Just user = get ctxmaybeuser ctx
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ head $ filter isSignatory siglinks

  reqNoAuthMethod <- mkRequest POST [("personal_number", inText "12345678901")]
  (resNoAuthMethod, _) <- runTestKontra reqNoAuthMethod ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resNoAuthMethod)

  reqInvalidMethod <- mkRequest POST [("authentication_type", inText "god_is_witness")]
  (resInvalidMethod, _) <- runTestKontra reqInvalidMethod ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resInvalidMethod)

  reqNOBankIDNoSSN <- mkRequest POST [("authentication_type", inText "no_bankid")]
  (resNOBankIDNoSSN, _) <- runTestKontra reqNOBankIDNoSSN ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resNOBankIDNoSSN)

  reqNOBankIDValid <- mkRequest POST [("authentication_type", inText "no_bankid"),("personal_number", inText "12345678901")]
  (resNOBankIDValid, _) <- runTestKontra reqNOBankIDValid ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resNOBankIDValid)

  reqNOBankIDValidWithMobile <- mkRequest POST [("authentication_type", inText "no_bankid"),("personal_number", inText "12345678901"),("mobile_number", inText "+4712345678")]
  (resNOBankIDValidWithMobile, _) <- runTestKontra reqNOBankIDValidWithMobile ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resNOBankIDValidWithMobile)
  updatedDocNOBankID <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedSigLinkNOBankID = documentsignatorylinks updatedDocNOBankID
      siglinkNOBankID = head $ filter isSignatory updatedSigLinkNOBankID
  assertEqual "Authentication to view should be NOBankID" NOBankIDAuthenticationToView (signatorylinkauthenticationtoviewmethod siglinkNOBankID)
  assertEqual "The phone number +4712345678 should be set" "+4712345678" (getMobile siglinkNOBankID)
  assertEqual "The personal number 12345678901 should be set" "12345678901" (getPersonalNumber siglinkNOBankID)

  reqNOBankIDInvalidMobile <- mkRequest POST [("authentication_type", inText "no_bankid"),("personal_number", inText "12345678901"),("mobile_number", inText "+4612345678")]
  (resNOBankIDInvalidMobile, _) <- runTestKontra reqNOBankIDInvalidMobile ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resNOBankIDInvalidMobile)

  reqSEBankIDButOldNOSSN <- mkRequest POST [("authentication_type", inText "se_bankid")]
  (resSEBankIDButOldNOSSN, _) <- runTestKontra reqSEBankIDButOldNOSSN ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resSEBankIDButOldNOSSN)

  reqSEBankIDValid12digits <- mkRequest POST [("authentication_type", inText "se_bankid"),("personal_number", inText "123456789012")]
  (resSEBankIDValid12digits, _) <- runTestKontra reqSEBankIDValid12digits ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resSEBankIDValid12digits)
  updatedDocSEBankID <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedSigLinkSEBankID = documentsignatorylinks updatedDocSEBankID
      siglinkSEBankID = head $ filter isSignatory updatedSigLinkSEBankID
  assertEqual "Authentication to view should be SEBankID" SEBankIDAuthenticationToView (signatorylinkauthenticationtoviewmethod siglinkSEBankID)
  assertEqual "The personal number 123456789012 should be set" "123456789012" (getPersonalNumber siglinkSEBankID)

  reqSEBankIDValid10digits <- mkRequest POST [("authentication_type", inText "se_bankid"),("personal_number", inText "1234567890")]
  (resSEBankIDValid10digits, _) <- runTestKontra reqSEBankIDValid10digits ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resSEBankIDValid10digits)

  reqStandard <- mkRequest POST [("authentication_type", inText "standard")]
  (resStandard, _) <- runTestKontra reqStandard ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resStandard)
  updatedDocStandard <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedSigLinkStandard = documentsignatorylinks updatedDocStandard
      siglinkStandard = head $ filter isSignatory updatedSigLinkStandard
  assertEqual "The personal number 1234567890 should be set from previous call" "1234567890" (getPersonalNumber siglinkStandard)
  assertEqual "The mobile number +4712345678 should be set from previous call" "+4712345678" (getMobile siglinkStandard)
  assertEqual "Authentication to view should be Standard" StandardAuthenticationToView (signatorylinkauthenticationtoviewmethod siglinkStandard)

  -- Check that we can't change authentication to view for signatory that has already authenticated to view
  reqSEBankIDAgain <- mkRequest POST [("authentication_type", inText "se_bankid"),("personal_number", inText "1234567890")]
  (resSEBankIDAgain, _) <- runTestKontra reqSEBankIDAgain ctx $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode resSEBankIDAgain)

  (sessionid, ctx')<- runTestKontra reqSEBankIDAgain ctx $ getNonTempSessionID
  dbUpdate $ MergeCGISEBankIDAuthentication (mkAuthKind doc) sessionid validsiglinkid $
    CGISEBankIDAuthentication {
        cgisebidaSignatoryName = "AName"
      , cgisebidaSignatoryPersonalNumber = "BName"
      , cgisebidaSignatoryIP = "1.2.3.4"
      , cgisebidaSignature = "sig_here"
      , cgisebidaOcspResponse = "sig_resp"
    }
  reqStandardAgain <- mkRequest POST [("authentication_type", inText "standard")]
  (resStandardAgain, _) <- runTestKontra reqStandardAgain ctx' $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resStandardAgain)

  -- Check that we can't change authentication to view if we are logged as user not connected to document
  user2 <- addNewRandomUser
  ctx2 <- (set ctxmaybeuser (Just user2)) <$> mkContext defaultLang
  (resBadUser, _) <- runTestKontra reqSEBankIDValid10digits ctx2 $ apiCallV1ChangeAuthenticationToView (documentid doc) validsiglinkid
  assertEqual "Response code should be 403" 403 (rsCode resBadUser)

testChangeAuthenticationToSignMethod :: TestEnv ()
testChangeAuthenticationToSignMethod = do
  ctx <- testUpdateDoc $ head jsonDocs
  let Just user = get ctxmaybeuser ctx
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ head $ filter isSignatory siglinks

  reqNoAuthMethod <- mkRequest POST [("authentication_value", inText "+46701234567")]
  (resNoAuthMethod, _) <- runTestKontra reqNoAuthMethod ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resNoAuthMethod)

  reqInvalidMethod <- mkRequest POST [("authentication_type", inText "god_is_witness")]
  (resInvalidMethod, _) <- runTestKontra reqInvalidMethod ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resInvalidMethod)

  req <- mkRequest POST [("authentication_type", inText "sms_pin"),("authentication_value", inText "+46701234567")]
  (res, _) <- runTestKontra req ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode res)

  user2 <- addNewRandomUser
  ctx2 <- (set ctxmaybeuser (Just user2)) <$> mkContext defaultLang
  (resBadUser, _) <- runTestKontra req ctx2 $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code should be 403" 403 (rsCode resBadUser)

testChangeAuthenticationToSignMethodWithEmptyAuthenticationValue :: TestEnv ()
testChangeAuthenticationToSignMethodWithEmptyAuthenticationValue = do
  ctx <- testUpdateDoc $ head jsonDocs
  let Just user = get ctxmaybeuser ctx
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ head $ filter isSignatory siglinks

  req <- mkRequest POST [("authentication_type", inText "sms_pin"),("authentication_value", inText "+46701234567")]
  (res, _) <- runTestKontra req ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code is supposed to be 202" 202 (rsCode res)

  updatedDoc <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedsiglinks = documentsignatorylinks updatedDoc
      siglink = head $ filter isSignatory updatedsiglinks
  assertEqual "The phone number +46701234567 should be set there" "+46701234567" (getMobile siglink)

  req2 <- mkRequest POST [("authentication_type", inText "standard")]
  (res2, _) <- runTestKontra req2 ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code is supposed to be 202" 202 (rsCode res2)

  req3 <- mkRequest POST [("authentication_type", inText "sms_pin")]
  (res3, _) <- runTestKontra req3 ctx $ apiCallV1ChangeAuthenticationToSign (documentid doc) validsiglinkid
  assertEqual "Response code is not 202" 202 (rsCode res3)

  updatedDoc' <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedsiglinks' = documentsignatorylinks updatedDoc'
      siglink' = head $ filter isSignatory updatedsiglinks'
  assertEqual "The phone number +46701234567 should be STILL there" "+46701234567" (getMobile siglink')

testChangeMainFile :: TestEnv ()
testChangeMainFile = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  req <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile

  let rspString = BS.toString $ rsBody rsp1
      Ok (JSObject response) = decode rspString
      Just (JSString sts) = lookup "id" $ fromJSObject response

  docid <- liftIO $ readIO (fromJSString sts)
  req' <- mkRequest POST [ ("file", inFile $ inTestDir "pdfs/simple.pdf")]

  (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid

  assertEqual "suceeded" 202 (rsCode rsp)

  return ()


mapObjectEntry :: String -> (Maybe JSValue -> Maybe JSValue) -> JSValue -> JSValue
mapObjectEntry key func old@(JSObject obj) =
  case lookup (toJSKey key) (fromJSObject obj) of
    Nothing -> case func Nothing of
      Nothing -> old
      Just newValue -> JSObject (toJSObject ((toJSKey key,newValue) : (fromJSObject obj)))
    Just oldValue -> case func (Just oldValue) of
      Nothing -> JSObject (toJSObject (filter ((/= toJSKey key) . fst) (fromJSObject obj)))
      Just newValue -> JSObject (toJSObject (map replaceValue (fromJSObject obj)))
        where
          replaceValue (k,_) | k == toJSKey key = (k,newValue)
          replaceValue x = x

mapObjectEntry _key _func somethingElse = somethingElse

-- this test is checking if negative field placements are
-- working as AVIS expects them to be (meaning if you have anchors
-- Signature w/ index -1 & Unterschrift w/ index -1
-- both match, we should pick the one that is last (when things are negative?)
testChangeMainFileMovePlacementsWithNegativeIndex :: TestEnv ()
testChangeMainFileMovePlacementsWithNegativeIndex = do
  let anchorpdf1 = inTestDir "pdfs/anchor-avis-contract-1.pdf"
  let anchorpdf2 = inTestDir "pdfs/anchor-avis-contract-2.pdf"

  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  req <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile anchorpdf1)]
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile

  let rspString = BS.toString $ rsBody rsp1
      Ok jsvalue@(JSObject response) = decode rspString
      Just (JSString sts) = lookup "id" $ fromJSObject response

  docid <- liftIO $ readIO (fromJSString sts)


  -- update field placement:
  {-
     "anchors":[{"text":"Signature","index": -1},
                {"text":"Unterschrift","index":-1}]
  -}
  let updatejs = mapObjectEntry "signatories"
                 (fmap (mapFirstInArray (mapObjectEntry "fields" (fmap (addAnchoredField)))))
                 jsvalue
      mapFirstInArray :: (JSValue -> JSValue) -> JSValue -> JSValue
      mapFirstInArray func (JSArray (x:xs)) = JSArray (func x : xs)
      mapFirstInArray _func x = x
      addAnchoredField :: JSValue -> JSValue
      addAnchoredField (JSArray arr) =
        JSArray (arr <> [runJSONGen $ do
                            value "name" ("anchored-field" :: String)
                            value "type" ("custom" :: String)
                            value "value" ("value!!" :: String)
                            objects "placements" [do
                              value "xrel" (0.5 :: Double)
                              value "yrel" (0.5 :: Double)
                              value "wrel" (0.2 :: Double)
                              value "hrel" (0.2 :: Double)
                              value "fsrel" (0.02 :: Double)
                              value "page" (1 :: Int)
                              objects "anchors" [do
                                                    value "text" ("Signature" :: String)
                                                    value "index" (-1::Int),
                                                 do
                                                    value "text" ("Unterschrift" :: String)
                                                    value "index" (-1::Int)]]])
      addAnchoredField x = x

  let getPositionsFromResponse :: Response -> TestEnv [(Int,Double,Double)]
      getPositionsFromResponse rsp = do
        let rspString1 = BS.toString $ rsBody rsp
            Ok js = decode rspString1
        Just (positions :: [[[(Int,Double,Double)]]]) <- withJSValue js $ do
              fromJSValueFieldCustom "signatories" . fromJSValueCustomMany $ do
                fromJSValueFieldCustom "fields" . fromJSValueCustomMany $ do
                  fromJSValueFieldCustom "placements" . fromJSValueCustomMany $ do
                    xrel <- fromJSValueField "xrel"
                    yrel <- fromJSValueField "yrel"
                    page <- fromJSValueField "page"
                    return ((,,) <$> page <*> xrel <*> yrel)
        return (concat (concat positions))

  do
    liftIO $ putStrLn "POST update"
    req' <- mkRequest POST [("json", inText (T.pack $ encode updatejs))]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1Update $ docid
    assertEqual "update call suceeded" 200 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions in initial anchors-Signature" [(1,0.5,0.5)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions after change to anchors-Unterschrift" [(6,0.5,0.513)] poss

testChangeMainFileMovePlacements :: TestEnv ()
testChangeMainFileMovePlacements = do
  let anchorpdf1 = inTestDir "pdfs/anchor-Signature.pdf"
  let anchorpdf2 = inTestDir "pdfs/anchor-Namnteckning.pdf"
  let anchorpdf3 = inTestDir "pdfs/anchor-Unterschrift-2.pdf"
  let noanchorpdf = inTestDir "pdfs/simple.pdf"

  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  req <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile anchorpdf1)]
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile

  let rspString = BS.toString $ rsBody rsp1
      Ok jsvalue@(JSObject response) = decode rspString
      Just (JSString sts) = lookup "id" $ fromJSObject response

  docid <- liftIO $ readIO (fromJSString sts)


  -- update field placement:
  {-
     "anchors":[{"text":"Signature","pages":[1,2]},
                {"text":"Namnteckning","pages":[1,2]},
                {"text":"Unterschrift","index":2,"pages":[1,2,3]}]
  -}
  let updatejs = mapObjectEntry "signatories"
                 (fmap (mapFirstInArray (mapObjectEntry "fields" (fmap (addAnchoredField)))))
                 jsvalue
      mapFirstInArray :: (JSValue -> JSValue) -> JSValue -> JSValue
      mapFirstInArray func (JSArray (x:xs)) = JSArray (func x : xs)
      mapFirstInArray _func x = x
      addAnchoredField :: JSValue -> JSValue
      addAnchoredField (JSArray arr) =
        JSArray (arr <> [runJSONGen $ do
                            value "name" ("anchored-field" :: String)
                            value "type" ("custom" :: String)
                            value "value" ("value!!" :: String)
                            objects "placements" [do
                              value "xrel" (0.5 :: Double)
                              value "yrel" (0.5 :: Double)
                              value "wrel" (0.2 :: Double)
                              value "hrel" (0.2 :: Double)
                              value "fsrel" (0.02 :: Double)
                              value "page" (1 :: Int)
                              objects "anchors" [do
                                                    value "text" ("Signature" :: String)
                                                    value "index" (1::Int),
                                                 do
                                                    value "text" ("Namnteckning" :: String)
                                                    value "index" (1::Int),
                                                 do
                                                    value "text" ("Unterschrift" :: String)
                                                    value "index" (2::Int)
                                                    -- note: "pages" is a backward compatibility mode that should be removed someday
                                                    ]]])
      addAnchoredField x = x

  let getPositionsFromResponse :: Response -> TestEnv [(Int,Double,Double)]
      getPositionsFromResponse rsp = do
        let rspString1 = BS.toString $ rsBody rsp
            Ok js = decode rspString1
        Just (positions :: [[[(Int,Double,Double)]]]) <- withJSValue js $ do
              fromJSValueFieldCustom "signatories" . fromJSValueCustomMany $ do
                fromJSValueFieldCustom "fields" . fromJSValueCustomMany $ do
                  fromJSValueFieldCustom "placements" . fromJSValueCustomMany $ do
                    xrel <- fromJSValueField "xrel"
                    yrel <- fromJSValueField "yrel"
                    page <- fromJSValueField "page"
                    return ((,,) <$> page <*> xrel <*> yrel)
        return (concat (concat positions))

  do
    liftIO $ putStrLn "POST update"
    req' <- mkRequest POST [("json", inText (T.pack $ encode updatejs))]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1Update $ docid
    assertEqual "update call suceeded" 200 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions in initial anchors-Signature" [(1,0.5,0.5)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions after change to anchors-Namnteckning" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile noanchorpdf)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions after change to no anchors document" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions after change back to anchors-Namnteckning" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf3)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqualDouble "positions after change to anchors-Unterschrift-2" [(3,0.5,1.1015681)] poss

  do
    -- it should return to the original position at this point
    req' <- mkRequest POST [ ("file", inFile anchorpdf1)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    -- we are almost exactly in the place we started
    -- assertEqual "positions after change to anchors-Signature" [(1,0.5,0.5)] poss
    assertEqualDouble "positions after change to anchors-Signature" [(1,0.5,0.5)] poss

-- TEMPORARY HACK TO MAKE TIM HAPPY AND MAKE THIS TEST PASS ON A WEIRD SYSTEM
-- THIS NEEDS TO BE DONE PROPERLY AND EVERYWHERE WHERE WE COMPARE DOUBLES!!!
round' :: Double -> Int
round' z = floor $ 1000 * z + 0.5

assertEqualDouble :: (MonadIO m) => String -> [(Int, Double, Double)] -> [(Int, Double, Double)] -> m()
assertEqualDouble msg [(x1,x2,x3)] [(y1,y2,y3)] = do
  assertEqual msg x1 y1
  assertEqual msg (round' x2) (round' y2)
  assertEqual msg (round' x3) (round' y3)
assertEqualDouble msg x y = do assertEqual msg x y

testCloseEvidenceAttachments:: TestEnv ()
testCloseEvidenceAttachments = do
  author <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just author)) <$> mkContext defaultLang
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
    { randomDocumentTypes = Or [Signable]
    , randomDocumentStatuses = Or [Pending]
    , randomDocumentSignatories =
        let signatory = Or
              [ And [ RSC_IsSignatoryThatHasntSigned
                    , RSC_AuthToSignIs StandardAuthenticationToSign
                    ]
              ]
        in Or [[signatory]]
    }

  req <- mkRequest GET []
  withDocument doc $ forM_ (documentsignatorylinks doc)
    (\sl -> when (isSignatory sl) $ do
      randomUpdate $ \t-> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
      randomUpdate $ \t-> SignDocument     (signatorylinkid sl) Nothing Nothing emptySignatoryScreenshots (systemActor t)
    )

  void $ runTestKontra req ctx $ withDocument doc $ do
      randomUpdate $ \t -> CloseDocument (systemActor t)
      postDocumentClosedActions True False  `E.catch` (\(_::KontraError) -> return False)

  (rsp, _) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments (documentid doc)
  let rspString = BS.toString $ rsBody rsp
      Ok (JSObject response) = decode rspString
      Just (JSArray attachments) = lookup "evidenceattachments" $ fromJSObject response
      attachmentNames = map (\(JSObject obj) -> let Just (JSString name) = lookup "name" $ fromJSObject obj
                                                in fromJSString name
                            ) attachments
  assertEqual "Evidence Attachments file names and order do not match"
      [ "Evidence Quality of Scrive E-signed Documents.html"
      , "Appendix 1 Evidence Quality Framework.html"
      , "Appendix 2 Service Description.html"
      , "Appendix 3 Evidence Log.html"
      , "Appendix 4 Evidence of Time.html"
      , "Appendix 5 Evidence of Intent.html"
      , "Appendix 6 Digital Signature Documentation.html"
      ]
      attachmentNames
