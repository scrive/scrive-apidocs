module Doc.API.V1.CallsTest (apiV1CallsTests) where

import Control.Monad.Trans
import Happstack.Server
import Network.URI
import Test.Framework
import Test.QuickCheck
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import qualified Data.ByteString.Lazy.UTF8 as BS

import Context
import DB
import Doc.API.V1.Calls
import Doc.API.V1.DocumentToJSON
import Doc.DocStateData
import Doc.Model
import KontraPrelude
import OAuth.Model
import TestingUtil
import TestKontra as T
import User.Model
import Util.HasSomeUserInfo
import Utils.Default

apiV1CallsTests :: TestEnvSt -> Test
apiV1CallsTests env = testGroup "CallsAPIV1" $
  map (\d -> testThat (d ++ " updates correctly") env (void $ testUpdateDoc d)) jsonDocs
  ++ [
    testThat "settings auto reminder works" env testSetAutoReminder
  , testThat "change main file works" env testChangeMainFile
  , testThat "change main file moves placements" env testChangeMainFileMovePlacements
  , testThat "Changing authentication method works" env testChangeAuthenticationMethod
  , testThat "Changing authentication method without changing existing info works" env testChangeAuthenticationMethodWithEmptyAuthenticationValue
  , testThat "Creating doc with access credentials via API works" env testOAuthCreateDoc
  , testThat "Creating doc with personal access credentials via API works" env testPersonalAccessCredentialsCreateDoc
  , testThat "Save flag in update call works" env (testUpdateDocToSaved False)
  , testThat "Save flag works when using API (OAuth)" env (testUpdateDocToSaved True)
  ]

jsonDocs :: [String]
jsonDocs = [
    "test/json/document1.json"
  , "test/json/document2.json"
  ]

testUpdateDoc :: String -> TestEnv Context
testUpdateDoc updateJsonPath = do
  cont <- liftIO $ readFile updateJsonPath

  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  do
     req <- mkRequest POST [ ("expectedType", inText "text")
                           , ("file", inFile "test/pdfs/simple.pdf")]
     (_, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile
     return ()

  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "Just one doc" 1 (length docs)
  let doc = $head docs

  do
     req <- mkRequest POST [("json_is_missing", inText cont)]
     (rsp,_) <- runTestKontra req ctx $ apiCallV1Update $ documentid doc
     assertEqual "Status code when json field is missing" 400 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [ ("json", inText cont)
                           , ("objectversion", inText "3412342341")]
     (rsp,_) <- runTestKontra req ctx $ apiCallV1Update $ documentid doc
     assertEqual "Status code for invalid objectversion is 409" 409 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [("json", inText cont)]
     _ <- runTestKontra req ctx $ apiCallV1Update $ documentid doc
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
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  -- Create OAuth API tokens
  let uid = userid user
  _ <- dbUpdate $ CreateAPIToken uid
  (apitoken, apisecret) : _ <- dbQuery $ GetAPITokensForUser uid
  time <- rand 10 arbitrary
  Just (tok, sec) <- dbUpdate $ RequestTempCredentials
    (OAuthTempCredRequest
      { tcCallback   = $fromJust $ parseURI "http://www.google.com/"
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
             ++ ",oauth_consumer_key=\"" ++ show apitoken ++ "\""
             ++ ",oauth_token=\"" ++ show t ++"\""
             ++ ",oauth_signature=\"" ++ show apisecret ++ "&" ++ show s ++ "\""
  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ]
                                      [("authorization", [authStr])]
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
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  -- Get the personal access token
  let uid = userid user
  _ <- dbUpdate $ DeletePersonalToken uid
  _ <- dbUpdate $ CreatePersonalToken uid
  Just (apitoken, apisecret, t, s) <- dbQuery $ GetPersonalToken uid

  let authStr = "oauth_signature_method=\"PLAINTEXT\""
             ++ ",oauth_consumer_key=\"" ++ show apitoken ++ "\""
             ++ ",oauth_token=\"" ++ show t ++"\""
             ++ ",oauth_signature=\"" ++ show apisecret ++ "&" ++ show s ++ "\""
  reqDoc <- mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                      , ("file", inFile "test/pdfs/simple.pdf")
                                      ]
                                      [("authorization", [authStr])]
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
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ $head jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  req <- mkRequest POST [("days", inText "3")]
  (res, _) <- runTestKontra req ctx $ apiCallV1SetAutoReminder (documentid doc)

  assertEqual "response code is 202" 202 (rsCode res)

testUpdateDocToSaved :: Bool -> TestEnv ()
testUpdateDocToSaved useOAuth = do
  user <- addNewRandomUser
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  authStr <- if useOAuth then do
    _ <- dbUpdate $ DeletePersonalToken (userid user)
    _ <- dbUpdate $ CreatePersonalToken (userid user)
    Just (apitoken, apisecret, t, s) <- dbQuery $ GetPersonalToken (userid user)
    return $ Just $ "oauth_signature_method=\"PLAINTEXT\""
                 ++ ",oauth_consumer_key=\"" ++ show apitoken ++ "\""
                 ++ ",oauth_token=\"" ++ show t ++"\""
                 ++ ",oauth_signature=\"" ++ show apisecret ++ "&" ++ show s ++ "\""
  else return Nothing

  reqDocJSON <- mkRequest GET []
  let mkDocJSON d = runTestKontra reqDocJSON ctx $ documentJSONV1 (Just user) True True Nothing d

  reqDoc <- if useOAuth
    then mkRequestWithHeaders POST [ ("expectedType", inText "text")
                                   , ("file", inFile "test/pdfs/simple.pdf")
                                   ]
                                   [("authorization", [$fromJust authStr])]
    else mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile "test/pdfs/simple.pdf")]
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
  when (not useOAuth) $ do
    assertEqual "The created document should not be saved (JSON)" False newSaved
    assertEqual "The created document should not be saved (DB)" True (documentunsaveddraft doc)

  -- Make document saved using update API call and check if it works
  (resDocSavedJSON, _) <- mkDocJSON doc{documentunsaveddraft = False}
  reqDocSaved <- mkRequest POST [("json", inText $ encode resDocSavedJSON)]
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
  reqUnsave <- mkRequest POST [("json", inText $ encode resUnsaveJSON)]
  (resUnsave, _) <- runTestKontra reqUnsave ctx $ apiCallV1Update did
  assertEqual "Updating document did not return HTTP 200" 200 (rsCode resUnsave)
  let rus = BS.toString $ rsBody resUnsave
      Ok (JSObject rusr) = decode rus
      Just (JSBool unsaved) = lookup "saved" $ fromJSObject rusr
  assertEqual "Document should still be saved (JSON)" True unsaved
  docUnsaved <- dbQuery $ GetDocumentByDocumentID did
  assertEqual "Document should still be saved (DB)" False (documentunsaveddraft docUnsaved)

testChangeAuthenticationMethod :: TestEnv ()
testChangeAuthenticationMethod = do
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ $head jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ $head $ filter signatoryispartner siglinks

  reqNoAuthMethod <- mkRequest POST [("authentication_value", inText "+46701234567")]
  (resNoAuthMethod, _) <- runTestKontra reqNoAuthMethod ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resNoAuthMethod)

  reqInvalidMethod <- mkRequest POST [("authentication_type", inText "god_is_witness")]
  (resInvalidMethod, _) <- runTestKontra reqInvalidMethod ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code should be 400" 400 (rsCode resInvalidMethod)

  req <- mkRequest POST [("authentication_type", inText "sms_pin"),("authentication_value", inText "+46701234567")]
  (res, _) <- runTestKontra req ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code should be 202" 202 (rsCode res)

  user2 <- addNewRandomUser
  ctx2 <- (\c -> c { ctxmaybeuser = Just user2 }) <$> mkContext defaultValue
  (resBadUser, _) <- runTestKontra req ctx2 $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code should be 403" 403 (rsCode resBadUser)

testChangeAuthenticationMethodWithEmptyAuthenticationValue :: TestEnv ()
testChangeAuthenticationMethodWithEmptyAuthenticationValue = do
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ $last jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ $head $ filter signatoryispartner siglinks

  req <- mkRequest POST [("authentication_type", inText "sms_pin"),("authentication_value", inText "+46701234567")]
  (res, _) <- runTestKontra req ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is supposed to be 202" 202 (rsCode res)

  updatedDoc <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedsiglinks = documentsignatorylinks updatedDoc
      siglink = $head $ filter signatoryispartner updatedsiglinks
  assertEqual "The phone number +46701234567 should be set there" "+46701234567" (getMobile siglink)

  req2 <- mkRequest POST [("authentication_type", inText "standard")]
  (res2, _) <- runTestKontra req2 ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is supposed to be 202" 202 (rsCode res2)

  req3 <- mkRequest POST [("authentication_type", inText "sms_pin")]
  (res3, _) <- runTestKontra req3 ctx $ apiCallV1ChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is not 202" 202 (rsCode res3)

  updatedDoc' <- dbQuery $ GetDocumentBySignatoryLinkID validsiglinkid
  let updatedsiglinks' = documentsignatorylinks updatedDoc'
      siglink' = $head $ filter signatoryispartner updatedsiglinks'
  assertEqual "The phone number +46701234567 should be STILL there" "+46701234567" (getMobile siglink')

testChangeMainFile :: TestEnv ()
testChangeMainFile = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  req <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile "test/pdfs/simple.pdf")]
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile

  let rspString = BS.toString $ rsBody rsp1
      Ok (JSObject response) = decode rspString
      Just (JSString sts) = lookup "id" $ fromJSObject response

  docid <- liftIO $ readIO (fromJSString sts)
  req' <- mkRequest POST [ ("file", inFile "test/pdfs/simple.pdf")]

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

testChangeMainFileMovePlacements :: TestEnv ()
testChangeMainFileMovePlacements = do
  let anchorpdf1 = "test/pdfs/anchor-Signature.pdf"
  let anchorpdf2 = "test/pdfs/anchor-Namnteckning.pdf"
  let anchorpdf3 = "test/pdfs/anchor-Unterschrift-2.pdf"
  let noanchorpdf = "test/pdfs/simple.pdf"

  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

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
        JSArray (arr ++ [runJSONGen $ do
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
                                                    value "index" (1::Int)
                                                    value "pages" [1,2::Int],
                                                 do
                                                    value "text" ("Namnteckning" :: String)
                                                    value "index" (1::Int)
                                                    value "pages" [1,2::Int],
                                                 do
                                                    value "text" ("Unterschrift" :: String)
                                                    value "index" (2::Int)
                                                    value "pages" [1,2,3::Int]]]])
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
    req' <- mkRequest POST [("json", inText (encode updatejs))]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1Update $ docid
    assertEqual "update call suceeded" 200 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions in initial anchors-Signature" [(1,0.5,0.5)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to anchors-Namnteckning" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile noanchorpdf)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to no anchors document" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change back to anchors-Namnteckning" [(2,0.5,0.80078405)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf3)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to anchors-Unterschrift-2" [(3,0.5,1.1015681)] poss

  do
    -- it should return to the original position at this point
    req' <- mkRequest POST [ ("file", inFile anchorpdf1)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallV1ChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    -- we are almost exactly in the place we started
    assertEqual "positions after change to anchors-Signature" [(1,0.5,0.5)] poss

  return ()
