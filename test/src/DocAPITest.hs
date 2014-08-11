module DocAPITest (docAPITests) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy.UTF8 as BS
import Doc.DocStateData
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen

import Context
import Doc.Model
import Utils.Default
import User.Model
import TestingUtil
import TestKontra as T
import Doc.API

docAPITests :: TestEnvSt -> Test
docAPITests env = testGroup "DocAPI" $
  map (\d -> testThat (d ++ " updates correctly") env (void $ testUpdateDoc d)) jsonDocs
  ++ [
    testThat "settings auto reminder works" env testSetAutoReminder
  , testThat "change main file works" env testChangeMainFile
  , testThat "change main file moves placements" env testChangeMainFileMovePlacements
  , testThat "Changing authentication method works" env testChangeAuthenticationMethod
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
     (_, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile
     return ()

  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "Just one doc" 1 (length docs)
  let doc = head docs

  do
     req <- mkRequest POST [("json_is_missing", inText cont)]
     (rsp,_) <- runTestKontra req ctx $ apiCallUpdate $ documentid doc
     assertEqual "Status code when json field is missing" 400 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [ ("json", inText cont)
                           , ("objectversion", inText "3412342341")]
     (rsp,_) <- runTestKontra req ctx $ apiCallUpdate $ documentid doc
     assertEqual "Status code for invalid objectversion is 409" 409 (rsCode rsp)
     return ()

  do
     req <- mkRequest POST [("json", inText cont)]
     _ <- runTestKontra req ctx $ apiCallUpdate $ documentid doc
     return ()

  do
     req <- mkRequest POST []
     (rsp, _) <- runTestKontra req ctx $ apiCallReady $ documentid doc
     let rspString = BS.toString $ rsBody rsp
         Ok (JSObject response) = decode rspString
         Just (JSString sts) = lookup "status" $ fromJSObject response

     assertEqual "Status is pending" "Pending" (fromJSString sts)

  return ctx

testSetAutoReminder :: TestEnv ()
testSetAutoReminder = do
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ head jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  req <- mkRequest POST [("days", inText "3")]
  (res, _) <- runTestKontra req ctx $ apiCallSetAutoReminder (documentid doc)

  assertEqual "response code is 202" 202 (rsCode res)

testChangeAuthenticationMethod :: TestEnv ()
testChangeAuthenticationMethod = do
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ head jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)
  let siglinks = documentsignatorylinks doc
      validsiglinkid = signatorylinkid $ head $ filter signatoryispartner siglinks

  reqNoAuthMethod <- mkRequest POST [("authentication_value", inText "+46701234567")]
  (resNoAuthMethod, _) <- runTestKontra reqNoAuthMethod ctx $ apiChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is not 400" 400 (rsCode resNoAuthMethod)

  reqInvalidMethod <- mkRequest POST [("authentication_type", inText "god_is_witness")]
  (resInvalidMethod, _) <- runTestKontra reqInvalidMethod ctx $ apiChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is not 400" 400 (rsCode resInvalidMethod)

  req <- mkRequest POST [("authentication_type", inText "sms_pin"),("authentication_value", inText "+46701234567")]
  (res, _) <- runTestKontra req ctx $ apiChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is not 202" 202 (rsCode res)

  user2 <- addNewRandomUser
  ctx2 <- (\c -> c { ctxmaybeuser = Just user2 }) <$> mkContext defaultValue
  (resBadUser, _) <- runTestKontra req ctx2 $ apiChangeAuthentication (documentid doc) validsiglinkid
  assertEqual "Response code is not 403" 403 (rsCode resBadUser)

testChangeMainFile :: TestEnv ()
testChangeMainFile = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue

  req <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile "test/pdfs/simple.pdf")]
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile

  let rspString = BS.toString $ rsBody rsp1
      Ok (JSObject response) = decode rspString
      Just (JSString sts) = lookup "id" $ fromJSObject response

  docid <- liftIO $ readIO (fromJSString sts)
  req' <- mkRequest POST [ ("file", inFile "test/pdfs/simple.pdf")]

  (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid

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
  (rsp1, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile

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
    (rsp,_) <- runTestKontra req' ctx $ apiCallUpdate $ docid
    assertEqual "update call suceeded" 200 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions in initial anchors-Signature" [(1,0.5,0.5)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to anchors-Namnteckning" [(2,0.5,0.800784)] poss

  do
    req' <- mkRequest POST [ ("file", inFile noanchorpdf)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to no anchors document" [(2,0.5,0.800784)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf2)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change back to anchors-Namnteckning" [(2,0.5,0.800784)] poss

  do
    req' <- mkRequest POST [ ("file", inFile anchorpdf3)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    assertEqual "positions after change to anchors-Unterschrift-2" [(3,0.5,1.1015681)] poss

  do
    -- it should return to the original position at this point
    req' <- mkRequest POST [ ("file", inFile anchorpdf1)]
    (rsp,_) <- runTestKontra req' ctx $ apiCallChangeMainFile $ docid
    assertEqual "suceeded" 202 (rsCode rsp)
    poss <- getPositionsFromResponse rsp
    -- we are almost exactly in the place we started
    assertEqual "positions after change to anchors-Signature" [(1,0.5,0.50000006)] poss

  return ()
