module ArchiveTest (archiveTests) where

-- import Control.Monad

import Happstack.Server
import Test.Framework
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.String
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

import Doc.API.V1.Calls
import Doc.DocStateData
import Doc.Model
import TestingUtil
import TestKontra as T
import User.Model

archiveTests :: TestEnvSt -> Test
archiveTests env =
  testGroup "Archive" $ [testThat "Archive lists docs correctly" env testListDocs]

testListDocs :: TestEnv ()
testListDocs = do
  cont        <- readTestFileAsStr "json/document1.json"
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"

  -- send a doc as author
  ctx         <- (set #ctxMaybeUser (Just user)) <$> mkContext defaultLang
  req         <- mkRequest
    POST
    [("expectedType", inText "text"), ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  void $ runTestKontra req ctx $ apiCallV1CreateFromFile
  doc : _ <- randomQuery $ GetDocumentsByAuthor (userid user)
  req'    <- mkRequest POST [("json", inText cont)]
  void $ runTestKontra req' ctx $ apiCallV1Update $ documentid doc
  req'' <- mkRequest POST []
  void $ runTestKontra req'' ctx $ apiCallV1Ready $ documentid doc

  -- send a doc to author from someoneelse
  (Just user2) <- addNewUser "Jackie" "Chan" "jackie@chan.com"
  ctx2         <- (set #ctxMaybeUser (Just user2)) <$> mkContext defaultLang
  req2         <- mkRequest
    POST
    [("expectedType", inText "text"), ("file", inFile $ inTestDir "pdfs/simple.pdf")]
  void $ runTestKontra req2 ctx2 $ apiCallV1CreateFromFile
  doc2 : _ <- randomQuery $ GetDocumentsByAuthor (userid user2)
  let cont2 = T.replace "example@example.com" "bob@blue.com"
        $ -- send to bob
          T.replace "\"signorder\":2" "\"signorder\":1" cont -- reset sign order to 1
  req2' <- mkRequest POST [("json", inText cont2)]
  void $ runTestKontra req2' ctx2 $ apiCallV1Update $ documentid doc2
  req2'' <- mkRequest POST []
  void $ runTestKontra req2'' ctx2 $ apiCallV1Ready $ documentid doc2

  req3     <- mkRequest GET []
  (rsp, _) <- runTestKontra req3 ctx $ apiCallV1List
  let rspString  = BS.unpack $ rsBody rsp

  let Right json = runGetJSON readJSObject rspString

  withJSValue json $ do
    Just (list :: [JSValue]) <- fromJSValueField "list"
    assertBool "Test apiCallV1List number of docs" $ length list == 2

    Just (authors :: [String]) <-
      fromJSValueFieldCustom "list"
      $ fromJSValueCustomMany
      $ fromJSValueFieldCustom "fields"
      $ fromJSValueField "author"
    let (doc1Index, doc2Index) = case head authors of
          "Bob Blue" -> (0, 1)
          _          -> (1, 0)

    Just (isAuthors :: [Bool]) <- fromJSValueFieldCustom "list"
      $ fromJSValueCustomMany (fromJSValueField "isauthor")
    assertBool "Test apiCallV1List isauthor for author is True" $ isAuthors !! doc1Index
    assertBool "Test apiCallV1List isauthor for recipient is False"
      $  not
      $  isAuthors
      !! doc2Index

    Just (docauthorcompanysameasusers :: [Bool]) <- fromJSValueFieldCustom "list"
      $ fromJSValueCustomMany (fromJSValueField "docauthorcompanysameasuser")
    assertBool "Test apiCallV1List docauthorcompanysameasuser for author is True"
      $  docauthorcompanysameasusers
      !! doc1Index
    assertBool "Test apiCallV1List docauthorcompanysameasuser for recipient is False"
      $  not
      $  docauthorcompanysameasusers
      !! doc2Index

  return ()
