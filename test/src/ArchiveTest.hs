module ArchiveTest (archiveTests) where

-- import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy.Char8 as BS
import Doc.DocStateData
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.String
import Data.String.Utils (replace)

import Context
import Doc.Model
import Utils.Default
import User.Model
import TestingUtil
import TestKontra as T
import Doc.API

archiveTests :: TestEnvSt -> Test
archiveTests env = testGroup "Archive" $
  [ testThat "Archive lists docs correctly" env testListDocs
  ]

testListDocs :: TestEnv ()
testListDocs = do
  cont <- liftIO $ readFile "test/json/document1.json"
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"

  -- send a doc as author
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequest POST [ ("expectedType", inText "text")
                       , ("file", inFile "test/pdfs/simple.pdf")]
  _ <- runTestKontra req ctx $ apiCallCreateFromFile
  doc:_ <- randomQuery $ GetDocumentsByAuthor (userid user)
  req' <- mkRequest POST [("json", inText cont)]
  _ <- runTestKontra req' ctx $ apiCallUpdate $ documentid doc
  req'' <- mkRequest POST []
  _ <- runTestKontra req'' ctx $ apiCallReady $ documentid doc

  -- send a doc to author from someoneelse
  (Just user2) <- addNewUser "Jackie" "Chan" "jackie@chan.com"
  ctx2 <- (\c -> c { ctxmaybeuser = Just user2 }) <$> mkContext defaultValue
  req2 <- mkRequest POST [ ("expectedType", inText "text")
                        , ("file", inFile "test/pdfs/simple.pdf")]
  _ <- runTestKontra req2 ctx2 $ apiCallCreateFromFile
  doc2:_ <- randomQuery $ GetDocumentsByAuthor (userid user2)
  let cont2 = replace "example@example.com" "bob@blue.com" $ -- send to bob
                replace "\"signorder\":2" "\"signorder\":1" cont -- reset sign order to 1
  req2' <- mkRequest POST [("json", inText cont2)]
  _ <- runTestKontra req2' ctx2 $ apiCallUpdate $ documentid doc2
  req2'' <- mkRequest POST []
  _ <- runTestKontra req2'' ctx2 $ apiCallReady $ documentid doc2

  req3 <- mkRequest GET []
  (rsp, _) <- runTestKontra req3 ctx $ apiCallList
  let rspString = BS.unpack $ rsBody rsp

  let Right json = runGetJSON readJSObject rspString
  withJSValue json $ do
    Just (list :: [JSValue])  <- fromJSValueField  "list"
    assertBool "Test apiCallList number of docs" $ length list == 2

    Just (authors :: [String]) <- fromJSValueFieldCustom "list" $
                                  fromJSValueCustomMany $
                                     fromJSValueFieldCustom "fields" $
                                       fromJSValueField "author"
    let (doc1Index, doc2Index) = case head authors of
                                   "Bob Blue" -> (0, 1)
                                   _          -> (1, 0)

    Just (isAuthors :: [Bool]) <- fromJSValueFieldCustom "list" $
                                  fromJSValueCustomMany (fromJSValueField "isauthor")
    assertBool "Test apiCallList isauthor for author is True" $ isAuthors !! doc1Index
    assertBool "Test apiCallList isauthor for recipient is False" $ not $ isAuthors !! doc2Index

    Just (docauthorcompanysameasusers :: [Bool]) <- fromJSValueFieldCustom "list" $
                                  fromJSValueCustomMany (fromJSValueField "docauthorcompanysameasuser")
    assertBool "Test apiCallList docauthorcompanysameasuser for author is True" $ docauthorcompanysameasusers !! doc1Index
    assertBool "Test apiCallList docauthorcompanysameasuser for recipient is False" $ not $ docauthorcompanysameasusers !! doc2Index 

  return ()
