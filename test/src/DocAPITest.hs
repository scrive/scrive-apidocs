module DocAPITest (docAPITests) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy.Char8 as BS
import Doc.DocStateData
import Text.JSON

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

  req <- mkRequest POST [ ("expectedType", inText "text")
                       , ("file", inFile "test/pdfs/simple.pdf")]
  (_, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile

  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertBool "just one doc" (length docs == 1)
  let doc = head docs

  req' <- mkRequest POST [("json", inText cont)]
  _ <- runTestKontra req' ctx $ apiCallUpdate $ documentid doc

  req'' <- mkRequest POST []
  (rsp, _) <- runTestKontra req'' ctx $ apiCallReady $ documentid doc
  let rspString = BS.unpack $ rsBody rsp
      Ok (JSObject response) = decode rspString
      Just (JSString sts) = lookup "status" $ fromJSObject response

  assertBool "status is pending" (fromJSString sts == "Pending")

  return ctx

testSetAutoReminder :: TestEnv ()
testSetAutoReminder = do
  ctx@Context{ctxmaybeuser = Just user} <- testUpdateDoc $ head jsonDocs
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  req <- mkRequest POST [("days", inText "3")]
  (res, _) <- runTestKontra req ctx $ apiCallSetAutoReminder (documentid doc)

  assertEqual ("response code is 202 (response is " ++ show res ++ ")") (rsCode res) 202
