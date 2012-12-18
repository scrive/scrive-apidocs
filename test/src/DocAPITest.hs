module DocAPITest (docAPITests) where

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
docAPITests env = testGroup "DocAPI"
                  [ testThat "document1.json updates correctly" env $ testUpdateDoc "test/json/document1.json"
                  , testThat "document2.json updates correctly" env $ testUpdateDoc "test/json/document2.json"
                  ]

testUpdateDoc :: String -> TestEnv ()
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
