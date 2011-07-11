module MailAPITest (mailApiTests) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Happstack.Server
import Happstack.State hiding (Method)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.JSON.Generic
import Text.Regex.TDFA ((=~))
import System.IO.Temp
import qualified Data.ByteString.Char8 as BS

import API.MailAPI
import MinutesTime
import Context
import StateHelper
import Doc.DocState
import User.UserState
import Templates.TemplatesLoader
import TestKontra as T

mailApiTests :: Test
mailApiTests = testGroup "MailAPI" [
      testCase "create proper document with one signatory" testSuccessfulDocCreation
    , testCase "fail if user doesn't exist" testFailureNoSuchUser
    ]

testSuccessfulDocCreation :: Assertion
testSuccessfulDocCreation = withTestEnvironment $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile "test/mailapi/email_onesig_ok.eml")]
    ctx <- (\c -> c { ctxdocstore = tmpdir }) <$> (mkContext =<< readTemplates LANG_EN)
    uid <- createTestUser
    _ <- update $ SetUserMailAPI uid $ Just UserMailAPI {
          umapiKey = read "ef545848bcd3f7d8"
        , umapiDailyLimit = 1
        , umapiSentToday = 0
        , umapiLastSentDate = asInt $ ctxtime ctx
    }
    (res, _) <- runTestKontra req ctx $ testAPI handleMailCommand
    successChecks $ jsonToStringList res

testFailureNoSuchUser :: Assertion
testFailureNoSuchUser = withTestEnvironment $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile "test/mailapi/email_onesig_ok.eml")]
    ctx <- (\c -> c { ctxdocstore = tmpdir }) <$> (mkContext =<< readTemplates LANG_EN)
    (res, _) <- first jsonToStringList <$> runTestKontra req ctx (testAPI handleMailCommand)
    assertBool "error occured" $ isJust $ lookup "error" res
    assertBool "message matches regex 'User .* not found'" $
        equalsKey (=~ "^User '.*' not found$") "error_message" res

successChecks :: [(String, String)] -> Assertion
successChecks res = do
    assertBool "status is success" $ equalsKey (== "success") "status" res
    assertBool "message matches 'Document #* created'" $
        equalsKey (=~ "^Document #[0-9]+ created$") "message" res
    let mdocid = lookup "documentid" res
    assertBool "documentid is given" $ isJust mdocid
    mdoc <- query $ GetDocumentByDocumentID $ read $ fromJust mdocid
    assertBool "document was really created" $ isJust mdoc
    let doc = fromJust mdoc
    assertBool "document has two signatories" $ length (documentsignatorylinks doc) == 2
    assertBool "document status is pending" $ documentstatus doc == Pending

equalsKey :: (a -> Bool) -> String -> [(String, a)] -> Bool
equalsKey f k list = (f <$> lookup k list) == Just True

jsonToStringList :: JSObject JSValue -> [(String, String)]
jsonToStringList = map (second f) . fromJSObject
    where
        f (JSString s) = fromJSString s
        f _ = ""

withTestEnvironment :: (FilePath -> Assertion) -> Assertion
withTestEnvironment = withTestState . withSystemTempDirectory "mailapi-test-"

createTestUser :: IO UserID
createTestUser = do
    Just User{userid} <- update $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") NoPassword Nothing Nothing Nothing
    return userid
