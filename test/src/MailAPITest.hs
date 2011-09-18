module MailAPITest (mailApiTests) where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Happstack.Server
import Happstack.State hiding (Method)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import Text.JSON.Generic
import Text.Regex.TDFA ((=~))
import System.IO.Temp
import qualified Data.ByteString.Char8 as BS

import API.MailAPI
import DB.Classes
import Context
import Doc.DocState
import Misc
import StateHelper
import User.Model
import TestingUtil
import Templates.TemplatesLoader
import TestKontra as T

mailApiTests :: Connection -> Test
mailApiTests conn = testGroup "MailAPI" [
      testCase "create proper document with one signatory" $ testSuccessfulDocCreation conn
    , testCase "fail if user doesn't exist" $ testFailureNoSuchUser conn
    , testCase "Create simple email document with one signatory" $ testSuccessfulSimpleEmailDocCreation conn
    ]

testSuccessfulDocCreation :: Connection -> Assertion
testSuccessfulDocCreation conn = withMyTestEnvironment conn $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile "test/mailapi/email_onesig_ok.eml")]
    uid <- createTestUser
    muser <- dbQuery $ GetUserByID uid
    ctx <- (\c -> c { ctxdbconn = conn, ctxdocstore = tmpdir, ctxmaybeuser = muser })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    _ <- dbUpdate $ SetUserMailAPI uid $ Just UserMailAPI {
          umapiKey = read "ef545848bcd3f7d8"
        , umapiDailyLimit = 1
        , umapiSentToday = 0
    }
    (res, _) <- runTestKontra req ctx $ testAPI handleMailCommand
    wrapDB rollback
    successChecks $ jsonToStringList res

testSuccessfulSimpleEmailDocCreation :: Connection -> Assertion
testSuccessfulSimpleEmailDocCreation conn = withMyTestEnvironment conn $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile "test/mailapi/email_simple_onesig.eml")]
    uid <- createTestUser
    muser <- dbQuery $ GetUserByID uid
    ctx <- (\c -> c { ctxdbconn = conn, ctxdocstore = tmpdir, ctxmaybeuser = muser })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    _ <- dbUpdate $ SetUserMailAPI uid $ Just UserMailAPI {
          umapiKey = read "ef545848bcd3f7d8"
        , umapiDailyLimit = 1
        , umapiSentToday = 0
    }
    (res, _) <- runTestKontra req ctx $ testAPI handleMailCommand
    wrapDB rollback
    successChecks $ jsonToStringList res


testFailureNoSuchUser :: Connection -> Assertion
testFailureNoSuchUser conn = withMyTestEnvironment conn $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile "test/mailapi/email_onesig_ok.eml")]
    ctx <- (\c -> c { ctxdbconn = conn, ctxdocstore = tmpdir })
      <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    (res, _) <- first jsonToStringList <$> runTestKontra req ctx (testAPI handleMailCommand)
    assertBool "error occured" $ isJust $ lookup "error" res
    assertBool "message matches regex 'User .* not found'" $
        equalsKey (=~ "^User '.*' not found$") "error_message" res

successChecks :: [(String, String)] -> DB ()
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
jsonToStringList = (mapSnd toString) . fromJSObject
    where
        toString (JSString s) = fromJSString s
        toString _ = error "Pattern not matched -> Waiting for JSON string, but other structure found"

withMyTestEnvironment :: Connection -> (FilePath -> DB ()) -> Assertion
withMyTestEnvironment conn f = 
  withSystemTempDirectory "mailapi-test-" (\d -> withTestEnvironment conn (f d))

createTestUser :: DB UserID
createTestUser = do
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") Nothing False Nothing Nothing defaultValue
    return userid
