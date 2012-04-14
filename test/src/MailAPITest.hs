module MailAPITest (mailApiTests) where

import Control.Applicative
import Data.Maybe
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion)
import qualified Data.ByteString.UTF8 as BS
import qualified Codec.MIME.Type as MIME
import Doc.DocStateData
import Data.List

import Crypto.RNG
import DB.Classes
import MagicHash
import Context
import Doc.Model
import Misc
import User.Model
import TestingUtil
import Templates.TemplatesLoader
import TestKontra as T
import Doc.DocInfo
import qualified Log

import ScriveByMail.Control
import ScriveByMail.Model
import ScriveByMail.Action

mailApiTests :: (Nexus, CryptoRNGState) -> Test
mailApiTests env = testGroup "MailAPI" [
      testThat "GetUserMailAPI/SetUserMailAPI works" env test_getUserMailAPI
    , testThat "Create simple email document with one signatory"      env $ testSuccessfulDocCreation "test/mailapi/email_simple_onesig.eml" 2
    , testCase "Parse mime document email_onesig_ok.eml"              $ testParseMimes "test/mailapi/email_onesig_ok.eml"
    , testCase "Parse mime document email_simple_onesig.eml"          $ testParseMimes "test/mailapi/email_simple_onesig.eml"
    , testCase "Parse mime document email_outlook_three.eml"          $ testParseMimes "test/mailapi/email_outlook_three.eml"
    , testCase "Parse mime document email_outlook_viktor.eml"         $ testParseMimes "test/mailapi/email_outlook_viktor.eml"
    , testCase "Parse mime document email_gmail_eric.eml"             $ testParseMimes "test/mailapi/email_gmail_eric.eml"
    , testThat "Create outlook email document with three signatories" env $ testSuccessfulDocCreation "test/mailapi/email_outlook_three.eml" 4
    , testThat "test lukas's error email"                             env $ testSuccessfulDocCreation "test/mailapi/lukas_mail_error.eml" 2
    , testThat "test eric's error email"                              env $ testSuccessfulDocCreation "test/mailapi/eric_email_error.eml" 2
    , testThat "test lukas's funny title"                             env $ testSuccessfulDocCreation "test/mailapi/email_weird_subject.eml" 2
    , testThat "test exchange email"                                  env $ testSuccessfulDocCreation "test/mailapi/email_exchange.eml" 2
    , testThat "test 2 sig model from outlook mac"                    env $ testSuccessfulDocCreation "test/mailapi/email_outlook_viktor.eml" 3
    , testThat "test json with 2 sigs"                                env $ testSuccessfulDocCreation "test/mailapi/email_onesig_json.eml" 2
    , testThat "test json with 2 sigs (from Roger)"                   env $ testSuccessfulDocCreation "test/mailapi/roger_json.eml" 2
    , testThat "test json with 2 sigs (from Roger) with decoding"     env $ testSuccessfulDocCreation "test/mailapi/roger_decode_json.eml" 2
    ]

testParseMimes :: String -> Assertion
testParseMimes mimepath = do
  cont <- readFile mimepath
  let (_mime, allParts) = parseEmailMessageToParts $ BS.fromString cont
      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf"
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"
      --typesOfParts = map fst allParts
      pdfs = filter isPDF allParts
      plains = filter isPlain allParts
  assertBool ("Should be exactly one pdf, found " ++ show pdfs) (length pdfs == 1)
  assertBool ("Should be exactly one plaintext, found " ++ show plains) (length plains == 1)

testSuccessfulDocCreation :: String -> Int -> TestEnv ()
testSuccessfulDocCreation emlfile sigs = withSystemTempDirectory' "mailapi-test-" $ \tmpdir -> do
    req <- mkRequest POST [("mail", inFile emlfile)]
    uid <- createTestUser
    muser <- dbQuery $ GetUserByID uid
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdocstore = tmpdir, ctxmaybeuser = muser })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    _ <- dbUpdate $ SetUserMailAPIKey uid (read "ef545848bcd3f7d8") 1
    (res, _) <- runTestKontra req ctx handleMailAPI

    Log.debug $ "Here's what I got back from handleMailCommand: " ++ show res
    let mdocid = maybeRead res
    assertBool ("documentid is not given: " ++ show mdocid) $ isJust mdocid
    mdoc <- dbQuery $ GetDocumentByDocumentID $ fromJust mdocid
    assertBool "document was really created" $ isJust mdoc
    let doc = fromJust mdoc
    assertBool ("document should have " ++ show sigs ++ " signatories has " ++ show (length (documentsignatorylinks doc)) ++": " ++ show (documentsignatorylinks doc)) $ length (documentsignatorylinks doc) == sigs

    assertBool ("document status should be pending, is " ++ show (documentstatus doc)) $ documentstatus doc == Pending
    assertBool "document has file no attached" $ (length $ documentfiles doc) == 1
    assertBool ("doc has iso encoded title " ++ show (documenttitle doc)) $ not $ "=?iso" `isInfixOf` (documenttitle doc)
    Just doc' <- dbQuery $ GetDocumentByDocumentID $ fromJust mdocid
    assertBool "document is in error!" $ not $ isDocumentError doc'

createTestUser :: TestEnv UserID
createTestUser = do
    Just User{userid} <- dbUpdate $ AddUser ("", "") "andrzej@skrivapa.se" Nothing False Nothing Nothing (mkLocaleFromRegion defaultValue)
    return userid

test_getUserMailAPI :: TestEnv ()
test_getUserMailAPI = do
  Just User{userid} <- addNewUser "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  let mapi = MailAPIInfo {
      umapiKey = unsafeMagicHash 0
    , umapiDailyLimit = 1
    , umapiSentToday = 0
  }
  res <- dbUpdate $ SetUserMailAPIKey userid (umapiKey mapi) (umapiDailyLimit mapi)
  assertBool "UserMailAPI created correctly" res
  Just mapi2 <- dbQuery $ GetUserMailAPI userid
  assertBool "Correct UserMailAPI returned" $ mapi == mapi2
  res2 <- dbUpdate $ IncrementUserMailAPI userid
  assertBool "UserMailAPI updated correctly" (res2 == Just 1)
  Just mapi3 <- dbQuery $ GetUserMailAPI userid
  assertBool "Correct updated UserMailAPI returned" $ umapiSentToday mapi3 == 1
  res3 <- dbUpdate $ RemoveUserMailAPI userid
  assertBool "UserMailAPI erased correctly" res3
  nomapi <- dbQuery $ GetUserMailAPI userid
  assertBool "No UserMailAPI returned" $ isNothing nomapi

