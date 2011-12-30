module DocControlTest(
    docControlTests
) where

import Control.Applicative
import Control.Monad.State
import Happstack.Server
import Happstack.State (query)
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import MinutesTime
import Misc
import Context
import ActionSchedulerState
import Database.HDBC.PostgreSQL
import DB.Classes
import Doc.Transitory
import Doc.DocStateData
import Doc.DocControl
import Company.Model
import KontraLink
import User.Model


docControlTests :: Connection -> Test
docControlTests conn =  testGroup "Templates"
                           [
                               testCase "Create document from template" $ testDocumentFromTemplate conn
                             , testCase "Create document from template | Shared" $ testDocumentFromTemplateShared conn
                             , testCase "Uploading file as contract makes doc" $ testUploadingFileAsContract conn
                             , testCase "Uploading file as offer makes doc" $ testUploadingFileAsOffer conn
                             , testCase "Uploading file as order makes doc" $ testUploadingFileAsOrder conn
                             , testCase "Sending document sends invites" $ testSendingDocumentSendsInvites conn
                           ]

{-
                     [testGroup "sendDocumentErrorEmail1"
                           [
                             testCase "sends one mail" test_sendDocumentErrorEmail1_sendsOneMail
                           ]
                     ]

test_sendDocumentErrorEmail1_sendsOneMail = do
  counter <- newIORef 0
  let ctx = aTestCtx{ctxmailer=countingMailer counter}
      doc = anUnsignedDocument
      siglink = head $ documentsignatorylinks doc
  sendDocumentErrorEmail1 ctx doc siglink
  numberSent <- readIORef counter
  assertEqual "for mail count" 1 numberSent
    where countMail _ = return ()

countingMailer counter mail = do
    modifyIORef counter $ (+) 1

 -}

testUploadingFileAsContract :: Connection -> Assertion
testUploadingFileAsContract conn = withTestEnvironment conn $ do
  (user, link) <- uploadDocAsNewUser conn Contract
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

testUploadingFileAsOffer :: Connection -> Assertion
testUploadingFileAsOffer conn = withTestEnvironment conn $ do
  (user, link) <- uploadDocAsNewUser conn Offer
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

testUploadingFileAsOrder :: Connection -> Assertion
testUploadingFileAsOrder conn = withTestEnvironment conn $ do
  (user, link) <- uploadDocAsNewUser conn Order
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

uploadDocAsNewUser :: Connection -> DocumentProcess -> DB (User, KontraLink)
uploadDocAsNewUser conn doctype = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("doctype", inText (show doctype))
                        , ("doc", inFile "test/pdfs/simple.pdf") ]
  (link, _ctx') <- runTestKontra req ctx $ handleIssueNewDocument
  return (user, link)

testSendingDocumentSendsInvites :: Connection -> Assertion
testSendingDocumentSendsInvites conn = withTestEnvironment conn $ do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> case documenttype d of
                                                               Signable _ -> True
                                                               _ -> False)

  req <- mkRequest POST [ ("final", inText "True")
                        -- this stuff is for updateDocument function, which I believe
                        -- is being deleted.
                        , ("docname", inText "Test Doc")
                        , ("allowedsignaturetypes", inText "Email")
                        , ("docfunctionality", inText "BasicFunctionality")
                        , ("authorrole", inText "secretary")
                        , ("signatoryrole", inText "signatory")
                        , ("sigid", inText "EDF92AA6-3595-451D-B5D1-04C823A616FF")
                        , ("signatoryfstname", inText "Fred")
                        , ("signatorysndname", inText "Frog")
                        , ("signatorycompany", inText "")
                        , ("signatorypersonalnumber", inText "")
                        , ("signatorycompanynumber", inText "")
                        , ("signatoriessignorder", inText "1")
                        , ("signatoryemail", inText "fred@frog.com")
                        , ("signatoryrole", inText "signatory")
                        ]
  (_link, _ctx') <- runTestKontra req ctx $ handleIssueShowPost (documentid doc)

  Just sentdoc <- doc_query' $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In pending state" Pending (documentstatus sentdoc)
  emails <- getEmailActions
  assertBool "Emails sent" (length emails > 0)

testDocumentFromTemplate :: Connection -> Assertion
testDocumentFromTemplate conn =  withTestEnvironment conn $ do
    (Just user) <- addNewUser "aaa" "bbb" "xxx@xxx.pl"
    doc <- addRandomDocumentWithAuthorAndCondition user (\d -> case documenttype d of
                                                            Template _ -> True
                                                            _ -> False)
    docs1 <- randomQuery $ GetDocumentsByUser user
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("template", inText (show $ documentid doc))]
    _ <- runTestKontra req ctx $ handleCreateFromTemplate
    docs2 <- randomQuery $ GetDocumentsByUser user
    assertBool "No new document" (length docs2 == 1+ length docs1)

testDocumentFromTemplateShared :: Connection -> Assertion
testDocumentFromTemplateShared conn = withTestEnvironment conn $ do
    (Company {companyid}) <- addNewCompany
    (Just author) <- addNewCompanyUser "aaa" "bbb" "xxx@xxx.pl" companyid
    doc <- addRandomDocumentWithAuthorAndCondition author (\d -> case documenttype d of
                                                            Template _ -> True
                                                            _ -> False)
    _ <- randomUpdate $ ShareDocument $ documentid doc
    (Just user) <- addNewCompanyUser "ccc" "ddd" "zzz@zzz.pl" companyid
    docs1 <- randomQuery $ GetDocumentsByUser user
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbconn = conn, ctxmaybeuser = Just user })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("template", inText (show $ documentid doc))]
    _ <- runTestKontra req ctx $ handleCreateFromTemplate
    docs2 <- randomQuery $ GetDocumentsByUser user
    assertBool "No new document" (length docs2 == 1+ length docs1)

getEmailActions :: MonadIO m => m [Action]
getEmailActions = do
  now <- getMinutesTime
  let expirytime = 1 `minutesAfter` now
  allactions <- query $ GetExpiredActions EmailSendoutAction expirytime
  return $ filter isEmailAction allactions

isEmailAction :: Action -> Bool
isEmailAction action =
  case actionType action of
    (EmailSendout _) -> True
    _ -> False
