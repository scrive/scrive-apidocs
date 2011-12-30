module DocControlTest(
    docControlTests
) where

import Control.Applicative
import Happstack.Server
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import Misc
import Context
import Database.HDBC.PostgreSQL
import Doc.Transitory
import Doc.DocStateData
import Doc.DocControl
import Company.Model
import User.Model
import KontraLink
import DB.Classes


docControlTests :: Connection -> Test
docControlTests conn =  testGroup "Templates"
                           [
                               testCase "Create document from template" $ testDocumentFromTemplate conn
                             , testCase "Create document from template | Shared" $ testDocumentFromTemplateShared conn
                             , testCase "Uploading file as contract makes doc" $ testUploadingFileAsContract conn
                             , testCase "Uploading file as offer makes doc" $ testUploadingFileAsOffer conn
                             , testCase "Uploading file as order makes doc" $ testUploadingFileAsOrder conn
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
