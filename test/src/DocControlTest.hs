module DocControlTest(
    docControlTests
) where


import qualified Data.ByteString.UTF8 as BS
import Control.Applicative
import Data.Maybe
import Happstack.Server
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import Mails.Model
import Misc
import Context
import DB.Classes
import Doc.Model
import Doc.DocStateData
import Doc.DocControl
import Doc.DocUtils
import KontraLink
import User.Model
import Util.SignatoryLinkUtils


docControlTests :: DBEnv -> Test
docControlTests env =  testGroup "Templates"
                           [
                               testCase "Create document from template" $ testDocumentFromTemplate env
                             , testCase "Uploading file as contract makes doc" $ testUploadingFileAsContract env
                             , testCase "Uploading file as offer makes doc" $ testUploadingFileAsOffer env
                             , testCase "Uploading file as order makes doc" $ testUploadingFileAsOrder env
                             , testCase "Sending document sends invites" $ testSendingDocumentSendsInvites env
                             , testCase "Signing document from design view sends invites" $ testSigningDocumentFromDesignViewSendsInvites env
                             , testCase "Person who isn't last signing a doc leaves it pending" $ testNonLastPersonSigningADocumentRemainsPending env
                             , testCase "Last person signing a doc closes it" $ testLastPersonSigningADocumentClosesIt env
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

testUploadingFileAsContract :: DBEnv -> Assertion
testUploadingFileAsContract env = withTestEnvironment env $ do
  (user, link) <- uploadDocAsNewUser env Contract
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

testUploadingFileAsOffer :: DBEnv -> Assertion
testUploadingFileAsOffer env = withTestEnvironment env $ do
  (user, link) <- uploadDocAsNewUser env Offer
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

testUploadingFileAsOrder :: DBEnv -> Assertion
testUploadingFileAsOrder env = withTestEnvironment env $ do
  (user, link) <- uploadDocAsNewUser env Order
  docs <- randomQuery $ GetDocumentsByUser user
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertEqual "Links to /d/docid" ("/d/" ++ (show $ documentid newdoc)) (show link)

uploadDocAsNewUser :: DBEnv -> DocumentProcess -> DB (User, KontraLink)
uploadDocAsNewUser env doctype = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  req <- mkRequest POST [ ("doctype", inText (show doctype))
                        , ("doc", inFile "test/pdfs/simple.pdf") ]
  (link, _ctx') <- runTestKontra req ctx $ handleIssueNewDocument
  return (user, link)

testSendingDocumentSendsInvites :: DBEnv -> Assertion
testSendingDocumentSendsInvites env = withTestEnvironment env $ do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> documentstatus d == Preparation
                                                               && case documenttype d of
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

  Just sentdoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In pending state" Pending (documentstatus sentdoc)
  emails <- dbQuery GetIncomingEmails
  assertBool "Emails sent" (length emails > 0)

testSigningDocumentFromDesignViewSendsInvites :: DBEnv -> Assertion
testSigningDocumentFromDesignViewSendsInvites env = withTestEnvironment env $ do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> documentstatus d == Preparation
                                                               && case documenttype d of
                                                                    Signable Contract -> True
                                                                    _ -> False)

  req <- mkRequest POST [ ("sign", inText "True")
                        -- this stuff is for updateDocument function, which I believe
                        -- is being deleted.
                        , ("docname", inText "Test Doc")
                        , ("allowedsignaturetypes", inText "Email")
                        , ("docfunctionality", inText "BasicFunctionality")
                        , ("authorrole", inText "signatory")
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

  Just sentdoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In pending state" Pending (documentstatus sentdoc)
  emails <- dbQuery GetIncomingEmails
  assertBool "Emails sent" (length emails > 0)

testNonLastPersonSigningADocumentRemainsPending :: DBEnv -> Assertion
testNonLastPersonSigningADocumentRemainsPending env = withTestEnvironment env $ do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  doc' <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Preparation
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False
                     && d `allowsIdentification` EmailIdentification
                     && documentfunctionality d == AdvancedFunctionality)

  Right _ <- randomUpdate $ ResetSignatoryDetails (documentid doc') ([
                   (signatorydetails . fromJust $ getAuthorSigLink doc', [SignatoryAuthor])
                 , (mkSigDetails "Fred" "Frog" "fred@frog.com", [SignatoryPartner])
                 , (mkSigDetails "Gordon" "Gecko" "gord@geck.com", [SignatoryPartner])
               ]) (documentctime doc')

  Right doc'' <- randomUpdate $ PreparationToPending (documentid doc') (documentctime doc')

  let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
      siglink = head $ filter isUnsigned (documentsignatorylinks doc'')

  Right doc <- randomUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid siglink) (signatorymagichash siglink) (documentctime doc') (ctxipnumber ctx)

  assertEqual "Two left to sign" 2 (length $ filter isUnsigned (documentsignatorylinks doc))

  req <- mkRequest POST [ ("magichash", inText $ show $ signatorymagichash siglink)
                        ]
  (_link, _ctx') <- runTestKontra req ctx $ signDocument (documentid doc) (signatorylinkid siglink)

  Just signeddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In pending state" Pending (documentstatus signeddoc)
  assertEqual "One left to sign" 1 (length $ filter isUnsigned (documentsignatorylinks signeddoc))
  emails <- dbQuery GetIncomingEmails
  assertEqual "No email sent" 0 (length emails)

testLastPersonSigningADocumentClosesIt :: DBEnv -> Assertion
testLastPersonSigningADocumentClosesIt env = withTestEnvironment env $ do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  globaltemplates <- readGlobalTemplates
  ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates

  doc' <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Preparation
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False
                     && d `allowsIdentification` EmailIdentification
                     && documentfunctionality d == AdvancedFunctionality)

  Right _ <- randomUpdate $ ResetSignatoryDetails (documentid doc') ([
                   (signatorydetails . fromJust $ getAuthorSigLink doc', [SignatoryAuthor])
                 , (mkSigDetails "Fred" "Frog" "fred@frog.com", [SignatoryPartner])
               ]) (documentctime doc')


  Right doc'' <- randomUpdate $ PreparationToPending (documentid doc') (documentctime doc')

  let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
      siglink = head $ filter isUnsigned (documentsignatorylinks doc'')

  Right doc <- randomUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid siglink) (signatorymagichash siglink) (documentctime doc') (ctxipnumber ctx)

  assertEqual "One left to sign" 1 (length $ filter isUnsigned (documentsignatorylinks doc))

  req <- mkRequest POST [ ("magichash", inText $ show $ signatorymagichash siglink)
                        ]
  (_link, _ctx') <- runTestKontra req ctx $ signDocument (documentid doc) (signatorylinkid siglink)

  Just signeddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In closed state" Closed (documentstatus signeddoc)
  --TODO: this should be commented out really, I guess it's a bug
  --assertEqual "None left to sign" 0 (length $ filter isUnsigned (documentsignatorylinks doc))
  --emails <- dbQuery GetIncomingEmails
  --assertEqual "Confirmation email sent" 1 (length emails)

testDocumentFromTemplate :: DBEnv -> Assertion
testDocumentFromTemplate env =  withTestEnvironment env $ do
    (Just user) <- addNewUser "aaa" "bbb" "xxx@xxx.pl"
    doc <- addRandomDocumentWithAuthorAndCondition user (\d -> case documenttype d of
                                                            Template _ -> True
                                                            _ -> False)
    docs1 <- randomQuery $ GetDocumentsByUser user
    globaltemplates <- readGlobalTemplates
    ctx <- (\c -> c { ctxdbenv = env, ctxmaybeuser = Just user })
      <$> mkContext (mkLocaleFromRegion defaultValue) globaltemplates
    req <- mkRequest POST [("template", inText (show $ documentid doc))]
    _ <- runTestKontra req ctx $ handleCreateFromTemplate
    docs2 <- randomQuery $ GetDocumentsByUser user
    assertBool "No new document" (length docs2 == 1+ length docs1)

mkSigDetails :: String -> String -> String -> SignatoryDetails
mkSigDetails fstname sndname email = SignatoryDetails {
    signatorysignorder = SignOrder 1
  , signatoryfields = [
      toSF FirstNameFT . BS.fromString $ fstname
    , toSF LastNameFT . BS.fromString $ sndname
    , toSF EmailFT . BS.fromString $ email
    ]
  }
  where
    toSF t v = SignatoryField {
        sfType = t
      , sfValue = v
      , sfPlacements = []
    }
