module DocControlTest(
    docControlTests
) where

import Control.Applicative
import Data.Maybe
import Data.List
import Happstack.Server
import Test.Framework
import TestingUtil
import TestKontra as T
import Mails.Model
import Utils.Default
import Context
import DB
import Doc.Model
import Doc.DocStateData
import Doc.DocControl
import Doc.DocUtils
import Company.Model
import User.Model
import Util.SignatoryLinkUtils
import Util.Actor
import Util.HasSomeUserInfo
import Doc.API

docControlTests :: TestEnvSt -> Test
docControlTests env = testGroup "Templates" [
    testThat "Sending a reminder updates last modified date on doc" env testSendReminderEmailUpdatesLastModifiedDate
  , testThat "Create document from template" env testDocumentFromTemplate
  , testThat "Uploading file as contract makes doc" env testUploadingFileAsContract
  , testThat "Create document from template | Shared" env testDocumentFromTemplateShared
  , testThat "Uploading file as offer makes doc" env testUploadingFileAsOffer
  , testThat "Uploading file as order makes doc" env testUploadingFileAsOrder
  , testThat "Sending document sends invites" env testSendingDocumentSendsInvites
  , testThat "Signing document from design view sends invites" env testSigningDocumentFromDesignViewSendsInvites
  , testThat "Person who isn't last signing a doc leaves it pending" env testNonLastPersonSigningADocumentRemainsPending
  , testThat "Last person signing a doc closes it" env testLastPersonSigningADocumentClosesIt
  , testThat "Sending an reminder clears delivery information" env testSendingReminderClearsDeliveryInformation
  , testThat "We can get json for document" env testGetLoggedIn
  , testThat "We can't get json for document if we are not logged in" env testGetNotLoggedIn
  , testThat "We can't get json for document is we are logged in but we provided authorization header" env testGetBadHeader
  
  ]
  
testUploadingFileAsContract :: TestEnv ()
testUploadingFileAsContract = do
  (user, rsp) <- uploadDocAsNewUser Contract
  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertBool "Document id in result json" ((show $ documentid newdoc) `isInfixOf` (show rsp))
  
testUploadingFileAsOffer :: TestEnv ()
testUploadingFileAsOffer = do
  (user, rsp) <- uploadDocAsNewUser Offer
  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertBool "Document id in result json" ((show $ documentid newdoc) `isInfixOf` (show rsp))
  
testUploadingFileAsOrder :: TestEnv ()
testUploadingFileAsOrder = do
  (user, rsp) <- uploadDocAsNewUser Order
  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertBool "Document id in result json" ((show $ documentid newdoc) `isInfixOf` (show rsp))

uploadDocAsNewUser :: DocumentProcess -> TestEnv (User, Response)
uploadDocAsNewUser doctype = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  req <- mkRequest POST [ ("type", inText (show $ Signable doctype))
                        , ("file", inFile "test/pdfs/simple.pdf") ]
  (rsp, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile
  return (user, rsp)

testSendingDocumentSendsInvites :: TestEnv ()
testSendingDocumentSendsInvites = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc <- addRandomDocumentWithAuthorAndCondition user (\d ->
       documentstatus d == Preparation
    && 2 <= length (filterSigLinksFor (signatoryispartner . signatorydetails) d)
    && case documenttype d of
          Signable _ -> True
          _ -> False
    && sendMailsDurringSigning d)

  req <- mkRequest POST [ ("send", inText "True")
                        -- this stuff is for updateDocument function, which I believe
                        -- is being deleted.
                        , ("docname", inText "Test Doc")
                        , ("allowedsignaturetypes", inText "Email")
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
  assertEqual "Should be pending" Pending (documentstatus sentdoc)
  emails <- dbQuery GetEmails
  assertBool "Emails sent" (length emails > 0)

testSigningDocumentFromDesignViewSendsInvites :: TestEnv ()
testSigningDocumentFromDesignViewSendsInvites = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc <- addRandomDocumentWithAuthorAndCondition user (\d ->
       documentstatus d == Preparation
    && case documenttype d of
        Signable Contract -> True
        _ -> False
    && isSignatory (getAuthorSigLink d)
    && 2 <= length (filterSigLinksFor (signatoryispartner . signatorydetails) d)
    && sendMailsDurringSigning d)

  req <- mkRequest POST [ ("sign", inText "True")
                        ]
  (_link, _ctx') <- runTestKontra req ctx $ handleIssueShowPost (documentid doc)

  Just sentdoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "Not in pending state" Pending (documentstatus sentdoc)
  emails <- dbQuery GetEmails
  assertBool "Emails sent" (length emails > 0)

testNonLastPersonSigningADocumentRemainsPending :: TestEnv ()
testNonLastPersonSigningADocumentRemainsPending = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc' <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Preparation
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False
                     && d `allowsAuthMethod` StandardAuthentication
                     && documentdeliverymethod d == EmailDelivery)

  let authorOnly sd = sd { signatoryisauthor = True, signatoryispartner = False }
  True <- randomUpdate $ ResetSignatoryDetails (documentid doc') ([
                   (authorOnly $ signatorydetails . fromJust $ getAuthorSigLink doc')
                 , (mkSigDetails "Fred" "Frog" "fred@frog.com" False True)
                 , (mkSigDetails "Gordon" "Gecko" "gord@geck.com" False True)
               ]) (systemActor $ documentctime doc')

  True <- randomUpdate $ PreparationToPending (documentid doc') (systemActor (documentctime doc'))
  Just doc'' <- dbQuery $ GetDocumentByDocumentID $ documentid doc'

  let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
      siglink = head $ filter isUnsigned (documentsignatorylinks doc'')
  True <- randomUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid siglink) (signatorymagichash siglink)
               (signatoryActor (documentctime doc') (ctxipnumber ctx) (maybesignatory siglink) (getEmail $ siglink) (signatorylinkid siglink))
  Just doc <- dbQuery $ GetDocumentByDocumentID $ documentid doc''

  assertEqual "Two left to sign" 2 (length $ filter isUnsigned (documentsignatorylinks doc))

  preq <- mkRequest GET [ ]
  (_,ctx') <- runTestKontra preq ctx $ handleSignShowSaveMagicHash (documentid doc) (signatorylinkid siglink) (signatorymagichash siglink)
  
  req <- mkRequest POST [ ("fields", inText "[]")]
  (_link, _ctx') <- runTestKontra req ctx' $ signDocument (documentid doc) (signatorylinkid siglink)
  Just signeddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In pending state" Pending (documentstatus signeddoc)
  assertEqual "One left to sign" 1 (length $ filter isUnsigned (documentsignatorylinks signeddoc))
  emails <- dbQuery GetEmails
  assertEqual "No email sent" 0 (length emails)

testLastPersonSigningADocumentClosesIt :: TestEnv ()
testLastPersonSigningADocumentClosesIt = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc' <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Preparation
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False
                     && d `allowsAuthMethod` StandardAuthentication
                     && documentdeliverymethod d == EmailDelivery)

  let authorOnly sd = sd { signatoryisauthor = True, signatoryispartner = False }
  True <- randomUpdate $ ResetSignatoryDetails (documentid doc') ([
                   (authorOnly $ signatorydetails . fromJust $ getAuthorSigLink doc')
                 , (mkSigDetails "Fred" "Frog" "fred@frog.com" False True)
               ]) (systemActor $ documentctime doc')


  True <- randomUpdate $ PreparationToPending (documentid doc') (systemActor (documentctime doc'))
  Just doc'' <- dbQuery $ GetDocumentByDocumentID $ documentid doc'

  let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
      siglink = head $ filter isUnsigned (documentsignatorylinks doc'')

  True <- randomUpdate $ MarkDocumentSeen (documentid doc') (signatorylinkid siglink) (signatorymagichash siglink)
               (signatoryActor (documentctime doc') (ctxipnumber ctx) (maybesignatory siglink) (getEmail siglink) (signatorylinkid siglink))
  Just doc <- dbQuery $ GetDocumentByDocumentID $ documentid doc''

  assertEqual "One left to sign" 1 (length $ filter isUnsigned (documentsignatorylinks doc))

  preq <- mkRequest GET [ ]
  (_,ctx') <- runTestKontra preq ctx $ handleSignShowSaveMagicHash (documentid doc) (signatorylinkid siglink) (signatorymagichash siglink)

  req <- mkRequest POST [ ("fields", inText "[]")]
  (_link, _ctx') <- runTestKontra req ctx' $ signDocument (documentid doc) (signatorylinkid siglink)

  Just signeddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "In closed state" Closed (documentstatus signeddoc)
  --TODO: this should be commented out really, I guess it's a bug
  --assertEqual "None left to sign" 0 (length $ filter isUnsigned (documentsignatorylinks doc))
  --emails <- dbQuery GetEmails
  --assertEqual "Confirmation email sent" 1 (length emails)

testSendReminderEmailUpdatesLastModifiedDate :: TestEnv ()
testSendReminderEmailUpdatesLastModifiedDate = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False)

  assertBool "Precondition" $ (ctxtime ctx) /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = head . reverse $ documentsignatorylinks doc
  req <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req ctx $ sendReminderEmail Nothing ctx (systemActor $ ctxtime ctx) doc sl

  Just updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "Modified date is updated" (ctxtime ctx) (documentmtime updateddoc)


testSendingReminderClearsDeliveryInformation :: TestEnv ()
testSendingReminderClearsDeliveryInformation = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext (mkLocaleFromRegion defaultValue)

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable _ -> True
                         _ -> False)
  let sl = head . reverse $ documentsignatorylinks doc
      actor  =  systemActor $ ctxtime ctx
  _ <- dbUpdate $ MarkInvitationRead (documentid doc) (signatorylinkid sl) actor
  -- who cares which one, just pick the last one
  req <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req ctx $ sendReminderEmail Nothing ctx actor doc sl
  Just updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  let (Just sl') = find (\t -> signatorylinkid t == signatorylinkid sl) (documentsignatorylinks updateddoc)
  assertEqual "Invitation is not delivered" (Unknown) (invitationdeliverystatus sl')

  
testDocumentFromTemplate :: TestEnv ()
testDocumentFromTemplate = do
    (Just user) <- addNewUser "aaa" "bbb" "xxx@xxx.pl"
    doc <- addRandomDocumentWithAuthorAndCondition user (\d -> case documenttype d of
                                                            Template _ -> True
                                                            _ -> False)
    docs1 <- randomQuery $ GetDocumentsByAuthor (userid user)
    ctx <- (\c -> c { ctxmaybeuser = Just user })
      <$> mkContext (mkLocaleFromRegion defaultValue)
    req <- mkRequest POST []
    _ <- runTestKontra req ctx $ apiCallCreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertBool "No new document" (length docs2 == 1+ length docs1)

testDocumentFromTemplateShared :: TestEnv ()
testDocumentFromTemplateShared = do
    (Company {companyid}) <- addNewCompany
    (Just author) <- addNewCompanyUser "aaa" "bbb" "xxx@xxx.pl" companyid
    doc <- addRandomDocumentWithAuthorAndCondition author (\d -> case documenttype d of
                                                            Template _ -> True
                                                            _ -> False)
    _ <- randomUpdate $ SetDocumentSharing [documentid doc] True
    (Just user) <- addNewCompanyUser "ccc" "ddd" "zzz@zzz.pl" companyid
    docs1 <- randomQuery $ GetDocumentsByAuthor (userid user)
    ctx <- (\c -> c { ctxmaybeuser = Just user })
      <$> mkContext (mkLocaleFromRegion defaultValue)
    req <- mkRequest POST []
    _ <- runTestKontra req ctx $ apiCallCreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertBool "New document should have been created" (length docs2 == 1+ length docs1)


testGetLoggedIn :: TestEnv ()
testGetLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext (mkLocaleFromRegion defaultValue)
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 200" 200 (rsCode res)

   
testGetNotLoggedIn :: TestEnv ()
testGetNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext (mkLocaleFromRegion defaultValue)
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 401" 401 (rsCode res)


testGetBadHeader :: TestEnv ()
testGetBadHeader = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext (mkLocaleFromRegion defaultValue)
  req <- mkRequestWithHeaders GET [] [("authorization", ["ABC"])]
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 401" 401 (rsCode res)
  

    
mkSigDetails :: String -> String -> String -> Bool -> Bool -> SignatoryDetails
mkSigDetails fstname sndname email isauthor ispartner = SignatoryDetails {
    signatorysignorder = SignOrder 1
  , signatoryfields = [
      toSF FirstNameFT fstname
    , toSF LastNameFT sndname
    , toSF EmailFT email
    ]
  , signatoryisauthor = isauthor
  , signatoryispartner = ispartner
  }
  where
    toSF t v = SignatoryField {
        sfType = t
      , sfValue = v
      , sfPlacements = []
    }
