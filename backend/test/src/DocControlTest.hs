module DocControlTest (docControlTests) where

import Control.Monad.Trans
import Crypto.RNG
import Data.Bifunctor
import Happstack.Server
import Test.Framework
import Text.JSON.Gen (toJSValue)
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Map as M
import qualified Text.JSON

import Archive.Control
import BrandedDomain.BrandedDomain
import Branding.Control
import Branding.CSS
import Context
import DB
import DB.TimeZoneName (mkTimeZoneName)
import Doc.API.V1.Calls
import Doc.DocControl
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentMonad (theDocument, updateDocumentWithID, withDocument, withDocumentID, withDocumentM)
import Doc.Model
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryScreenshots (SignatoryScreenshots(signing), emptySignatoryScreenshots)
import Doc.SMSPin.Model
import Doc.Tokens.Model
import File.FileID
import File.Storage
import KontraError
import MagicHash
import Mails.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Model
import UserGroup.Data
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

docControlTests :: TestEnvSt -> Test
docControlTests env = testGroup "DocControl" [
    testThat "Sending a reminder updates last modified date on doc" env testSendReminderEmailUpdatesLastModifiedDate
  , testThat "Create document from template" env testDocumentFromTemplate
  , testThat "Uploading file as contract makes doc" env testUploadingFile
  , testThat "Create document from template | Shared" env testDocumentFromTemplateShared
  , testThat "Uploading file creates unsaved draft" env testNewDocumentUnsavedDraft
  , testThat "Last person signing a doc closes it" env testLastPersonSigningADocumentClosesIt
  , testThat "Signing with pin" env testSigningWithPin
  , testThat "Sending an reminder clears delivery information" env testSendingReminderClearsDeliveryInformation
  , testThat "Sending reminder email works for company admin" env testSendReminderEmailByCompanyAdmin
  , testThat "We can get json for document" env testGetLoggedIn
  , testThat "We can't get json for document if we are not logged in" env testGetNotLoggedIn
  , testThat "We can't get json for document is we are logged in but we provided authorization header" env testGetBadHeader
  , testThat "We can get json for evidence attachments" env testGetEvidenceAttachmentsLoggedIn
  , testThat "We can't get json for evidence attachments if we are not logged in" env testGetEvidenceAttachmentsNotLoggedIn
  , testThat "Document bulk delete works fast" env testDocumentDeleteInBulk
  , testThat "Download file and download main file obey access rights" env testDownloadFile
  , testThat "Download file and download main file blocked if authorization to view is needed" env testDownloadFileWithAuthToView

  , testThat "Signview branding generation block nasty input " env testSignviewBrandingBlocksNastyInput
  , testThat "We can download signview branding if we have access to document" env testDownloadSignviewBrandingAccess

  , testThat "We can't get a document as a signatory if it has been cancelled" env testGetCancelledDocument
  , testThat "Generate document to sign from shareable template"
             env testDocumentFromShareableTemplate
  ]

testUploadingFile :: TestEnv ()
testUploadingFile = do
  (user, rsp) <- uploadDocAsNewUser
  docs <- randomQuery $ GetDocumentsByAuthor (userid user)
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertBool "Document id in result json" ((show $ documentid newdoc) `isInfixOf` (show rsp))

testNewDocumentUnsavedDraft :: TestEnv ()
testNewDocumentUnsavedDraft = do
  (user, _rsp) <- uploadDocAsNewUser
  docs <- randomQuery $ GetDocuments (DocumentsVisibleToUser $ userid user) [DocumentFilterDeleted False] [] maxBound
  assertEqual "Draft is there" 1 (length docs)
  docs' <- randomQuery $ GetDocuments (DocumentsVisibleToUser $ userid user) [DocumentFilterUnsavedDraft False, DocumentFilterDeleted False] [] maxBound
  assertEqual "Draft is not visible in archive" 0 (length docs')



uploadDocAsNewUser :: TestEnv (User, Response)
uploadDocAsNewUser = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  req <- mkRequest POST [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (rsp, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile
  return (user, rsp)


signScreenshots :: (String, Input)
signScreenshots = ("screenshots", inText $ Text.JSON.encode $ toJSValue $
                   emptySignatoryScreenshots { signing = s })
  where s = Just $ Screenshot unixEpoch $ "\255\216\255\224\NUL\DLEJFIF\NUL\SOH\SOH\SOH\NULH\NULH\NUL\NUL\255\219\NULC\NUL\ETX\STX\STX\STX\STX\STX\ETX\STX\STX\STX\ETX\ETX\ETX\ETX\EOT\ACK\EOT\EOT\EOT\EOT\EOT\b\ACK\ACK\ENQ\ACK\t\b\n\n\t\b\t\t\n\f\SI\f\n\v\SO\v\t\t\r\DC1\r\SO\SI\DLE\DLE\DC1\DLE\n\f\DC2\DC3\DC2\DLE\DC3\SI\DLE\DLE\DLE\255\201\NUL\v\b\NUL\SOH\NUL\SOH\SOH\SOH\DC1\NUL\255\204\NUL\ACK\NUL\DLE\DLE\ENQ\255\218\NUL\b\SOH\SOH\NUL\NUL?\NUL\210\207 \255\217"

testLastPersonSigningADocumentClosesIt :: TestEnv ()
testLastPersonSigningADocumentClosesIt = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- saveNewFile filename filecontent

  addRandomDocumentWithAuthorAndConditionAndFile
            user
            (\d -> documentstatus d == Preparation
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))
            file `withDocumentM` do

    True <- do d <- theDocument
               randomUpdate $ ResetSignatoryDetails ([
                      (def {   signatoryfields = (signatoryfields $ fromJust $ getAuthorSigLink d)
                                      , signatoryisauthor = True
                                      , signatoryispartner = False
                                      , maybesignatory = Just $ userid user })
                    , (def {   signatorysignorder = SignOrder 1
                                      , signatoryisauthor = False
                                      , signatoryispartner = True
                                      , signatoryfields = [
                                          fieldForTests (NameFI (NameOrder 1)) "Fred"
                                        , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                        , fieldForTests EmailFI "fred@frog.com"
                                        ]})
                 ]) (systemActor $ documentctime d)


    do t <- documentctime <$> theDocument
       tz <- mkTimeZoneName "Europe/Stockholm"
       randomUpdate $ PreparationToPending (systemActor t) tz
    let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
    siglink <- head . filter isUnsigned .documentsignatorylinks <$> theDocument

    do t <- documentctime <$> theDocument
       randomUpdate . MarkDocumentSeen (signatorylinkid siglink) (signatorymagichash siglink)
                 =<< signatoryActor (set ctxtime t ctx) siglink

    assertEqual "One left to sign" 1 . length . filter isUnsigned . documentsignatorylinks =<< theDocument

    preq <- mkRequest GET [ ]
    (_,ctx') <- updateDocumentWithID $ \did ->
                lift . runTestKontra preq ctx $ handleSignShowSaveMagicHash did (signatorylinkid siglink) (signatorymagichash siglink)

    req <- mkRequest POST [ ("fields", inText "[]"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
                      lift . runTestKontra req ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "In closed state" Closed .documentstatus =<< theDocument
    --TODO: this should be commented out really, I guess it's a bug
    --assertEqual "None left to sign" 0 (length $ filter isUnsigned (documentsignatorylinks doc))
    --emails <- dbQuery GetEmails
    --assertEqual "Confirmation email sent" 1 (length emails)


testSigningWithPin :: TestEnv ()
testSigningWithPin = do
  ugid1 <- (get ugID) <$> addNewUserGroup
  ugid2 <- (get ugID) <$> addNewUserGroup
  Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
  Just user2 <- addNewUser "Gary" "Green" "gary@green.com"
  True <- dbUpdate $ SetUserUserGroup (userid user1) ugid1
  True <- dbUpdate $ SetUserUserGroup (userid user2) ugid2
  ctx <- (set ctxmaybeuser (Just user1)) <$> mkContext def

  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- saveNewFile filename filecontent

  addRandomDocumentWithAuthorAndConditionAndFile
            user1
            (\d -> documentstatus d == Preparation
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))
            file `withDocumentM` do
    d <- theDocument
    True <- randomUpdate $ ResetSignatoryDetails ([
        (def {
            signatoryfields = signatoryfields $ fromJust $ getAuthorSigLink d
          , signatoryisauthor = True
          , signatoryispartner = False
          , maybesignatory = Just $ userid user1 })
          , (def {
              signatorysignorder = SignOrder 1
            , signatoryisauthor = False
            , signatoryispartner = True
            , signatorylinkauthenticationtosignmethod = SMSPinAuthenticationToSign
            , signatorylinkdeliverymethod = MobileDelivery
            , signatoryfields = [
                fieldForTests (NameFI (NameOrder 1)) "Fred"
              , fieldForTests (NameFI (NameOrder 2)) "Frog"
              , fieldForTests EmailFI "fred@frog.com"
              , fieldForTests MobileFI "+47 666 111 777"
            ]
          })
      ]) (systemActor $ documentctime d)

    req <- mkRequest POST []
    (rdyrsp, _) <- lift . runTestKontra req ctx $ apiCallV1Ready $ documentid d
    lift $ do
      assertEqual "Ready call was successful" 202 (rsCode rdyrsp)
      runSQL ("SELECT * FROM chargeable_items WHERE type = 1 AND user_id =" <?> userid user1 <+> "AND user_group_id =" <?> ugid1 <+> "AND document_id =" <?> documentid d)
        >>= assertBool "Author and the company get charged for the delivery" . (> 0)
    let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
    siglink <- head . filter isUnsigned .documentsignatorylinks <$> theDocument

    --do t <- documentctime <$> theDocument
    --   randomUpdate . MarkDocumentSeen (signatorylinkid siglink) (signatorymagichash siglink)
    --             =<< signatoryActor (set ctxtime t ctx) siglink

    pin <- dbQuery $ GetSignatoryPin SMSPinToSign (signatorylinkid siglink) (getMobile siglink)
    preq <- mkRequest GET [ ]
    (_,ctx') <- updateDocumentWithID $ \did ->
                lift . runTestKontra preq ctx $ handleSignShowSaveMagicHash did (signatorylinkid siglink) (signatorymagichash siglink)


    req1 <- mkRequest POST [ ("fields", inText "[]"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
                      lift . runTestKontra req1 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is not closed if no pin is provided" Pending .documentstatus =<< theDocument

    req2 <- mkRequest POST [ ("fields", inText "[]"),("pin",inText $ pin ++ "4"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
                      lift . runTestKontra req2 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is not closed if pin is not valid" Pending .documentstatus =<< theDocument

    req3 <- mkRequest POST [ ("fields", inText "[]"),("pin",inText pin), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
                      lift . runTestKontra req3 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is closed if pin is valid" Closed .documentstatus =<< theDocument

    -- make sure that smses are counted only for the designated company
    runSQL ("SELECT * FROM chargeable_items WHERE type = 1 AND user_id <>" <?> userid user1)
      >>= assertEqual "Users other than author don't get charged" 0

    runSQL ("SELECT * FROM chargeable_items WHERE type = 1 AND user_group_id <>" <?> ugid1)
      >>= assertEqual "Companies other than author's one don't get charged" 0


testSendReminderEmailUpdatesLastModifiedDate :: TestEnv ()
testSendReminderEmailUpdatesLastModifiedDate = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))

  assertBool "Precondition" $ (get ctxtime ctx) /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = head . reverse $ documentsignatorylinks doc
  req <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req ctx $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is updated" $ compareTime (get ctxtime ctx) (documentmtime updateddoc)
  emails <- dbQuery GetEmailsForTest
  assertEqual "Email was sent" 1 (length emails)

testSendReminderEmailByCompanyAdmin :: TestEnv ()
testSendReminderEmailByCompanyAdmin = do
  ugid <- (get ugID) <$> addNewUserGroup
  user      <- addNewRandomCompanyUser ugid False
  otheruser <- addNewRandomCompanyUser ugid False
  adminuser <- addNewRandomCompanyUser ugid True

  ctx      <- (set ctxmaybeuser (Just user))      <$> mkContext def
  ctxadmin <- (set ctxmaybeuser (Just adminuser)) <$> mkContext def
  ctxother <- (set ctxmaybeuser (Just otheruser)) <$> mkContext def

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable -> True
                         _ -> False
                     && (all (== EmailDelivery) $ signatorylinkdeliverymethod <$> documentsignatorylinks d)   )

  assertBool "Precondition" $ (get ctxtime ctx) /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = head . reverse $ documentsignatorylinks doc

  -- fail if have no right to send reminder
  req1 <- mkRequest POST []
  result' <- E.try $ runTestKontra req1 ctxother $ handleResend (documentid doc) (signatorylinkid sl)

  case result' of
    Right _ -> assertFailure "Should not be able to resend when having no rights to do it"
    Left (_ :: E.SomeException) -> return ()


  updateddoc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is not updated" $ compareTime (documentmtime doc) (documentmtime updateddoc1)
  emails1 <- dbQuery GetEmailsForTest
  assertEqual "No emails were sent" 0 (length emails1)

  -- succeed to send a reminder as company admin
  req2 <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req2 ctxadmin $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is updated" $ compareTime (get ctxtime ctxadmin) (documentmtime updateddoc)
  emails <- dbQuery GetEmailsForTest
  assertEqual "Email was sent" 1 (length emails)

testDownloadFile :: TestEnv ()
testDownloadFile = do
  ugid <- (get ugID) <$> addNewUserGroup
  user <- addNewRandomCompanyUser ugid False
  otheruser <- addNewRandomCompanyUser ugid False
  adminuser <- addNewRandomCompanyUser ugid True

  ctxnotloggedin <- mkContext def

  ctxuser      <- (set ctxmaybeuser    (Just user))      <$> mkContext def
  ctxuseronpad <- (set ctxmaybepaduser (Just user))      <$> mkContext def
  ctxadmin     <- (set ctxmaybeuser    (Just adminuser)) <$> mkContext def
  ctxother     <- (set ctxmaybeuser    (Just otheruser)) <$> mkContext def

  reqfile <- mkRequest POST [ ("file", inFile $ inTestDir "pdfs/simple.pdf") ]
  (_rsp, _ctx') <- runTestKontra reqfile ctxuser $ apiCallV1CreateFromFile
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  assertBool "Document access token should not be zero" (documentmagichash doc /= unsafeMagicHash 0)

  -- who cares which one, just pick the last one
  --let sl = head . reverse $ documentsignatorylinks doc
  let Just (fid :: FileID) = (mainfileid <$> documentfile doc)

  let cases =
       [ (False, ctxnotloggedin, [], "nobody is not logged in")
       , (True,  ctxuser,        [], "user logged in is author")
       , (False, ctxuseronpad,   [], "user on pad is author when document in Preparation")
       , (False, ctxadmin,       [], "user logged in is admin of author when document in Preparation")
       , (False, ctxother,       [], "user logged in is unrelated to document")
       , (True,  ctxnotloggedin, [("accesstoken",inText (show (documentmagichash doc)))],
                                     "using accesstoken, nobody logged in")
       ]

  let sortOutResult apicall shouldallow res comment =
          case (shouldallow,res) of
            (True, Left (e :: E.SomeException)) -> do
              assertFailure $ "Should be able to download " ++ apicall ++ " when " ++ comment ++ ": " ++ show e
            (True, Right (resp1,_ctx1)) | rsCode resp1 < 200 || rsCode resp1 >= 399 -> do
              assertFailure $ "Should be able to download " ++ apicall ++ " when " ++ comment ++ ":\n" ++ show resp1
            (False, Right (resp1,_ctx1)) | rsCode resp1 >= 200 && rsCode resp1 <= 399 -> do
              assertFailure $ "Should not be able to download " ++ apicall ++ " when " ++ comment
            _ -> return ()

  forM_ cases $ \(shouldallow, ctx, params, comment) -> do
    req1 <- mkRequest GET params
    result1 <- E.try $ runTestKontra req1 ctx $
                  apiCallV1DownloadFile (documentid doc) fid "anything.pdf"
    sortOutResult "apiCallV1DownloadFile" shouldallow result1 comment

    result2 <- E.try $ runTestKontra req1 ctx $
                  apiCallV1DownloadMainFile (documentid doc) "anything.pdf"

    sortOutResult "apiCallV1DownloadMainFile" shouldallow result2 comment


testDownloadFileWithAuthToView :: TestEnv ()
testDownloadFileWithAuthToView = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- mkContext def
  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && length (documentsignatorylinks d) == 2
                     && (signatorylinkauthenticationtoviewmethod (documentsignatorylinks d !! 1) /= StandardAuthenticationToView)
                     && (not $ hasSigned (documentsignatorylinks d !! 1))
                     && (isSignatory (documentsignatorylinks d !! 1))
                     && case documenttype d of
                         Signable -> True
                         _ -> False)
  let sl = head $ reverse $ documentsignatorylinks $ doc
  req1 <- mkRequest GET []
  (_,ctx') <- runTestKontra req1 ctx $ handleSignShowSaveMagicHash (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
  req2 <- mkRequest GET [("signatorylinkid",inText $ show (signatorylinkid sl) )]
  (res2,_) <- runTestKontra req2 ctx' $  apiCallV1DownloadMainFile  (documentid doc) "anything.pdf"
  assertEqual "Response should be 403" 403 (rsCode res2)

testSendingReminderClearsDeliveryInformation :: TestEnv ()
testSendingReminderClearsDeliveryInformation = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (set ctxmaybeuser (Just user))
    <$> mkContext def

  addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable -> True
                         _ -> False) `withDocumentM` do
    sl <- head . reverse . documentsignatorylinks <$> theDocument
    let actor  =  systemActor $ get ctxtime ctx
    void $ dbUpdate $ MarkInvitationRead (signatorylinkid sl) actor
    -- who cares which one, just pick the last one
    req <- mkRequest POST []
    (_link, _ctx') <- do
      updateDocumentWithID $ \did -> lift . runTestKontra req ctx $ withDocumentID did $ sendReminderEmail Nothing actor False sl
    Just sl' <- find (\t -> signatorylinkid t == signatorylinkid sl) . documentsignatorylinks <$> theDocument
    assertEqual "Invitation is not delivered" (Unknown) (mailinvitationdeliverystatus sl')


testDocumentFromTemplate :: TestEnv ()
testDocumentFromTemplate = do
    (Just user) <- addNewUser "aaa" "bbb" "xxx@xxx.pl"
    doc <- addRandomDocumentWithAuthorAndCondition user (\d -> case documenttype d of
                                                            Template -> True
                                                            _ -> False)
    docs1 <- randomQuery $ GetDocumentsByAuthor (userid user)
    ctx <- (set ctxmaybeuser (Just user))
      <$> mkContext def
    req <- mkRequest POST []
    void $ runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertBool "No new document" (length docs2 == 1+ length docs1)

testDocumentFromTemplateShared :: TestEnv ()
testDocumentFromTemplateShared = do
    ugid <- (get ugID) <$> addNewUserGroup
    (Just author) <- addNewUserToUserGroup "aaa" "bbb" "xxx@xxx.pl" ugid
    doc <- addRandomDocumentWithAuthorAndCondition author (\d -> case documenttype d of
                                                            Template -> True
                                                            _ -> False)
    void $ randomUpdate $ SetDocumentSharing [documentid doc] True
    (Just user) <- addNewUserToUserGroup "ccc" "ddd" "zzz@zzz.pl" ugid
    docs1 <- randomQuery $ GetDocumentsByAuthor (userid user)
    ctx <- (set ctxmaybeuser (Just user))
      <$> mkContext def
    req <- mkRequest POST []
    void $ runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertEqual "New document should have been created" (1+length docs1) (length docs2)

testDocumentDeleteInBulk :: TestEnv ()
testDocumentDeleteInBulk = do
    ugid <- (get ugID) <$> addNewUserGroup
    (Just author) <- addNewUserToUserGroup "aaa" "bbb" "xxx@xxx.pl" ugid
    -- isSignable condition below is wrong. Tests somehow generate template documents
    -- that are pending and that breaks everything.
    docs <- replicateM 100 (addRandomDocumentWithAuthorAndCondition author (isSignable))

    ctx <- (set ctxmaybeuser (Just author)) <$> mkContext def
    req <- mkRequest POST [("documentids",  inText $ (show $ documentid <$> docs))]

    void $ runTestKontra req ctx $ handleDelete
    docs2 <- dbQuery $ GetDocumentsByAuthor (userid author)
    assertEqual "Documents are deleted" 0 (length docs2)

testGetLoggedIn :: TestEnv ()
testGetLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 200" 200 (rsCode res)


testGetNotLoggedIn :: TestEnv ()
testGetNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext def
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


testGetBadHeader :: TestEnv ()
testGetBadHeader = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  req <- mkRequestWithHeaders GET [] [("authorization", ["ABC"])]
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


-- Testing access to evidence documentation
testGetEvidenceAttachmentsLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext def
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 200" 200 (rsCode res)

testGetEvidenceAttachmentsNotLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext def
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 403" 403 (rsCode res)

-- Some test for signview branding generation

testSignviewBrandingBlocksNastyInput:: TestEnv ()
testSignviewBrandingBlocksNastyInput = do
  bd <- get ctxbrandeddomain <$> mkContext def -- We need to get default branded domain. And it can be fetched from default ctx
  theme <- dbQuery $ GetTheme $ (get bdSignviewTheme $ bd)
  emptyBrandingCSS <- signviewBrandingCSS theme
  assertBool "CSS generated for empty branding is not empty" (not $ BSL.null $ emptyBrandingCSS)
  let
    nasty1 = "nastyColor \n \n";
    nasty2 =  "alert('Nasty color')"
    nasty3 = "& very nasty font {}"
    nastyTheme = theme  {
        themeBrandColor  = nasty1
      , themeBrandTextColor = nasty2
      , themeFont = nasty3
      }
  nastyCSS <-  signviewBrandingCSS nastyTheme
  assertBool "CSS generated for nasty company branding is not empty" (not $ BSL.null $ nastyCSS)
  assertBool "CSS generated for nasty company branding does not contain nasty strings" $
       (not $ nasty1 `isInfixOf ` (BSL.toString $ nastyCSS))
    && (not $ nasty2 `isInfixOf ` (BSL.toString $ nastyCSS))
    && (not $ nasty3 `isInfixOf ` (BSL.toString $ nastyCSS))

testDownloadSignviewBrandingAccess :: TestEnv ()
testDownloadSignviewBrandingAccess = do
  -- Create file and make it ready for signing
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- saveNewFile filename filecontent

  doc <- addRandomDocumentWithAuthorAndConditionAndFile
            user
            (\d -> documentstatus d == Preparation
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))
            file
  withDocumentID (documentid doc) $ do
    d <- theDocument
    void $ randomUpdate $ ResetSignatoryDetails ([
                      (def {   signatoryfields = (signatoryfields $ fromJust $ getAuthorSigLink d)
                                      , signatoryisauthor = True
                                      , signatoryispartner = False
                                      , maybesignatory = Just $ userid user })
                    , (def {   signatorysignorder = SignOrder 1
                                      , signatoryisauthor = False
                                      , signatoryispartner = True
                                      , signatoryfields = [
                                          fieldForTests (NameFI (NameOrder 1)) "Fred"
                                        , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                        , fieldForTests EmailFI "fred@frog.com"
                                        ]})
                 ]) (systemActor $ documentctime d)
    t <- documentctime <$> theDocument
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor t) tz


  -- We have a document, now proper tests will take place

  -- 1) Check access to main signview branding
  emptyContext <- mkContext def
  let bid = get (bdid . ctxbrandeddomain) emptyContext
  svbr1 <- mkRequest GET [ ]
  resp1 <- E.try $  runTestKontra svbr1 emptyContext $ handleSignviewBranding bid (documentid doc) "branding-hash-12xdaad32-some_name.css"
  case resp1 of
    Right (cssResp1, _)->  assertBool "CSS should be returned" (rsCode cssResp1 == 200)
    Left (_ :: E.SomeException) -> assertFailure "CSS should be avaialbe for CDN"


  -- 2) Check access to main signview branding for author. Used when logged in a to-sign view.

  svbr2 <- mkRequest GET [ ]
  resp2 <- E.try $ runTestKontra svbr2 emptyContext $ handleSignviewBrandingWithoutDocument bid (userid user) "branding-hash-7cdsgSAq1-some_name.css"
  case resp2 of
    Right (cssResp2, _)->  assertBool "CSS should be returned" (rsCode cssResp2 == 200)
    Left (_ :: E.SomeException) -> assertFailure "CSS should be avaialbe for CDN"

testGetCancelledDocument :: TestEnv ()
testGetCancelledDocument = do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- anonymiseContext . set ctxmaybeuser (Just user) <$> mkContext def

  doc <- addRandomDocumentWithAuthorAndCondition user $ \d ->
    isPending d && isSignable d
  let did       = documentid doc
      signatory = head $ documentsignatorylinks doc
      slid      = signatorylinkid signatory
      mh        = signatorymagichash signatory

  withDocument doc $ randomUpdate $ CancelDocument $ authorActor ctx user

  -- It should fail if we're using the link with the magic hash.
  do
    req  <- mkRequest GET []
    eRes <- E.try $ runTestKontra req ctx $
      handleSignShowSaveMagicHash did slid mh

    case eRes of
      Right (res, _) ->
        assertFailure $ "Should have failed, returned code " ++ show (rsCode res)
      Left err ->
        assertEqual "Should throw LinkInvalid" LinkInvalid err

  -- It shouldn't fail if we had already clicked on the link.
  do
    req <- mkRequest GET []
    (res, _) <- runTestKontra req ctx $ do
      randomUpdate $ AddDocumentSessionToken slid mh
      handleSignShow did slid
    assertEqual "Status is 200" 200 (rsCode res)

testDocumentFromShareableTemplate :: TestEnv ()
testDocumentFromShareableTemplate = do
  Just user <- addNewUser "Bob" "Blue" "bob@blue.com"
  tpl <- addRandomDocumentWithAuthorAndCondition user $ \d ->
    isTemplate d && any (not . isAuthor) (documentsignatorylinks d)
  mh <- random
  withDocument tpl $ randomUpdate $ UpdateShareableLinkHash $ Just mh

  ctx <- anonymiseContext <$> mkContext def
  req <- mkRequest GET []
  (res, ctx') <- runTestKontra req ctx $
    handleSignFromTemplate (documentid tpl) mh

  assertEqual "Status is 303" 303 (rsCode res)
  let Just HeaderPair{ hValue = [loc] } = M.lookup "location" $ rsHeaders res
      (did, slid) = bimap read (read . drop 1)
        . break (=='/')
        . drop (length ("/s/" :: String))
        . BS8.unpack
        $ loc
  doc <- randomQuery $ GetDocumentByDocumentID did
  assertNotEqual "Should be a different document"
                 (documentid doc) (documentid tpl)
  assertEqual "Should have the same title"
              (documenttitle doc) (documenttitle tpl)
  assertEqual "Shouldn't have a shareable link hash"
              Nothing (documentshareablelinkhash doc)

  req' <- mkRequest GET []
  (res', _) <- runTestKontra req' ctx' $ handleSignShow did slid
  assertEqual "Status is 200" 200 (rsCode res')
