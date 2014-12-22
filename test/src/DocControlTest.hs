module DocControlTest(
    docControlTests
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Monoid.Space
import Happstack.Server
import Test.Framework
import Text.JSON.Gen (toJSValue)
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Text.JSON

import Archive.Control
import Company.CompanyUI
import Company.Model
import Context
import DB
import DB.TimeZoneName (mkTimeZoneName)
import Doc.API.V1.Calls
import Doc.DocControl
import Doc.DocStateData
import Doc.DocumentMonad (withDocumentM, withDocumentID, theDocument, updateDocumentWithID)
import Doc.DocUtils
import Doc.DocView (documentSignviewBrandingCSS)
import Doc.Model
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryFieldID
import Doc.SignatoryScreenshots(emptySignatoryScreenshots, SignatoryScreenshots(signing))
import Doc.SMSPin.Model
import File.FileID
import MagicHash
import Mails.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Default

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

  , testThat "Signview branding generation block nasty input " env testSignviewBrandingBlocksNastyInput
  , testThat "We can download signview branding if we have access to document" env testDownloadSignviewBrandingAccess
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
  docs <- randomQuery $ GetDocuments [DocumentsVisibleToUser $ userid user] [DocumentFilterDeleted False] [] (0,maxBound)
  assertEqual "Draft is there" 1 (length docs)
  docs' <- randomQuery $ GetDocuments [DocumentsVisibleToUser $ userid user] [DocumentFilterUnsavedDraft False, DocumentFilterDeleted False] [] (0,maxBound)
  assertEqual "Draft is not visible in archive" 0 (length docs')



uploadDocAsNewUser :: TestEnv (User, Response)
uploadDocAsNewUser = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  req <- mkRequest POST [ ("file", inFile "test/pdfs/simple.pdf") ]
  (rsp, _ctx') <- runTestKontra req ctx $ apiCallV1CreateFromFile
  return (user, rsp)


signScreenshots :: (String, Input)
signScreenshots = ("screenshots", inText $ Text.JSON.encode $ toJSValue $
                   emptySignatoryScreenshots { signing = s })
  where s = Just $ Screenshot unixEpoch $ Binary "\255\216\255\224\NUL\DLEJFIF\NUL\SOH\SOH\SOH\NULH\NULH\NUL\NUL\255\219\NULC\NUL\ETX\STX\STX\STX\STX\STX\ETX\STX\STX\STX\ETX\ETX\ETX\ETX\EOT\ACK\EOT\EOT\EOT\EOT\EOT\b\ACK\ACK\ENQ\ACK\t\b\n\n\t\b\t\t\n\f\SI\f\n\v\SO\v\t\t\r\DC1\r\SO\SI\DLE\DLE\DC1\DLE\n\f\DC2\DC3\DC2\DLE\DC3\SI\DLE\DLE\DLE\255\201\NUL\v\b\NUL\SOH\NUL\SOH\SOH\SOH\DC1\NUL\255\204\NUL\ACK\NUL\DLE\DLE\ENQ\255\218\NUL\b\SOH\SOH\NUL\NUL?\NUL\210\207 \255\217"

testLastPersonSigningADocumentClosesIt :: TestEnv ()
testLastPersonSigningADocumentClosesIt = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  let filename = "test/pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent

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
                      (defaultValue {   signatoryfields = (signatoryfields $ fromJust $ getAuthorSigLink d)
                                      , signatoryisauthor = True
                                      , signatoryispartner = False
                                      , maybesignatory = Just $ userid user })
                    , (defaultValue {   signatorysignorder = SignOrder 1
                                      , signatoryisauthor = False
                                      , signatoryispartner = True
                                      , signatoryfields = [
                                          SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Fred" True False []
                                        , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Frog"  True False []
                                        , SignatoryField (unsafeSignatoryFieldID 0) EmailFT "fred@frog.com" True False []
                                        ]})
                 ]) (systemActor $ documentctime d)


    do t <- documentctime <$> theDocument
       tz <- mkTimeZoneName "Europe/Stockholm"
       randomUpdate $ PreparationToPending (systemActor t) tz
    let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
    siglink <- head . filter isUnsigned .documentsignatorylinks <$> theDocument

    do t <- documentctime <$> theDocument
       randomUpdate . MarkDocumentSeen (signatorylinkid siglink) (signatorymagichash siglink)
                 =<< signatoryActor ctx{ ctxtime = t } siglink

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
  company1 <- addNewCompany
  company2 <- addNewCompany
  Just user1 <- addNewUser "Bob" "Blue" "bob@blue.com"
  Just user2 <- addNewUser "Gary" "Green" "gary@green.com"
  True <- dbUpdate $ SetUserCompany (userid user1) (companyid company1)
  True <- dbUpdate $ SetUserCompany (userid user2) (companyid company2)
  ctx <- (\c -> c { ctxmaybeuser = Just user1 })
    <$> mkContext defaultValue

  let filename = "test/pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent

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
        (defaultValue {
            signatoryfields = signatoryfields $ fromJust $ getAuthorSigLink d
          , signatoryisauthor = True
          , signatoryispartner = False
          , maybesignatory = Just $ userid user1 })
          , (defaultValue {
              signatorysignorder = SignOrder 1
            , signatoryisauthor = False
            , signatoryispartner = True
            , signatorylinkauthenticationmethod = SMSPinAuthentication
            , signatorylinkdeliverymethod = MobileDelivery
            , signatoryfields = [
                SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Fred" True False []
              , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Frog"  True False []
              , SignatoryField (unsafeSignatoryFieldID 0) EmailFT "fred@frog.com" True False []
              , SignatoryField (unsafeSignatoryFieldID 0) MobileFT "+47 666 111 777" True False []
            ]
          })
      ]) (systemActor $ documentctime d)

    req <- mkRequest POST []
    (rdyrsp, _) <- lift . runTestKontra req ctx $ apiCallV1Ready $ documentid d
    lift $ do
      assertEqual "Ready call was successful" 202 (rsCode rdyrsp)
      runSQL ("SELECT * FROM chargeable_items WHERE type = 1 AND user_id =" <?> userid user1 <+> "AND company_id =" <?> companyid company1 <+> "AND document_id =" <?> documentid d)
        >>= assertBool "Author and the company get charged for the delivery" . (> 0)
    let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
    siglink <- head . filter isUnsigned .documentsignatorylinks <$> theDocument

    --do t <- documentctime <$> theDocument
    --   randomUpdate . MarkDocumentSeen (signatorylinkid siglink) (signatorymagichash siglink)
    --             =<< signatoryActor ctx{ ctxtime = t } siglink

    pin <- dbQuery $ GetSignatoryPin (signatorylinkid siglink) (getMobile siglink)
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

    runSQL ("SELECT * FROM chargeable_items WHERE type = 1 AND company_id <>" <?> companyid company1)
      >>= assertEqual "Companies other than author's one don't get charged" 0


testSendReminderEmailUpdatesLastModifiedDate :: TestEnv ()
testSendReminderEmailUpdatesLastModifiedDate = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))

  assertBool "Precondition" $ (ctxtime ctx) /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = head . reverse $ documentsignatorylinks doc
  req <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req ctx $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "Modified date is updated" (ctxtime ctx) (documentmtime updateddoc)
  emails <- dbQuery GetEmails
  assertEqual "Email was sent" 1 (length emails)

testSendReminderEmailByCompanyAdmin :: TestEnv ()
testSendReminderEmailByCompanyAdmin = do
  company <- addNewCompany
  user <- addNewRandomCompanyUser (companyid company) False
  otheruser <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True

  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  ctxadmin <- (\c -> c { ctxmaybeuser = Just adminuser })
    <$> mkContext defaultValue

  ctxother <- (\c -> c { ctxmaybeuser = Just otheruser })
    <$> mkContext defaultValue

  doc <- addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable -> True
                         _ -> False
                     && (all (== EmailDelivery) $ signatorylinkdeliverymethod <$> documentsignatorylinks d)   )

  assertBool "Precondition" $ (ctxtime ctx) /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = head . reverse $ documentsignatorylinks doc

  -- fail if have no right to send reminder
  req1 <- mkRequest POST []
  result' <- E.try $ runTestKontra req1 ctxother $ handleResend (documentid doc) (signatorylinkid sl)

  case result' of
    Right _ -> assertFailure "Should not be able to resend when having no rights to do it"
    Left (_ :: E.SomeException) -> return ()


  updateddoc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "Modified date is not updated" (documentmtime doc) (documentmtime updateddoc1)
  emails1 <- dbQuery GetEmails
  assertEqual "No emails were sent" 0 (length emails1)

  -- succeed to send a reminder as company admin
  req2 <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req2 ctxadmin $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertEqual "Modified date is updated" (ctxtime ctxadmin) (documentmtime updateddoc)
  emails <- dbQuery GetEmails
  assertEqual "Email was sent" 1 (length emails)

testDownloadFile :: TestEnv ()
testDownloadFile = do
  company <- addNewCompany
  user <- addNewRandomCompanyUser (companyid company) False
  otheruser <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True

  ctxnotloggedin <- mkContext defaultValue
  ctxuser <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  ctxuseronpad <- (\c -> c { ctxmaybepaduser = Just user })
    <$> mkContext defaultValue

  ctxadmin <- (\c -> c { ctxmaybeuser = Just adminuser })
    <$> mkContext defaultValue

  ctxother <- (\c -> c { ctxmaybeuser = Just otheruser })
    <$> mkContext defaultValue

  reqfile <- mkRequest POST [ ("file", inFile "test/pdfs/simple.pdf") ]
  (_rsp, _ctx') <- runTestKontra reqfile ctxuser $ apiCallV1CreateFromFile
  [doc] <- randomQuery $ GetDocumentsByAuthor (userid user)

  assertBool "Document access token should not be zero" (documentmagichash doc /= unsafeMagicHash 0)

  -- who cares which one, just pick the last one
  --let sl = head . reverse $ documentsignatorylinks doc
  let Just (fid :: FileID) = (documentfile doc)

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

testSendingReminderClearsDeliveryInformation :: TestEnv ()
testSendingReminderClearsDeliveryInformation = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  ctx <- (\c -> c { ctxmaybeuser = Just user })
    <$> mkContext defaultValue

  addRandomDocumentWithAuthorAndCondition
            user
            (\d -> documentstatus d == Pending
                     && case documenttype d of
                         Signable -> True
                         _ -> False) `withDocumentM` do
    sl <- head . reverse . documentsignatorylinks <$> theDocument
    let actor  =  systemActor $ ctxtime ctx
    _ <- dbUpdate $ MarkInvitationRead (signatorylinkid sl) actor
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
    ctx <- (\c -> c { ctxmaybeuser = Just user })
      <$> mkContext defaultValue
    req <- mkRequest POST []
    _ <- runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertBool "No new document" (length docs2 == 1+ length docs1)

testDocumentFromTemplateShared :: TestEnv ()
testDocumentFromTemplateShared = do
    (Company {companyid}) <- addNewCompany
    (Just author) <- addNewCompanyUser "aaa" "bbb" "xxx@xxx.pl" companyid
    doc <- addRandomDocumentWithAuthorAndCondition author (\d -> case documenttype d of
                                                            Template -> True
                                                            _ -> False)
    _ <- randomUpdate $ SetDocumentSharing [documentid doc] True
    (Just user) <- addNewCompanyUser "ccc" "ddd" "zzz@zzz.pl" companyid
    docs1 <- randomQuery $ GetDocumentsByAuthor (userid user)
    ctx <- (\c -> c { ctxmaybeuser = Just user })
      <$> mkContext defaultValue
    req <- mkRequest POST []
    _ <- runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
    docs2 <- randomQuery $ GetDocumentsByAuthor (userid user)
    assertEqual "New document should have been created" (1+length docs1) (length docs2)

testDocumentDeleteInBulk :: TestEnv ()
testDocumentDeleteInBulk = do
    (Company {companyid}) <- addNewCompany
    (Just author) <- addNewCompanyUser "aaa" "bbb" "xxx@xxx.pl" companyid
    -- isSignable condition below is wrong. Tests somehow generate template documents
    -- that are pending and that breaks everything.
    docs <- replicateM 100 (addRandomDocumentWithAuthorAndCondition author (isSignable))

    ctx <- (\c -> c { ctxmaybeuser = Just author})
      <$> mkContext defaultValue
    req <- mkRequest POST [("documentids",  inText $ (show $ documentid <$> docs))]

    _ <- runTestKontra req ctx $ handleDelete
    docs2 <- dbQuery $ GetDocumentsByAuthor (userid author)
    assertEqual "Documents are deleted" 0 (length docs2)


testGetLoggedIn :: TestEnv ()
testGetLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 200" 200 (rsCode res)


testGetNotLoggedIn :: TestEnv ()
testGetNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


testGetBadHeader :: TestEnv ()
testGetBadHeader = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequestWithHeaders GET [] [("authorization", ["ABC"])]
  (res,_) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


-- Testing access to evidence documentation
testGetEvidenceAttachmentsLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 200" 200 (rsCode res)

testGetEvidenceAttachmentsNotLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 403" 403 (rsCode res)

-- Some test for signview branding generation

testSignviewBrandingBlocksNastyInput:: TestEnv ()
testSignviewBrandingBlocksNastyInput = do
  emptyBrandingCSS <- liftIO $ documentSignviewBrandingCSS Nothing Nothing
  assertBool "CSS generated for empty branding is not empty" (not $ BSL.null $ emptyBrandingCSS)
  company<- addNewCompany
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  companyCSS <- liftIO $ documentSignviewBrandingCSS Nothing (Just companyui)
  assertBool "CSS generated for random company branding is not empty" (not $ BSL.null $ companyCSS)

  let
    nasty1 = "nastyColor \n \n";
    nasty2 =  "alert('Nasty color')"
    nasty3 = "& very nasty font {}"
    nastyCompanyUI = companyui {
        companysignviewsecondarycolour  = Just nasty1
      , companysignviewbackgroundcolour = Just nasty2
      , companysignviewtextfont = Just nasty3
    }
  nastyCSS <- liftIO $ documentSignviewBrandingCSS Nothing (Just nastyCompanyUI)
  assertBool "CSS generated for nasty company branding is not empty" (not $ BSL.null $ nastyCSS)
  assertBool "CSS generated for nasty company branding does not contain nasty strings" $
       (not $ nasty1 `isInfixOf ` (BSL.toString $ nastyCSS))
    && (not $ nasty2 `isInfixOf ` (BSL.toString $ nastyCSS))
    && (not $ nasty2 `isInfixOf ` (BSL.toString $ nastyCSS))

testDownloadSignviewBrandingAccess :: TestEnv ()
testDownloadSignviewBrandingAccess = do
  -- Create file and make it ready for signing
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  let filename = "test/pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent

  doc <- addRandomDocumentWithAuthorAndConditionAndFile
            user
            (\d -> documentstatus d == Preparation
                     && (case documenttype d of
                          Signable -> True
                          _ -> False)
                     && all ((==) EmailDelivery . signatorylinkdeliverymethod) (documentsignatorylinks d))
            file
  siglink <- withDocumentID (documentid doc) $ do
    d <- theDocument
    _ <- randomUpdate $ ResetSignatoryDetails ([
                      (defaultValue {   signatoryfields = (signatoryfields $ fromJust $ getAuthorSigLink d)
                                      , signatoryisauthor = True
                                      , signatoryispartner = False
                                      , maybesignatory = Just $ userid user })
                    , (defaultValue {   signatorysignorder = SignOrder 1
                                      , signatoryisauthor = False
                                      , signatoryispartner = True
                                      , signatoryfields = [
                                          SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Fred" True False []
                                        , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Frog"  True False []
                                        , SignatoryField (unsafeSignatoryFieldID 0) EmailFT "fred@frog.com" True False []
                                        ]})
                 ]) (systemActor $ documentctime d)
    t <- documentctime <$> theDocument
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor t) tz
    let isUnsigned sl = isSignatory sl && isNothing (maybesigninfo sl)
    head . filter isUnsigned .documentsignatorylinks <$> theDocument

  -- We have a document, now proper tests will take place

  -- 1) Check access to main signview branding
  emptyContext <- mkContext defaultValue
  svbr1 <- mkRequest GET [ ]
  resp1 <- E.try $  runTestKontra svbr1 emptyContext $ handleSignviewCSS (documentid doc) (signatorylinkid siglink) "some_name.css"
  case resp1 of
    Right _ ->   assertFailure "CSS for signing is available for anyone, and thats bad"
    Left (_ :: E.SomeException) -> return ()


  preq <- mkRequest GET [ ]
  (_,ctxWithSignatory) <- runTestKontra preq emptyContext $ handleSignShowSaveMagicHash (documentid doc) (signatorylinkid siglink) (signatorymagichash siglink)

  svbr2 <- mkRequest GET [ ]
  (cssResp2, _) <- runTestKontra svbr2 ctxWithSignatory $ handleSignviewCSS (documentid doc) (signatorylinkid siglink) "some_name.css"
  assertBool "CSS for signing is available for signatories" (rsCode cssResp2 == 200)

  -- 2) Check access to main signview branding for author. Used when logged in a to-sign view.

  svbr3 <- mkRequest GET [ ]
  resp3 <- E.try $ runTestKontra svbr3 emptyContext $ handleSignviewCSSWithoutDocument  "some_name.css"
  case resp3 of
    Right _ ->   assertFailure "CSS for signing without document is available for anyone, and thats bad"
    Left (_ :: E.SomeException) -> return ()

  let ctxWithAuthor = emptyContext {ctxmaybeuser = Just user}
  svbr4 <- mkRequest GET [ ]
  (cssResp4, _) <- runTestKontra svbr4 ctxWithAuthor $ handleSignviewCSSWithoutDocument  "some_name.css"
  assertBool "CSS for signing is available for author" (rsCode cssResp4 == 200)

  -- 3) Check access to main signview branding for branded domain (no user or document). Used for logging in to to-sign view.
  svbr5 <- mkRequest GET [ ]
  (cssResp5, _) <- runTestKontra svbr5 emptyContext $ handleSignviewCSSWithoutDocumentAndWithoutUser  "some_name.css"
  assertBool "CSS for signing for branded domain is available for anyone" (rsCode cssResp5 == 200)
