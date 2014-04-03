module DocControlTest(
    docControlTests
) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception.Lifted as E
import Data.List
import Happstack.Server
import Test.Framework
import TestingUtil
import TestKontra as T
import qualified Text.JSON
import Text.JSON.Gen (toJSValue)
import Mails.Model
import MinutesTime(fromSeconds)
import Utils.Default
import Context
import DB
import Doc.Model
import Doc.DocStateData
import Doc.DocControl
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryScreenshots(emptySignatoryScreenshots, SignatoryScreenshots(signing))
import Archive.Control
import Doc.DocumentMonad (withDocumentM, withDocumentID, theDocument, updateDocumentWithID)
import Doc.DocUtils
import Company.Model
import User.Model
import Util.SignatoryLinkUtils
import Util.Actor
import File.FileID
import Doc.API
import ELegitimation.BankIDUtils
import DB.TimeZoneName (mkTimeZoneName)
import MagicHash

docControlTests :: TestEnvSt -> Test
docControlTests env = testGroup "Templates" [
    testThat "Sending a reminder updates last modified date on doc" env testSendReminderEmailUpdatesLastModifiedDate
  , testThat "Create document from template" env testDocumentFromTemplate
  , testThat "Uploading file as contract makes doc" env testUploadingFile
  , testThat "Create document from template | Shared" env testDocumentFromTemplateShared
  , testThat "Uploading file creates unsaved draft" env testNewDocumentUnsavedDraft
  , testThat "Last person signing a doc closes it" env testLastPersonSigningADocumentClosesIt
  , testThat "Sending an reminder clears delivery information" env testSendingReminderClearsDeliveryInformation
  , testThat "Sending reminder email works for company admin" env testSendReminderEmailByCompanyAdmin
  , testThat "We can get json for document" env testGetLoggedIn
  , testThat "We can't get json for document if we are not logged in" env testGetNotLoggedIn
  , testThat "We can't get json for document is we are logged in but we provided authorization header" env testGetBadHeader
  , testThat "Document bulk delete works fast" env testDocumentDeleteInBulk
  , testThat "Bankid mismatch happends when it should" env testBankIDMismatch
  , testThat "Download file and download main file obey access rights" env testDownloadFile
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
  (rsp, _ctx') <- runTestKontra req ctx $ apiCallCreateFromFile
  return (user, rsp)


signScreenshots :: (String, Input)
signScreenshots = ("screenshots", inText $ Text.JSON.encode $ toJSValue $
                   emptySignatoryScreenshots { signing = s })
  where s = Just $ Screenshot (fromSeconds 0) $ Binary "\255\216\255\224\NUL\DLEJFIF\NUL\SOH\SOH\SOH\NULH\NULH\NUL\NUL\255\219\NULC\NUL\ETX\STX\STX\STX\STX\STX\ETX\STX\STX\STX\ETX\ETX\ETX\ETX\EOT\ACK\EOT\EOT\EOT\EOT\EOT\b\ACK\ACK\ENQ\ACK\t\b\n\n\t\b\t\t\n\f\SI\f\n\v\SO\v\t\t\r\DC1\r\SO\SI\DLE\DLE\DC1\DLE\n\f\DC2\DC3\DC2\DLE\DC3\SI\DLE\DLE\DLE\255\201\NUL\v\b\NUL\SOH\NUL\SOH\SOH\SOH\DC1\NUL\255\204\NUL\ACK\NUL\DLE\DLE\ENQ\255\218\NUL\b\SOH\SOH\NUL\NUL?\NUL\210\207 \255\217"

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
                                          SignatoryField FirstNameFT "Fred" True False []
                                        , SignatoryField LastNameFT "Frog"  True False []
                                        , SignatoryField EmailFT "fred@frog.com" True False []
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
                      lift . runTestKontra req ctx' $ apiCallSign did (signatorylinkid siglink)

    assertEqual "In closed state" Closed .documentstatus =<< theDocument
    --TODO: this should be commented out really, I guess it's a bug
    --assertEqual "None left to sign" 0 (length $ filter isUnsigned (documentsignatorylinks doc))
    --emails <- dbQuery GetEmails
    --assertEqual "Confirmation email sent" 1 (length emails)

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
  (_rsp, _ctx') <- runTestKontra reqfile ctxuser $ apiCallCreateFromFile
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
                  apiCallDownloadFile (documentid doc) fid "anything.pdf"
    sortOutResult "apiCallDownloadFile" shouldallow result1 comment

    result2 <- E.try $ runTestKontra req1 ctx $
                  apiCallDownloadMainFile (documentid doc) "anything.pdf"

    sortOutResult "apiCallDownloadMainFile" shouldallow result2 comment

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
    _ <- runTestKontra req ctx $ apiCallCreateFromTemplate (documentid doc)
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
    _ <- runTestKontra req ctx $ apiCallCreateFromTemplate (documentid doc)
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


testBankIDMismatch :: TestEnv ()
testBankIDMismatch = do
    r1 <- return $ compareNames "Mariusz Rak" "Mariusz Rak"
    assertEqual "Valid match" MergeMatch r1
    r2 <- return $ compareNames "Mariusz Rak" "Moriusz  Rok"
    assertEqual "Valid match" MergeMatch r2
    r3 <- return $ compareNames "Michal Sloink" "Rak Mariusz"
    assertBool "Invalid match " (MergeMatch /= r3)
    r4 <- return $ compareNames "AAAA CCCC" "BBBB DDDD"
    assertBool "Invalid match " (MergeMatch /= r4)

testGetLoggedIn :: TestEnv ()
testGetLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 200" 200 (rsCode res)


testGetNotLoggedIn :: TestEnv ()
testGetNotLoggedIn = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- mkContext defaultValue
  req <- mkRequest GET []
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 403" 403 (rsCode res)


testGetBadHeader :: TestEnv ()
testGetBadHeader = do
  (Just user) <- addNewUser "Bob" "Blue" "bob@blue.com"
  doc <- addRandomDocumentWithAuthor user
  ctx <- (\c -> c { ctxmaybeuser = Just user }) <$> mkContext defaultValue
  req <- mkRequestWithHeaders GET [] [("authorization", ["ABC"])]
  (res,_) <- runTestKontra req ctx $ apiCallGet doc
  assertEqual "Response code is 403" 403 (rsCode res)
