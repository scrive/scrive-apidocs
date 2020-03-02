module DocControlTest (docControlTests) where

import Control.Monad.Trans
import Crypto.RNG
import Data.Bifunctor
import Data.Int
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import Text.JSON.Gen (toJSValue)
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.JSON

import Archive.Control
import Branding.Control
import Branding.CSS
import Chargeable
import DB
import DB.TimeZoneName (mkTimeZoneName)
import Doc.API.V1.Calls
import Doc.DocControl
import Doc.DocStateData
import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad
  ( theDocument, updateDocumentWithID, withDocument, withDocumentID
  , withDocumentM
  )
import Doc.Model
import Doc.Screenshot (Screenshot(..))
import Doc.SignatoryLinkID (SignatoryLinkID)
import Doc.SignatoryScreenshots
  ( SignatoryScreenshots(signing), emptySignatoryScreenshots
  )
import Doc.SMSPin.Model
import Doc.Tokens.Model
import Doc.Types.SignatoryAccessToken
import File.FileID
import File.Storage
import KontraError
import MagicHash
import Mails.Model
import MinutesTime
import SealingMethod
import Session.Model
import TestCron
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Model
import UserGroup.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Util.SMSLinkShortening as SMSLinkShortening

docControlTests :: TestEnvSt -> Test
docControlTests env = testGroup
  "DocControl"
  [ testThat "Sending a reminder updates last modified date on doc"
             env
             testSendReminderEmailUpdatesLastModifiedDate
  , testThat "Create document from template"          env testDocumentFromTemplate
  , testThat "Uploading file as contract makes doc"   env testUploadingFile
  , testThat "Create document from template | Shared" env testDocumentFromTemplateShared
  , testThat "Uploading file creates unsaved draft"   env testNewDocumentUnsavedDraft
  , testThat "Last person signing a doc closes it"
             env
             testLastPersonSigningADocumentClosesIt
  , testThat "Signing with pin" env testSigningWithPin
  , testThat "Sending an reminder clears delivery information"
             env
             testSendingReminderClearsDeliveryInformation
  , testThat "Sending reminder email works for company admin"
             env
             testSendReminderEmailByCompanyAdmin
  , testThat "We can get json for document" env testGetLoggedIn
  , testThat "We can't get json for document if we are not logged in"
             env
             testGetNotLoggedIn
  , testThat
    "We can't get json for document is we are logged in but we provided authorization header"
    env
    testGetBadHeader
  , testThat "We can get json for evidence attachments"
             env
             testGetEvidenceAttachmentsLoggedIn
  , testThat "We can't get json for evidence attachments if we are not logged in"
             env
             testGetEvidenceAttachmentsNotLoggedIn
  , testThat "Document bulk delete works fast" env testDocumentDeleteInBulk
  , testThat "Download file and download main file obey access rights"
             env
             testDownloadFile
  , testThat
    "Download file and download main file blocked if authorization to view is needed"
    env
    testDownloadFileWithAuthToView
  , testThat "Signview branding generation block nasty input "
             env
             testSignviewBrandingBlocksNastyInput
  , testThat "We can download signview branding if we have access to document"
             env
             testDownloadSignviewBrandingAccess
  , testThat "We can't get a document as a signatory if it has been cancelled"
             env
             testGetCancelledDocument
  , testThat "Generate document to sign from shareable template"
             env
             testDocumentFromShareableTemplate
  , testThat "We can get a document using a temporary magic hash if it is not expired"
             env
             testGetDocumentWithSignatoryAccessTokens
  , testThat "Timeouting document can trigger info mail sendout to author"
             env
             testSendEmailOnTimeout
  , testThat "Short link handler either redirects or returns 404"
             env
             testShortLinkRedirectTest
  ]

testUploadingFile :: TestEnv ()
testUploadingFile = do
  (user, rsp) <- uploadDocAsNewUser
  docs        <- randomQuery $ GetDocumentsByAuthor (user ^. #id)
  assertEqual "New doc" 1 (length docs)
  let newdoc = head docs
  assertBool "Document id in result json" (show (documentid newdoc) `isInfixOf` show rsp)

testNewDocumentUnsavedDraft :: TestEnv ()
testNewDocumentUnsavedDraft = do
  (user, _rsp) <- uploadDocAsNewUser
  docs         <- randomQuery $ GetDocuments (DocumentsVisibleToUser $ user ^. #id)
                                             [DocumentFilterDeleted False]
                                             []
                                             maxBound
  assertEqual "Draft is there" 1 (length docs)
  docs' <- randomQuery $ GetDocuments
    (DocumentsVisibleToUser $ user ^. #id)
    [DocumentFilterUnsavedDraft False, DocumentFilterDeleted False]
    []
    maxBound
  assertEqual "Draft is not visible in archive" 0 (length docs')



uploadDocAsNewUser :: TestEnv (User, Response)
uploadDocAsNewUser = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx          <- mkContextWithUser defaultLang user

  req          <- mkRequest POST [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (rsp, _ctx') <- runTestKontra req ctx apiCallV1CreateFromFile
  return (user, rsp)


signScreenshots :: (Text, Input)
signScreenshots =
  ( "screenshots"
  , inText . T.pack $ Text.JSON.encode
    (toJSValue $ emptySignatoryScreenshots { signing = s })
  )
  where
    s = Just $ Screenshot
      unixEpoch
      "\255\216\255\224\NUL\DLEJFIF\NUL\SOH\SOH\SOH\NULH\NULH\NUL\NUL\255\219\NULC\NUL\ETX\STX\STX\STX\STX\STX\ETX\STX\STX\STX\ETX\ETX\ETX\ETX\EOT\ACK\EOT\EOT\EOT\EOT\EOT\b\ACK\ACK\ENQ\ACK\t\b\n\n\t\b\t\t\n\f\SI\f\n\v\SO\v\t\t\r\DC1\r\SO\SI\DLE\DLE\DC1\DLE\n\f\DC2\DC3\DC2\DLE\DC3\SI\DLE\DLE\DLE\255\201\NUL\v\b\NUL\SOH\NUL\SOH\SOH\SOH\DC1\NUL\255\204\NUL\ACK\NUL\DLE\DLE\ENQ\255\218\NUL\b\SOH\SOH\NUL\NUL?\NUL\210\207 \255\217"

testLastPersonSigningADocumentClosesIt :: TestEnv ()
testLastPersonSigningADocumentClosesIt = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx <- mkContextWithUser defaultLang user

  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file        <- saveNewFile (T.pack filename) filecontent

  let
    genDoc = addRandomDocumentWithFile
      file
      (rdaDefault user)
        { rdaTypes       = OneOf [Signable]
        , rdaStatuses    = OneOf [Preparation]
        , rdaSignatories = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                           in  OneOf $ map (`replicate` signatory) [1 .. 10]
        }
  withDocumentM genDoc $ do

    success <- do
      d <- theDocument
      randomUpdate $ ResetSignatoryDetails
        [ (defaultSignatoryLink
            { signatoryfields   = signatoryfields . fromJust $ getAuthorSigLink d
            , signatoryisauthor = True
            , signatoryrole     = SignatoryRoleViewer
            , maybesignatory    = Just $ user ^. #id
            }
          )
        , (defaultSignatoryLink
            { signatorysignorder = SignOrder 1
            , signatoryisauthor  = False
            , signatoryrole      = SignatoryRoleSigningParty
            , signatoryfields    = [ fieldForTests (NameFI (NameOrder 1)) "Fred"
                                   , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                   , fieldForTests EmailFI                "fred@frog.com"
                                   ]
            }
          )
        ]
        (systemActor $ documentctime d)
    unless success $ unexpectedError "Expected True"

    do
      t  <- documentctime <$> theDocument
      tz <- mkTimeZoneName "Europe/Stockholm"
      randomUpdate $ PreparationToPending (systemActor t) tz
    siglink <-
      head . filter isSignatoryAndHasNotSigned . documentsignatorylinks <$> theDocument

    mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid siglink)
                                             SignatoryAccessTokenForMailBeforeClosing
                                             Nothing

    do
      t <- documentctime <$> theDocument
      randomUpdate
        .   MarkDocumentSeen (signatorylinkid siglink)
        =<< signatoryActor (set #time t ctx) siglink

    assertEqual "One left to sign" 1
      .   length
      .   filter isSignatoryAndHasNotSigned
      .   documentsignatorylinks
      =<< theDocument

    preq      <- mkRequest GET []
    (_, ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra preq ctx $ handleSignShowSaveMagicHash
        did
        (signatorylinkid siglink)
        mh

    req            <- mkRequest POST [("fields", inText "[]"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra req ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "In closed state" Closed . documentstatus =<< theDocument
    -- TODO: this should be commented out really, I guess it's a bug
    -- assertEqual "None left to sign" 0 (length $ filter
    -- isSignatoryAndHasNotSigned (documentsignatorylinks doc)) emails
    -- <- dbQuery GetEmails assertEqual "Confirmation email sent" 1
    -- (length emails)


testSigningWithPin :: TestEnv ()
testSigningWithPin = do
  ugid1 <- view #id <$> instantiateRandomUserGroup
  ugid2 <- view #id <$> instantiateRandomUserGroup
  user1 <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                                , lastName  = return "Blue"
                                                , email     = return "bob@blue.com"
                                                }
  user2 <- instantiateUser $ randomUserTemplate { firstName = return "Gary"
                                                , lastName  = return "Green"
                                                , email     = return "gary@green.com"
                                                }
  True <- dbUpdate $ SetUserUserGroup (user1 ^. #id) ugid1
  True <- dbUpdate $ SetUserUserGroup (user2 ^. #id) ugid2
  ctx  <- mkContextWithUser defaultLang user1

  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file        <- saveNewFile (T.pack filename) filecontent

  let
    genDoc = addRandomDocumentWithFile
      file
      (rdaDefault user1)
        { rdaTypes          = OneOf [Signable]
        , rdaStatuses       = OneOf [Preparation]
        , rdaSignatories    = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                              in  OneOf $ map (`replicate` signatory) [1 .. 10]
        , rdaSealingMethods = OneOf [Guardtime]
        }
  withDocumentM genDoc $ do
    d       <- theDocument
    success <- randomUpdate $ ResetSignatoryDetails
      [ (defaultSignatoryLink
          { signatoryfields   = signatoryfields . fromJust $ getAuthorSigLink d
          , signatoryisauthor = True
          , signatoryrole     = SignatoryRoleViewer
          , maybesignatory    = Just $ user1 ^. #id
          }
        )
      , (defaultSignatoryLink
          { signatorysignorder          = SignOrder 1
          , signatoryisauthor           = False
          , signatoryrole               = SignatoryRoleSigningParty
          , signatorylinkauthenticationtosignmethod = SMSPinAuthenticationToSign
          , signatorylinkdeliverymethod = MobileDelivery
          , signatoryfields             = [ fieldForTests (NameFI (NameOrder 1)) "Fred"
                                          , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                          , fieldForTests EmailFI "fred@frog.com"
                                          , fieldForTests MobileFI "+47 666 111 777"
                                          ]
          }
        )
      ]
      (systemActor $ documentctime d)
    unless success $ unexpectedError "Expected True"

    req         <- mkRequest POST []
    (rdyrsp, _) <- (lift . runTestKontra req ctx) . apiCallV1Ready $ documentid d
    lift $ do
      assertEqual "Ready call was successful" 202 (rsCode rdyrsp)
      runSQL
          (   "SELECT * FROM chargeable_items WHERE type = 1 AND user_id ="
          <?> (user1 ^. #id)
          <+> "AND user_group_id ="
          <?> ugid1
          <+> "AND document_id ="
          <?> documentid d
          )
        >>= assertBool "Author and the company get charged for the delivery"
        .   (> 0)
    siglink <-
      head . filter isSignatoryAndHasNotSigned . documentsignatorylinks <$> theDocument

    mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid siglink)
                                             SignatoryAccessTokenForMailBeforeClosing
                                             Nothing
    pin <- dbQuery
      $ GetSignatoryPin SMSPinToSign (signatorylinkid siglink) (getMobile siglink)
    preq      <- mkRequest GET []
    (_, ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra preq ctx $ handleSignShowSaveMagicHash
        did
        (signatorylinkid siglink)
        mh


    req1           <- mkRequest POST [("fields", inText "[]"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra req1 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is not closed if no pin is provided" Pending
      .   documentstatus
      =<< theDocument

    req2 <- mkRequest
      POST
      [("fields", inText "[]"), ("pin", inText $ pin <> "4"), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra req2 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is not closed if pin is not valid" Pending
      .   documentstatus
      =<< theDocument

    req3 <- mkRequest POST [("fields", inText "[]"), ("pin", inText pin), signScreenshots]
    (_link, _ctx') <- updateDocumentWithID $ \did ->
      lift . runTestKontra req3 ctx' $ apiCallV1Sign did (signatorylinkid siglink)

    assertEqual "Document is closed if pin is valid" Closed
      .   documentstatus
      =<< theDocument

    -- make sure that smses are counted only for the designated company
    runSQL
        ("SELECT * FROM chargeable_items WHERE type = 1 AND user_id <>" <?> (user1 ^. #id)
        )
      >>= assertEqual "Users other than author don't get charged" 0

    runSQL
        ("SELECT * FROM chargeable_items WHERE type = 1 AND user_group_id <>" <?> ugid1)
      >>= assertEqual "Companies other than author's one don't get charged" 0


testSendReminderEmailUpdatesLastModifiedDate :: TestEnv ()
testSendReminderEmailUpdatesLastModifiedDate = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx <- mkContextWithUser defaultLang user

  doc <- addRandomDocument (rdaDefault user)
    { rdaStatuses    = OneOf [Pending]
    , rdaTypes       = OneOf [Signable]
    , rdaSignatories = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                       in  OneOf $ map (`replicate` signatory) [2 .. 10]
    }

  assertBool "Precondition" $ ctx ^. #time /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = last $ documentsignatorylinks doc
  req            <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req ctx
    $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is updated"
    $ compareTime (ctx ^. #time) (documentmtime updateddoc)
  emails <- dbQuery GetEmailsForTest
  assertEqual "Email was sent" 1 (length emails)

testSendReminderEmailByCompanyAdmin :: TestEnv ()
testSendReminderEmailByCompanyAdmin = do
  ugid      <- view #id <$> instantiateRandomUserGroup
  user      <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  otheruser <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  adminuser <- instantiateUser
    $ randomUserTemplate { groupID = return ugid, isCompanyAdmin = True }

  ctx      <- mkContextWithUser defaultLang user
  ctxadmin <- mkContextWithUser defaultLang adminuser
  ctxother <- mkContextWithUser defaultLang otheruser

  doc      <- addRandomDocument (rdaDefault user)
    { rdaStatuses    = OneOf [Pending]
    , rdaTypes       = OneOf [Signable]
    , rdaSignatories = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                       in  OneOf $ map (`replicate` signatory) [2 .. 10]
    }

  assertBool "Precondition" $ ctx ^. #time /= documentmtime doc

  -- who cares which one, just pick the last one
  let sl = last $ documentsignatorylinks doc

  -- fail if have no right to send reminder
  req1    <- mkRequest POST []
  result' <- E.try . runTestKontra req1 ctxother $ handleResend (documentid doc)
                                                                (signatorylinkid sl)

  case result' of
    Right _ ->
      assertFailure "Should not be able to resend when having no rights to do it"
    Left (_ :: E.SomeException) -> return ()


  updateddoc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is not updated"
    $ compareTime (documentmtime doc) (documentmtime updateddoc1)
  emails1 <- dbQuery GetEmailsForTest
  assertEqual "No emails were sent" 0 (length emails1)

  -- succeed to send a reminder as company admin
  req2           <- mkRequest POST []
  (_link, _ctx') <- runTestKontra req2 ctxadmin
    $ handleResend (documentid doc) (signatorylinkid sl)

  updateddoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  assertBool "Modified date is updated"
    $ compareTime (ctxadmin ^. #time) (documentmtime updateddoc)
  emails <- dbQuery GetEmailsForTest
  assertEqual "Email was sent" 1 (length emails)

testDownloadFile :: TestEnv ()
testDownloadFile = do
  ugid      <- view #id <$> instantiateRandomUserGroup
  user      <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  otheruser <- instantiateUser $ randomUserTemplate { groupID = return ugid }
  adminuser <- instantiateUser
    $ randomUserTemplate { groupID = return ugid, isCompanyAdmin = True }

  ctxnotloggedin <- mkContext defaultLang

  ctxuser        <- mkContextWithUser defaultLang user
  ctxuseronpad   <- set #maybePadUser (Just user) <$> mkContext defaultLang
  ctxadmin       <- mkContextWithUser defaultLang adminuser
  ctxother       <- mkContextWithUser defaultLang otheruser

  reqfile        <- mkRequest POST [("file", inFile $ inTestDir "pdfs/simple.pdf")]
  (_rsp, _ctx')  <- runTestKontra reqfile ctxuser apiCallV1CreateFromFile
  [doc]          <- randomQuery $ GetDocumentsByAuthor (user ^. #id)

  assertBool "Document access token should not be zero"
             (documentmagichash doc /= unsafeMagicHash 0)

  -- who cares which one, just pick the last one
  --let sl = head . reverse $ documentsignatorylinks doc
  let Just (fid :: FileID) = mainfileid <$> documentfile doc

  let cases =
        [ (False, ctxnotloggedin, [], "nobody is not logged in")
        , (True , ctxuser       , [], "user logged in is author")
        , (False, ctxuseronpad, [], "user on pad is author when document in Preparation")
        , ( False
          , ctxadmin
          , []
          , "user logged in is admin of author when document in Preparation"
          )
        , (False, ctxother, [], "user logged in is unrelated to document")
        , ( True
          , ctxnotloggedin
          , [("accesstoken", inText (showt (documentmagichash doc)))]
          , "using accesstoken, nobody logged in"
          )
        ]

  let sortOutResult apicall shouldallow res comment = case (shouldallow, res) of
        (True, Left (e :: E.SomeException)) -> do
          assertFailure
            $  "Should be able to download "
            <> apicall
            <> " when "
            <> comment
            <> ": "
            <> show e
        (True, Right (resp1, _ctx1)) | rsCode resp1 < 200 || rsCode resp1 >= 399 -> do
          assertFailure
            $  "Should be able to download "
            <> apicall
            <> " when "
            <> comment
            <> ":\n"
            <> show resp1
        (False, Right (resp1, _ctx1)) | rsCode resp1 >= 200 && rsCode resp1 <= 399 -> do
          assertFailure
            $  "Should not be able to download "
            <> apicall
            <> " when "
            <> comment
        _ -> return ()

  forM_ cases $ \(shouldallow, ctx, params, comment) -> do
    req1    <- mkRequest GET params
    result1 <- E.try . runTestKontra req1 ctx $ apiCallV1DownloadFile (documentid doc)
                                                                      fid
                                                                      "anything.pdf"
    sortOutResult "apiCallV1DownloadFile" shouldallow result1 comment

    result2 <- E.try . runTestKontra req1 ctx $ apiCallV1DownloadMainFile
      (documentid doc)
      "anything.pdf"

    sortOutResult "apiCallV1DownloadMainFile" shouldallow result2 comment

testDownloadFileWithAuthToView :: TestEnv ()
testDownloadFileWithAuthToView = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx <- mkContext defaultLang
  doc <- addRandomDocument (rdaDefault user)
    { rdaTypes       = OneOf [Signable]
    , rdaStatuses    = OneOf [Pending]
    , rdaSignatories = let signatory =
                             let authsToView =
                                     [toEnum 0 ..] \\ [StandardAuthenticationToView]
                             in  OneOf . (`map` authsToView) $ \auth ->
                                   [RSC_IsSignatoryThatHasntSigned, RSC_AuthToViewIs auth]
                       in  OneOf [[randomSignatory, signatory]]
    }
  let sl = last (documentsignatorylinks doc)
  req1 <- mkRequest GET []
  mh   <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid sl)
                                             SignatoryAccessTokenForMailBeforeClosing
                                             Nothing
  (_, ctx') <- runTestKontra req1 ctx
    $ handleSignShowSaveMagicHash (documentid doc) (signatorylinkid sl) mh
  req2      <- mkRequest GET [("signatorylinkid", inText $ showt (signatorylinkid sl))]
  (res2, _) <- runTestKontra req2 ctx'
    $ apiCallV1DownloadMainFile (documentid doc) "anything.pdf"
  assertEqual "Response should be 403" 403 (rsCode res2)

testSendingReminderClearsDeliveryInformation :: TestEnv ()
testSendingReminderClearsDeliveryInformation = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx <- mkContextWithUser defaultLang user
  let genDoc = addRandomDocument (rdaDefault user) { rdaTypes    = OneOf [Signable]
                                                   , rdaStatuses = OneOf [Pending]
                                                   }
  withDocumentM genDoc $ do
    sl <- last . documentsignatorylinks <$> theDocument
    let actor = systemActor $ ctx ^. #time
    void . dbUpdate $ MarkInvitationRead (signatorylinkid sl) actor
    -- who cares which one, just pick the last one
    req            <- mkRequest POST []
    (_link, _ctx') <- do
      updateDocumentWithID $ \did ->
        (lift . runTestKontra req ctx) . withDocumentID did $ sendReminderEmail Nothing
                                                                                actor
                                                                                False
                                                                                sl
    sl' <-
      guardJustM
      $   find (\t -> signatorylinkid t == signatorylinkid sl)
      .   documentsignatorylinks
      <$> theDocument
    assertEqual "Invitation is not delivered" Unknown (mailinvitationdeliverystatus sl')


testDocumentFromTemplate :: TestEnv ()
testDocumentFromTemplate = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "aaa"
                                               , lastName  = return "bbb"
                                               , email     = return "xxx@xxx.pl"
                                               }
  doc   <- addRandomDocument (rdaDefault user) { rdaTypes = OneOf [Template] }
  docs1 <- randomQuery $ GetDocumentsByAuthor (user ^. #id)
  ctx   <- mkContextWithUser defaultLang user
  req   <- mkRequest POST []
  void . runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
  docs2 <- randomQuery $ GetDocumentsByAuthor (user ^. #id)
  assertBool "No new document" (length docs2 == 1 + length docs1)

testDocumentFromTemplateShared :: TestEnv ()
testDocumentFromTemplateShared = do
  author <- instantiateRandomUser
  doc    <- addRandomDocument (rdaDefault author) { rdaTypes = OneOf [Template] }
  void . randomUpdate $ SetDocumentSharing [documentid doc] True
  user  <- instantiateUser $ randomUserTemplate { groupID = return $ author ^. #groupID }
  docs1 <- randomQuery $ GetDocumentsByAuthor (user ^. #id)
  ctx   <- mkContextWithUser defaultLang user
  req   <- mkRequest POST []
  void . runTestKontra req ctx $ apiCallV1CreateFromTemplate (documentid doc)
  docs2 <- randomQuery $ GetDocumentsByAuthor (user ^. #id)
  assertEqual "New document should have been created" (1 + length docs1) (length docs2)

testDocumentDeleteInBulk :: TestEnv ()
testDocumentDeleteInBulk = do
  author <- instantiateRandomUser
  -- isSignable condition below is wrong. Tests somehow generate template documents
  -- that are pending and that breaks everything.
  docs   <- replicateM
    100
    (addRandomDocument (rdaDefault author) { rdaTypes = OneOf [Signable] })

  ctx <- mkContextWithUser defaultLang author
  req <- mkRequest POST [("documentids", inText (showt $ documentid <$> docs))]

  void $ runTestKontra req ctx handleDelete
  docs2 <- dbQuery $ GetDocumentsByAuthor (author ^. #id)
  assertEqual "Documents are deleted" 0 (length docs2)

testGetLoggedIn :: TestEnv ()
testGetLoggedIn = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc      <- addRandomDocumentWithAuthor user
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest GET []
  (res, _) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 200" 200 (rsCode res)


testGetNotLoggedIn :: TestEnv ()
testGetNotLoggedIn = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc      <- addRandomDocumentWithAuthor user
  ctx      <- mkContext defaultLang
  req      <- mkRequest GET []
  (res, _) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


testGetBadHeader :: TestEnv ()
testGetBadHeader = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc      <- addRandomDocumentWithAuthor user
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequestWithHeaders GET [] [("authorization", ["ABC"])]
  (res, _) <- runTestKontra req ctx $ apiCallV1Get doc
  assertEqual "Response code is 403" 403 (rsCode res)


-- Testing access to evidence documentation
testGetEvidenceAttachmentsLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsLoggedIn = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc      <- addRandomDocumentWithAuthor user
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest GET []
  (res, _) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 200" 200 (rsCode res)

testGetEvidenceAttachmentsNotLoggedIn :: TestEnv ()
testGetEvidenceAttachmentsNotLoggedIn = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc      <- addRandomDocumentWithAuthor user
  ctx      <- mkContext defaultLang
  req      <- mkRequest GET []
  (res, _) <- runTestKontra req ctx $ apiCallV1GetEvidenceAttachments doc
  assertEqual "Response code is 403" 403 (rsCode res)

-- Some test for signview branding generation

testSignviewBrandingBlocksNastyInput :: TestEnv ()
testSignviewBrandingBlocksNastyInput = do
  bd               <- view #brandedDomain <$> mkContext defaultLang -- We need to get default branded domain. it can be fetched from default ctx
  theme            <- dbQuery . GetTheme $ bd ^. #signviewTheme
  emptyBrandingCSS <- signviewBrandingCSS theme
  assertBool "CSS generated for empty branding is not empty"
             (not $ BSL.null emptyBrandingCSS)
  let
    nasty1 = "nastyColor \n \n"
    nasty2 = "alert('Nasty color')"
    nasty3 = "& very nasty font {}"
    nastyTheme =
      theme { themeBrandColor = nasty1, themeBrandTextColor = nasty2, themeFont = nasty3 }
  nastyCSS <- signviewBrandingCSS nastyTheme
  assertBool "CSS generated for nasty company branding is not empty"
             (not $ BSL.null nastyCSS)
  assertBool "CSS generated for nasty company branding does not contain nasty strings"
    $  not (T.unpack nasty1 `isInfixOf` BSL.toString nastyCSS)
    && not (T.unpack nasty2 `isInfixOf` BSL.toString nastyCSS)
    && not (T.unpack nasty3 `isInfixOf` BSL.toString nastyCSS)

testDownloadSignviewBrandingAccess :: TestEnv ()
testDownloadSignviewBrandingAccess = do
  -- Create file and make it ready for signing
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file        <- saveNewFile (T.pack filename) filecontent

  doc         <- addRandomDocumentWithFile
    file
    (rdaDefault user)
      { rdaTypes       = OneOf [Signable]
      , rdaStatuses    = OneOf [Preparation]
      , rdaSignatories = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                         in  OneOf $ map (`replicate` signatory) [1 .. 10]
      }

  withDocumentID (documentid doc) $ do
    d <- theDocument
    void . randomUpdate $ ResetSignatoryDetails
      [ (defaultSignatoryLink
          { signatoryfields   = signatoryfields . fromJust $ getAuthorSigLink d
          , signatoryisauthor = True
          , signatoryrole     = SignatoryRoleViewer
          , maybesignatory    = Just $ user ^. #id
          }
        )
      , (defaultSignatoryLink
          { signatorysignorder = SignOrder 1
          , signatoryisauthor  = False
          , signatoryrole      = SignatoryRoleSigningParty
          , signatoryfields    = [ fieldForTests (NameFI (NameOrder 1)) "Fred"
                                 , fieldForTests (NameFI (NameOrder 2)) "Frog"
                                 , fieldForTests EmailFI                "fred@frog.com"
                                 ]
          }
        )
      ]
      (systemActor $ documentctime d)
    t  <- documentctime <$> theDocument
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor t) tz


  -- We have a document, now proper tests will take place

  -- 1) Check access to main signview branding
  emptyContext <- mkContext defaultLang
  let bid = emptyContext ^. #brandedDomain % #id
  svbr1 <- mkRequest GET []
  resp1 <- E.try . runTestKontra svbr1 emptyContext $ handleSignviewBranding
    bid
    (documentid doc)
    "branding-hash-12xdaad32-some_name.css"
  case resp1 of
    Right (cssResp1, _) -> assertBool "CSS should be returned" (rsCode cssResp1 == 200)
    Left (_ :: E.SomeException) -> assertFailure "CSS should be avaialbe for CDN"


  -- 2) Check access to main signview branding for author. Used when logged in a to-sign view.

  svbr2 <- mkRequest GET []
  resp2 <-
    E.try . runTestKontra svbr2 emptyContext $ handleSignviewBrandingWithoutDocument
      bid
      (user ^. #id)
      "branding-hash-7cdsgSAq1-some_name.css"
  case resp2 of
    Right (cssResp2, _) -> assertBool "CSS should be returned" (rsCode cssResp2 == 200)
    Left (_ :: E.SomeException) -> assertFailure "CSS should be avaialbe for CDN"

testGetCancelledDocument :: TestEnv ()
testGetCancelledDocument = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  ctx <- mkContext defaultLang
  doc <- addRandomDocument (rdaDefault user) { rdaTypes    = OneOf [Signable]
                                             , rdaStatuses = OneOf [Pending]
                                             }
  let did       = documentid doc
      signatory = head $ documentsignatorylinks doc
      slid      = signatorylinkid signatory

  withDocument doc . randomUpdate $ CancelDocument (authorActor ctx user)

  -- It should fail if we're using the link with a magic hash.
  do
    req  <- mkRequest GET []
    mh   <- dbUpdate $ NewSignatoryAccessToken slid SignatoryAccessTokenForAPI Nothing
    eRes <- E.try . runTestKontra req ctx $ handleSignShowSaveMagicHash did slid mh

    case eRes of
      Right (res, _) ->
        assertFailure $ "Should have failed, returned code " <> show (rsCode res)
      Left err -> assertEqual "Should throw LinkInvalid" LinkInvalid err

  -- It shouldn't fail if we had already clicked on the link.
  do
    req      <- mkRequest GET []
    (res, _) <- runTestKontra req ctx $ do
      sid <- getNonTempSessionID
      randomUpdate $ AddDocumentSession sid slid
      handleSignShow did slid
    assertEqual "Status is 200" 200 (rsCode res)

testDocumentFromShareableTemplate :: TestEnv ()
testDocumentFromShareableTemplate = replicateM_ 10 $ do
  user <- instantiateRandomUser
  tpl  <- addRandomDocument (rdaDefault user)
    { rdaTypes       = OneOf [Template]
    , rdaSignatories = let author = OneOf
                             [ [ RSC_IsViewer
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               ]
                             ]
                           signatory = OneOf
                             [ [ RSC_IsSignatoryThatHasntSigned
                               , RSC_AuthToViewIs StandardAuthenticationToView
                               , RSC_AuthToSignIs StandardAuthenticationToSign
                               , RSC_DeliveryMethodIs APIDelivery
                               ]
                             ]
                       in  OneOf [[author, signatory]]
    }

  mh <- random
  withDocument tpl . randomUpdate $ UpdateShareableLinkHash (Just mh)

  ctx         <- mkContext defaultLang
  req         <- mkRequest GET []
  (res, ctx') <- runTestKontra req ctx $ handleSignFromTemplate (documentid tpl) mh

  assertEqual "Status is 303" 303 (rsCode res)
  let Just HeaderPair { hValue = [loc] } = M.lookup "location" $ rsHeaders res

      (did, slid) :: (DocumentID, SignatoryLinkID) =
        bimap read (read . T.drop 1)
          . T.break (== '/')
          . T.drop (T.length "/s/")
          . T.pack
          . BS8.unpack
          $ loc

  doc <- randomQuery $ GetDocumentByDocumentID did
  assertNotEqual "Should be a different document" (documentid doc) (documentid tpl)
  assertEqual "Should have the same title" (documenttitle doc) (documenttitle tpl)
  assertEqual "Shouldn't have a shareable link hash"
              Nothing
              (documentshareablelinkhash doc)
  assertEqual "Should have a reference to the template"
              (Just (documentid tpl))
              (documenttemplateid doc)
  assertEqual "Should be marked as created by a shareable link"
              True
              (documentfromshareablelink doc)

  (res', ctx'') <- runTestKontra req ctx' $ handleSignShow did slid
  assertEqual "Status is 200" 200 (rsCode res')

  req'       <- mkRequest POST [("fields", inText "[]")]
  (res'', _) <- runTestKontra req' ctx'' $ apiCallV1Sign did slid
  assertEqual "Status is 202" 202 (rsCode res'')

  forM_ [CIStartingDocument, CIShareableLink, CIClosingDocument] $ \typ -> do
    runQuery_
      $   "SELECT COUNT(*) FROM chargeable_items WHERE type ="
      <?> typ
      <+> "AND document_id ="
      <?> did
    c <- fetchOne runIdentity
    assertEqual ("Should have been charged with " <> show typ) 1 (c :: Int64)

testGetDocumentWithSignatoryAccessTokens :: TestEnv ()
testGetDocumentWithSignatoryAccessTokens = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }

  doc <- addRandomDocument (rdaDefault user) { rdaTypes    = OneOf [Signable]
                                             , rdaStatuses = OneOf [Pending]
                                             }
  let did       = documentid doc
      signatory = head $ documentsignatorylinks doc
      slid      = signatorylinkid signatory

  now <- currentTime
  let expiration = 2 `daysAfter` now
  mh <- dbUpdate $ NewSignatoryAccessToken slid
                                           SignatoryAccessTokenForMailBeforeClosing
                                           (Just expiration)

  do
    ctx      <- mkContext defaultLang
    req      <- mkRequest GET []
    (res, _) <- runTestKontra req ctx $ handleSignShowSaveMagicHash did slid mh
    assertEqual "Status is 303" 303 (rsCode res)

  setTestTime $ 3 `daysAfter` now

  do
    ctx  <- mkContext defaultLang
    req  <- mkRequest GET []
    eRes <- E.try . runTestKontra req ctx $ handleSignShowSaveMagicHash did slid mh

    case eRes of
      Right (res, _) ->
        assertFailure $ "Should have failed, returned code " <> show (rsCode res)
      Left err -> assertEqual "Should throw LinkInvalid" LinkInvalid err

  do
    ctx <- mkContext defaultLang
    runSQL_
      "UPDATE cron_jobs SET run_at = to_timestamp(0)\
            \ WHERE id = 'timeouted_signatory_access_tokens_purge'"
    runTestCronUntilIdle ctx
    runQuery_
      $ "SELECT COUNT(*) FROM signatory_access_tokens\
                \ WHERE signatory_link_id ="
      <?> slid
    c <- fetchOne runIdentity
    assertEqual "Signatory_access tokens count should be 0" (0 :: Int64) c

testSendEmailOnTimeout :: TestEnv ()
testSendEmailOnTimeout = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  True <- dbUpdate $ SetUserUserGroup (user ^. #id) (ug ^. #id)
  let newUGS = set #sendTimeoutNotification True (fromJust $ ug ^. #settings)
  dbUpdate $ UserGroupUpdateSettings (ug ^. #id) (Just newUGS)

  doc <- addRandomDocument (rdaDefault user) { rdaTypes       = OneOf [Signable]
                                             , rdaStatuses    = OneOf [Pending]
                                             , rdaTimeoutTime = True
                                             }

  modifyTestTime (const (10 `minutesAfter` fromJust (documenttimeouttime doc)))
  ctx <- mkContext defaultLang
  runSQL_
    "UPDATE cron_jobs SET run_at = to_timestamp(0)\
            \ WHERE id = 'find_and_timeout_documents'"
  runTestCronUntilIdle ctx
  doc2 <- dbQuery . GetDocumentByDocumentID $ documentid doc

  assertEqual "Document should be timeouted" Timedout (documentstatus doc2)

  runSQL_ "SELECT COUNT(*) FROM mails WHERE receivers ILIKE '%bob@blue.com%'"
  c <- fetchOne runIdentity
  assertEqual "One mail should be sent to the author" (1 :: Int64) c


testShortLinkRedirectTest :: TestEnv ()
testShortLinkRedirectTest = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Bob"
                                               , lastName  = return "Blue"
                                               , email     = return "bob@blue.com"
                                               }
  doc <- addRandomDocument (rdaDefault user)
    { rdaStatuses    = OneOf [Pending]
    , rdaTypes       = OneOf [Signable]
    , rdaSignatories = let signatory = OneOf [[RSC_DeliveryMethodIs EmailDelivery]]
                       in  OneOf $ map (`replicate` signatory) [2 .. 10]
    }
  let sl = last $ documentsignatorylinks doc
  ctx               <- mkContext defaultLang
  (mh :: MagicHash) <- rand 10 arbitrary -- We don't check mh value here at all
  req               <- mkRequest GET []
  let goodParam = SMSLinkShortening.short (signatorylinkid sl, mh)
  (res1, _) <- runTestKontra req ctx $ handleSignShowShortRedirect goodParam
  assertEqual "handleSignShowShortRedirect redirects with 303" 303 (rsCode res1)
  assertRaises404 (void . runTestKontra req ctx $ handleSignShowShortRedirect "bad_param")
