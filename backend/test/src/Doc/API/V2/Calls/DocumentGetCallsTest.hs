module Doc.API.V2.Calls.DocumentGetCallsTest (apiV2DocumentGetCallsTests) where

import Control.Monad.IO.Class
import Data.Time (UTCTime(..), addUTCTime, fromGregorian)
import Happstack.Server
import Log
import Test.Framework
import qualified Data.Text as T

import BrandedDomain.BrandedDomain
import Context
import DB.Query (dbUpdate)
import DB.TimeZoneName (mkTimeZoneName)
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.DocumentGetCalls
import Doc.API.V2.Calls.DocumentPostCalls
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.Mock.TestUtils
import Doc.DocInfo
import Doc.DocumentID (DocumentID, unsafeDocumentID)
import Doc.DocumentMonad (withDocument, withDocumentID)
import Doc.Model.Query (GetDocumentByDocumentID(..))
import Doc.Model.Update
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots (emptySignatoryScreenshots)
import Doc.Tokens.Model
import Doc.Types.Document
import Doc.Types.DocumentStatus (DocumentStatus(..))
import Doc.Types.SignatoryAttachment
  ( SignatoryAttachment(..), defaultSignatoryAttachment )

import Doc.Types.SignatoryLink
  ( AuthenticationToSignMethod(..), SignatoryLink(..) )

import File.Storage (saveNewFile)
import Session.Model
import TestingUtil
import TestKontra
import User.Lang (defaultLang)
import UserGroup.Types
import Util.Actor
import Util.QRCode

apiV2DocumentGetCallsTests :: TestEnvSt -> Test
apiV2DocumentGetCallsTests env = testGroup "APIv2DocumentGetCalls" $
  [ testThat "API v2 List"                  env testDocApiV2List
  , testThat "API v2 Get"                   env testDocApiV2Get
  , testThat "API v2 Get by shortcode"      env testDocApiV2GetShortCode
  , testThat "API v2 Get QR code"           env testDocApiV2GetQRCode
  , testThat "API v2 Get by Company Admin"  env testDocApiV2GetByAdmin
  , testThat "API v2 Get for Shared doc"    env testDocApiV2GetShared
  , testThat "API v2 History"               env testDocApiV2History
  , testThat "API v2 History (permissions)" env testDocApiV2HistoryPermissionCheck
  , testThat "API v2 Evidence attachments"  env testDocApiV2EvidenceAttachments
  , testThat "API v2 Files - Main"          env testDocApiV2FilesMain
  , testThat "API v2 Files - Pages"         env testDocApiV2FilesPages
  , testThat "API v2 Files - Get"           env testDocApiV2FilesGet
  , testThat "API v2 Files - Full"          env testDocApiV2FilesFull
--  , testThat "API v2 Get - Not after 30 days for signatories" env testDocApiV2GetFailsAfter30Days
  ]

testDocApiV2List :: TestEnv ()
testDocApiV2List = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang

  void $ testDocApiV2New' ctx
  void $ testDocApiV2New' ctx
  void $ testDocApiV2New' ctx

  listJSON <- jsonTestRequestHelper ctx GET [] docApiV2List 200
  listArray <- lookupObjectArray "documents" listJSON
  assertEqual "`docApiV2List` should return same number of docs" 3 (length listArray)
  let docs = map mockDocFromValue $ listArray
  forM_ docs $ \d -> assertEqual "Status should be" Preparation (getMockDocStatus d)

testDocApiV2Get :: TestEnv ()
testDocApiV2Get = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2New' ctx
  let did = getMockDocId newMockDoc

  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Mock Document from `docApiV2Get` should match from `docApiV2New`" getMockDoc newMockDoc
  assertEqual "Document viewer should be" "signatory" (getMockDocViewerRole getMockDoc)

-- temporarily disabled
_testDocApiV2GetFailsAfter30Days :: TestEnv ()
_testDocApiV2GetFailsAfter30Days = do
  user <- addNewRandomUser
  let check s = isNothing (maybesignatory s) && not (signatoryisauthor s)
  doc <- addRandomDocumentWithAuthorAndCondition user $ \doc ->
    isClosed doc && any check (documentsignatorylinks doc)
  let Just sl = find check (documentsignatorylinks doc)

  req <- mkRequest GET []
  ctx <- anonymiseContext <$> mkContext defaultLang
  (_, ctx') <- runTestKontra req ctx $ do
    sid <- getNonTempSessionID
    dbUpdate $ AddDocumentSession sid (signatorylinkid sl)
  let vars = [ ("signatory_id", inText . showt . fromSignatoryLinkID . signatorylinkid $ sl)
             ]
      ctxWithin30Days = set ctxtime (addUTCTime (29*24*3600) (documentmtime doc)) ctx'
      ctxAfter30Days  = set ctxtime (addUTCTime (31*24*3600) (documentmtime doc)) ctx'
  void $ testRequestHelper ctxWithin30Days GET vars (docApiV2Get (documentid doc)) 200
  void $ testRequestHelper ctxAfter30Days  GET vars (docApiV2Get (documentid doc)) 410

  return ()

mockDocToShortID :: MockDoc -> DocumentID
mockDocToShortID md = read $ T.pack $ reverse $ take 6 $ reverse $ show (getMockDocId md)

testDocApiV2GetShortCode :: TestEnv ()
testDocApiV2GetShortCode = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2StartNew ctx
  let shortDid = mockDocToShortID newMockDoc

  -- Test that everything works normally...
  getMockDoc <- mockDocTestRequestHelper ctx GET []
    (docApiV2GetByShortID shortDid) 200
  assertEqual "Mock document from `docApiV2Get` should match from `docApiV2New`"
    getMockDoc newMockDoc
  assertEqual "Document viewer should be" "signatory"
    (getMockDocViewerRole getMockDoc)

  -- Now test some failure cases...

  -- We reuse getRequest.
  getRequest <- mkRequestWithHeaders GET [] []

  testMallory getRequest (docApiV2GetByShortID shortDid)

  -- Short DocID should be <= 6 digits
  (resLong,_) <- runTestKontra getRequest ctx $ docApiV2GetByShortID (unsafeDocumentID 1234567)
  assertEqual "We should get a 400 response for a long docid"
    400 (rsCode resLong)

  -- Should not work for documents older than 24h, make the document "old"
  withDocumentID (getMockDocId newMockDoc) $ do
    updateMTimeAndObjectVersion $
      UTCTime { utctDay = fromGregorian 2000 1 1, utctDayTime = 0 }
  (resOld,_) <- runTestKontra getRequest ctx $ docApiV2GetByShortID shortDid
  assertEqual "We should get a 404 response for old documents"
    404 (rsCode resOld)

  -- Should not work for other document status, we only test preparation here...
  prepDoc <- testDocApiV2New' ctx
  (resPrep,_) <- runTestKontra getRequest ctx $ docApiV2GetByShortID (mockDocToShortID prepDoc)
  assertEqual "We should get a 409 response for a document in Preparation"
    409 (rsCode resPrep)

-- | A malicious Mallory should not be able to execute this request.
testMallory :: Request -> KontraTest Response -> TestEnv ()
testMallory getRequest req = do
  mallory <- addNewRandomUser
  ctxMallory <- (set ctxmaybeuser (Just mallory)) <$> mkContext defaultLang
  (resMallory,_) <- runTestKontra getRequest ctxMallory $ req
  assertEqual "We should get a 403 response for someone else's document"
    403 (rsCode resMallory)

testDocApiV2GetQRCode :: TestEnv ()
testDocApiV2GetQRCode = do
  user <- addNewRandomUser
  ctx  <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  newMockDoc <- testDocApiV2StartNew ctx
  let did  = getMockDocId newMockDoc
      slid = getMockDocSigLinkId 1 newMockDoc

  -- Test that everything works normally...
  forM_ [ "https://scrive.com", "http://scrive.com", "http://scrive.com:9000"
        , "scrive.com", "scrive.com:9000", "localhost:8000" ] $ \domain -> do
    let ctx' = set (bdUrl . ctxbrandeddomain) domain $ ctx
    getQRCode <- testRequestHelper ctx' GET [] (docApiV2GetQRCode did slid) 200
    getURL    <- liftIO $ decodeQRBSL getQRCode
    logInfo_ $ "Decoded QR code: " <> (T.pack getURL)
    let (urlScheme, rest)    = splitAt 9 getURL
        (server,    rest')   = span (/= '/') rest
        (docID,     rest'')  = span (/= '/') $ drop 3 rest'
        (sigID,     rest''') = span (/= '/') $ tail rest''
        _token               = tail rest'''
        domain'              = stripUrlScheme $ T.unpack domain
        stripUrlScheme   url = case break (== ':') url of
          (_protocol, ':':'/':'/':srv) -> srv
          _                            -> url

    assertEqual "URL scheme must be `scrive://`" "scrive://"  urlScheme
    assertEqual ("Server name must be `" <> domain' <> "`") domain' server
    assertEqual "Doc ID from `getqrcode` should match the one from `new`"
      (getMockDocId newMockDoc) (read $ T.pack docID)

    assertEqual "Signatory link ID should match the one from `new`"
      slid (read $ T.pack sigID)


  -- Next, test some failure cases...

  -- We reuse getRequest.
  getRequest <- mkRequestWithHeaders GET [] []

  -- Access control.
  testMallory getRequest (docApiV2GetQRCode did slid)

  -- Should not work for other document status, we only test preparation here...
  prepDoc <- testDocApiV2New' ctx
  (resPrep,_) <- runTestKontra getRequest ctx $
                 docApiV2GetQRCode (getMockDocId prepDoc)
                 (getMockDocSigLinkId 1 prepDoc)
  assertEqual "We should get a 409 response for a document in Preparation"
    409 (rsCode resPrep)


testDocApiV2GetByAdmin :: TestEnv ()
testDocApiV2GetByAdmin = do
  ug <- addNewUserGroup
  let ugid = (get ugID ug)
  author <- addNewRandomCompanyUser ugid False
  ctxauthor <- (set ctxmaybeuser (Just author)) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctxauthor

  admin <- addNewRandomCompanyUser ugid True
  ctx <- (set ctxmaybeuser (Just admin)) <$> mkContext defaultLang
  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Document viewer should be" "company_admin" (getMockDocViewerRole getMockDoc)

testDocApiV2GetShared :: TestEnv ()
testDocApiV2GetShared = do
  ug <- addNewUserGroup
  let ugid = (get ugID ug)
  author <- addNewRandomCompanyUser ugid False
  ctxauthor <- (set ctxmaybeuser (Just author)) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctxauthor

  void $ mockDocTestRequestHelper ctxauthor POST [("document", inText "{\"is_template\":true}")] (docApiV2Update did) 200
  setshare <- withDocumentID did $ do
    dbUpdate $ SetDocumentSharing [did] True
  assert setshare

  user <- addNewRandomCompanyUser ugid False
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  getMockDoc <- mockDocTestRequestHelper ctx GET [] (docApiV2Get did) 200
  assertEqual "Document viewer should be" "company_shared" (getMockDocViewerRole getMockDoc)
  assertEqual "Document should be template" True (getMockDocIsTemplate getMockDoc)
  assertEqual "Document should be shared" True (getMockDocIsShared getMockDoc)

testDocApiV2History :: TestEnv ()
testDocApiV2History = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  did <- getMockDocId <$> testDocApiV2New' ctx

  let checkHistoryHasNItems :: Int -> TestEnv ()
      checkHistoryHasNItems n = do
        historyJSON <- jsonTestRequestHelper ctx GET [] (docApiV2History did) 200
        historyArray <- lookupObjectArray "events" historyJSON
        assertEqual "History did not have same number of events" n (length historyArray)

  checkHistoryHasNItems 0

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Start did) 200
  checkHistoryHasNItems 1

  void $ mockDocTestRequestHelper ctx POST [] (docApiV2Cancel did) 200
  checkHistoryHasNItems 2

testDocApiV2HistoryPermissionCheck :: TestEnv ()
testDocApiV2HistoryPermissionCheck = do
  userAuthor <- addNewRandomUser
  userOther <- addNewRandomUser
  ctxWithAuthor <- (set ctxmaybeuser (Just userAuthor)) <$> mkContext defaultLang
  ctxWithOtherUser <- (set ctxmaybeuser (Just userOther)) <$> mkContext defaultLang

  did <- getMockDocId <$> testDocApiV2New' ctxWithAuthor

  void $ jsonTestRequestHelper ctxWithAuthor GET [] (docApiV2History did) 200
  void $ jsonTestRequestHelper ctxWithOtherUser GET [] (docApiV2History did) 403
  return ()


testDocApiV2EvidenceAttachments :: TestEnv ()
testDocApiV2EvidenceAttachments = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  mockDoc <- testDocApiV2StartNew ctx
  let did = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  void $ mockDocTestRequestHelper ctx
    POST
      [ ("fields", inText "[]")
      , ("accepted_author_attachments", inText "[]")
      , ("consent_responses", inText "[]")
      ]
    (docApiV2SigSign did slid) 200
  sealTestDocument ctx did

  eaJSON <- jsonTestRequestHelper ctx GET [] (docApiV2EvidenceAttachments did) 200
  eaList <- lookupObjectArray "attachments" eaJSON
  attachmentNames <- forM eaList (lookupObjectString "name")
  assertEqual "Evidence attachment file names and order should match"
    [ "Evidence Quality of Scrive E-signed Documents.html"
    , "Appendix 1 Evidence Quality Framework.html"
    , "Appendix 2 Service Description.html"
    , "Appendix 3 Evidence Log.html"
    , "Appendix 4 Evidence of Time.html"
    , "Appendix 5 Evidence of Intent.html"
    , "Appendix 6 Digital Signature Documentation.html"
    ]
    attachmentNames

testDocApiV2FilesMain :: TestEnv ()
testDocApiV2FilesMain = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  doc <- testDocApiV2New' ctx
  let did = getMockDocId doc

  -- Normal GET request
  getReq ctx did [] "(standard)" 200

  -- GET request via access token
  let ctx' = set ctxmaybeuser Nothing ctx
      vars = [ ("access_token"
               , inText . T.pack . getMockDocAccessToken $ doc) ]
  getReq ctx' did []   "(no access token - expected failure)" 401
  getReq ctx' did vars "(access token)" 200


  where
    getReq ctx did vars desc expected_code = do
      req <- mkRequest GET vars
      (rsp,_) <- runTestKontra req ctx $ docApiV2FilesMain did
      assertEqual ("Successful `docApiV2FilesMain` " ++ desc ++ " response code")
        expected_code (rsCode rsp)

testDocApiV2FilesPages :: TestEnv ()
testDocApiV2FilesPages = do
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  doc <- testDocApiV2New' ctx
  let did = getMockDocId doc
      fid = getMockDocFileId doc

  -- GET number of pages
  rspJSON <- jsonTestRequestHelper ctx
    GET [] (docApiV2FilesPagesCount did fid) 200
  pagecount <- lookupObjectInt "pages" rspJSON

    -- GET the last page image
  req2 <- mkRequest GET [("pixelwidth", inText "200")]
  (rsp2,_) <- runTestKontra req2 ctx $ docApiV2FilesPage did fid pagecount
  assertEqual ("Successful `docApiV2FilesPage` response code")
    200 (rsCode rsp2)

testDocApiV2FilesGet :: TestEnv ()
testDocApiV2FilesGet = do
  user <- addNewRandomUser
  ctx  <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  doc  <- testDocApiV2New' ctx
  let did = getMockDocId doc

  mockDocSet <- mockDocTestRequestHelper ctx POST [
      ("attachments", inText $ "[{\"name\" : \"simple-rotate-90.pdf\", \"required\" : false, \"add_to_sealed_file\" : true, \"file_param\" : \"afile\"}]")
    , ("afile", inFile $ inTestDir "pdfs/simple-rotate-90.pdf")
    ] (docApiV2SetAttachments did) 200

  let fid = getMockDocAuthorAttachmentFileId 1 mockDocSet

  -- Normal GET request
  getReq ctx did fid [] "(standard)" 200

  -- GET request via access token
  let ctx' = set ctxmaybeuser Nothing ctx
      vars = [ ("access_token"
               , inText . T.pack . getMockDocAccessToken $ doc) ]
  getReq ctx' did fid []   "(no access token - expected failure)" 401
  getReq ctx' did fid vars "(access token)" 200


  where
    getReq ctx did fid vars desc expected_code = do
      req <- mkRequest GET vars
      (rsp,_) <- runTestKontra req ctx $ docApiV2FilesGet did fid Nothing
      assertEqual ("Successful `docApiV2FilesGet` " ++ desc ++ " response code")
        expected_code (rsCode rsp)

testDocApiV2FilesFull :: TestEnv ()
testDocApiV2FilesFull = do
  now  <- currentTime
  user <- addNewRandomUser
  ctx  <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req  <- mkRequest GET []

  initDoc <- addRandomDocument (randomDocumentAllowsDefault user)
    { randomDocumentTypes = Or [Signable]
    , randomDocumentStatuses = Or [Preparation]
    , randomDocumentSignatories =
        let author = Or
              [ And [RSC_IsViewer]
              ]
            signatory = Or
              [ And [ RSC_IsSignatoryThatHasntSigned
                    , RSC_AuthToSignIs StandardAuthenticationToSign
                    ]
              ]
        in Or [[author, signatory]]
    }
  let did = documentid initDoc
      att  = defaultSignatoryAttachment { signatoryattachmentname = "sig_att" }

  -- Add attachments to the generated document.
  void $ do
    let mkFile name add =
          "{\"name\":\"" <> name
          <> ".pdf\",\"required\":false,\"add_to_sealed_file\":"
          <> (if add then "true" else "false")
          <> ",\"file_param\":\"" <> name <> "_file\"}"
        vars = [ ( "attachments", inText $ "[" <> mkFile "merged" True <> ","
                                           <> mkFile "not_merged" False <> "]" )
               , ( "merged_file"
                 , inFile $ inTestDir "pdfs/simple-rotate-90.pdf" )
               , ( "not_merged_file"
                 , inFile $ inTestDir "pdfs/simple-rotate-90.pdf" )
               ]
    void $ testRequestHelper ctx POST vars
                           (docApiV2SetAttachments (documentid initDoc)) 200

    let [sl1, sl2] = documentsignatorylinks initDoc
        sl2' = sl2 { signatoryattachments = [att] }
        sls  = [sl1, sl2']
    withDocumentID did $ randomUpdate $ ResetSignatoryDetails sls $ systemActor now

  doc <- randomQuery $ GetDocumentByDocumentID did

  do
    (files, _) <- runTestKontra req ctx $ docApiV2FilesFullForTests doc
    assertEqual "Main file + two attachments" 3 (length files)
    assertBool "Has merged.pdf" $ "merged.pdf" `elem` map fst files
    assertBool "Has not_merged.pdf" $ "not_merged.pdf" `elem` map fst files

  -- Sign and upload signatory attachment but don't close.
  withDocument doc $ do
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor now) tz
    let [_, sl] = documentsignatorylinks doc
    fid <- saveNewFile "some_name.pdf" "Some contents"
    randomUpdate $ SaveSigAttachment (signatorylinkid sl) att fid
                                     (systemActor now)
    randomUpdate $ SignDocument
      (signatorylinkid sl) Nothing Nothing emptySignatoryScreenshots
      (systemActor now)

  do
    signedDoc <- randomQuery $ GetDocumentByDocumentID did
    (files, _) <- runTestKontra req ctx $ docApiV2FilesFullForTests signedDoc
    assertEqual "Main file + two attachments + one signatory attachment"
                4 (length files)
    assertBool "Has merged.pdf" $ "merged.pdf" `elem` map fst files
    assertBool "Has not_merged.pdf" $ "not_merged.pdf" `elem` map fst files
    assertBool "Has sig_att.pdf" $ "sig_att.pdf" `elem` map fst files

  -- Close the document.
  withDocumentID did $ randomUpdate $ CloseDocument (systemActor now)
  void $ testRequestHelper ctx POST [] (docApiV2FilesFull did) 503

  -- Seal the document.
  sealTestDocument ctx did
  closedDoc <- randomQuery $ GetDocumentByDocumentID did
  do
    (files, _) <- runTestKontra req ctx $ docApiV2FilesFullForTests closedDoc
    assertEqual "Main file + one unmerged attachment" 2 (length files)
    assertBool "Does not have merged.pdf" $ "merged.pdf" `notElem` map fst files
    assertBool "Does not have sig_att.pdf" $ "sig_att.pdf" `notElem` map fst files
    assertBool "Has not_merged.pdf" $ "not_merged.pdf" `elem` map fst files
