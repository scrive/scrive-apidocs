module DocStateTest (docStateTests, docStateSideEffectsTests) where

import Control.Arrow (first)
import Control.Conditional ((<|), (|>))
import Control.Monad.Reader
import Data.Functor
import Data.Text (unpack)
import Log
import Test.Framework
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Set as S

import Amazon
import Company.Model
import Context (ctxtime)
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.Conditions
import Doc.DBActions
import Doc.DocInfo
import Doc.DocSeal
import Doc.DocStateData
import Doc.DocumentMonad (DocumentT, theDocument, theDocumentID, withDocument, withDocumentID, withDocumentM)
import Doc.DocUtils
import Doc.Model
import Doc.Model.Filter
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryFieldID
import Doc.TestInvariants
import EvidenceLog.Model
import EvidenceLog.View (getSignatoryIdentifierMap, simplyfiedEventText)
import File.FileID
import KontraPrelude
import MinutesTime
import TestingUtil
import TestKontra
import Text.XML.DirtyContent (renderXMLContent)
import User.Model
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified MemCache

docStateSideEffectsTests :: TestEnvSt -> Test
docStateSideEffectsTests env =
    testGroup "DocState side effects"
      [
        testThat "Author user ID for documents relation is updated" env testDocumentAuthorUserID,
        testThat "Triggers update search field in documents relation" env testSignDocumentSearchData
      ]

docStateTests :: TestEnvSt -> Test
docStateTests env = testGroup "DocState" [
  testThat "RejectDocument adds to the log" env testRejectDocumentEvidenceLog,
  testThat "RestartDocument adds to the log" env testRestartDocumentEvidenceLog,
  testThat "SignDocument adds to the log" env testSignDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" env testTimeoutDocumentEvidenceLog,
  testThat "ProlongDocument can be executed and adds to a log" env testProlongDocument,

  testThat "Documents are shared in company properly" env testGetDocumentsSharedInCompany,
  testThat "SetDocumentUnsavedDraft and filtering based on unsaved_draft works" env testSetDocumentUnsavedDraft,
  testThat "Documents sorting SQL syntax is correct" env testGetDocumentsSQLSorted,
  testThat "Search string pre-processing works" env testProcessSearchStringToFilter,
  testThat "Documents searching by text works" env testGetDocumentsSQLTextFiltered,

  testThat "PreparationToPending adds to the log" env testPreparationToPendingEvidenceLog,
  testThat "MarkInvitationRead adds correct text to the log" env testMarkInvitationReadEvidenceLog,
  testThat "SaveSigAttachment adds to the log" env testSaveSigAttachmentEvidenceLog,
  testThat "DeleteSigAttachment will not work after signing" env testDeleteSigAttachmentAlreadySigned,
  testThat "DeleteSigAttachment adds to the log" env testDeleteSigAttachmentEvidenceLog,
  testThat "CloseDocument adds to the log" env testCloseDocumentEvidenceLog,
  testThat "ChangeSignatoryEmail adds to the log" env testChangeSignatoryEmailEvidenceLog,
  testThat "CancelDocument adds to the log" env testCancelDocumentEvidenceLog,

  testThat "AppendFirstSealedFile adds to the log" env testAppendFirstSealedFileEvidenceLog,
  testThat "GetDocumentsByCompanyWithFiltering filters" env testGetDocumentsByCompanyWithFilteringFilters,
  testThat "GetDocumentsByCompanyWithFiltering finds" env testGetDocumentsByCompanyWithFilteringFinds,
  testThat "GetDocumentsByCompanyWithFiltering finds with multiple" env testGetDocumentsByCompanyWithFilteringFindsMultiple,
  testThat "GetDocumentsByCompanyWithFiltering finds with company filter" env testGetDocumentsByCompanyWithFilteringCompany,
  testThat "NewDocument inserts a new contract for a single user successfully" env testNewDocumentForNonCompanyUser,
  testThat "NewDocument inserts a new contract for a company user successfully" env testNewDocumentForACompanyUser,

  testThat "CancelDocument cancels a document" env testCancelDocumentCancelsDocument,
  testThat "CancelDocument fails if doc not pending or awaiting author" env testCancelDocumentReturnsLeftIfDocInWrongState,

  testThat "SetDocumentLang fails when doc doesn't exist" env testSetDocumentLangNotLeft,

  testThat "SetDocumentTitle fails when doc doesn't exist" env testSetDocumentTitleNotLeft,
  testThat "SetDocumentTitle succeeds when doc exists and has proper status" env testSetDocumentTitleRight,

  testThat "SetDaysToSign fails when doc doesn't exist" env testSetDocumentDaysToSignNotLeft,
  testThat "SetDaysToSign and RemoveDaysToSign succeed when doc exist and has proper status" env testSetDocumentDaysToSignRight,

  testThat "CloseDocument fails when doc is not signable" env testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" env testCloseDocumentNotNothing,
  testThat "CloseDocument fails when doc is signable but not everybody has signed" env testCloseDocumentSignableButNotEverybodyHasSigned,

  testThat "CancelDocument fails when doc is not signable" env testCancelDocumentNotSignableNothing,
  testThat "CancelDocument fails when doc doesn't exist" env testCancelDocumentNotNothing,

  testThat "SetDocumentTags succeeds" env testSetDocumentTagsRight,

  testThat "SetShowHeader works" env testSetShowHeader,
  testThat "SetShowPDFDownload succeeds" env testSetShowPDFDownload,
  testThat "SetShowRejectOption succeeds" env testSetShowRejectOption,
  testThat "SetAllowRejectReason succeeds" env testSetAllowRejectReason,
  testThat "SetShowFooter succeeds" env testSetShowFooter,

  testThat "GetTimeoutedButPendingDocuments works as expected" env testGetTimedOutButPendingDocuments,

  testThat "SetInvitationDeliveryStatus fails when not signable" env testSetInvitationDeliveryStatusNotSignableLeft,
  testThat "SetInvitationDeliveryStatus fails when doc does not exist" env testSetInvitationDeliveryStatusNotLeft,
  testThat "SetInvitationDeliveryStatus succeeds if signable" env testSetInvitationDeliveryStatusSignableRight,

  testThat "MarkDocumentSeen fails when not signable" env testMarkDocumentSeenNotSignableLeft,
  testThat "MarkDocumentSeen fails when closed or preparation" env testMarkDocumentSeenClosedOrPreparationLeft,
  testThat "MarkDocumentSeen fails when doc does not exist" env testMarkDocumentSeenNotLeft,
  testThat "MarkDocumentSeen succeeds when siglink and magichash match" env testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight,
  testThat "MarkDocumentSeen fails when the siglink matches but magichash does not" env testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft,

  testThat "MarkInvitationRead when has not read" env testMarkInvitationRead,
  testThat "MarkInvitationRead never fails when doc doesn't exist" env testMarkInvitationReadDocDoesntExist,

  testThat "Seal document works" env testSealDocument,
  testThat "RejectDocument succeeds when signable and pending" env testRejectDocumentSignablePendingRight,
  testThat "RejectDocument fails when document doesn't exist" env testRejectDocumentNotLeft,
  testThat "RejectDocument fails when signable but not pending" env testRejectDocumentSignableNotPendingLeft,
  testThat "RejectDocument fails when not signable" env testRejectDocumentNotSignableLeft,

--  testThat "AuthorSignDocument succeeds when signable and preparation" env testAuthorSignDocumentSignablePreparationRight,
--  testThat "AuthorSignDocument fails when document doesn't exist" env testAuthorSignDocumentNotLeft,
--  testThat "AuthorSignDocument fails when signable but not preparation" env testAuthorSignDocumentSignableNotPreparationLeft,
--  testThat "AuthorSignDocument fails when not signable" env testAuthorSignDocumentNotSignableLeft,

  testThat "PreparationToPending succeeds when signable and preparation" env testPreparationToPendingSignablePreparationRight,
  testThat "PreparationToPending fails when document doesn't exist" env testPreparationToPendingNotLeft,
  testThat "PreparationToPending fails when signable but not preparation" env testPreparationToPendingSignableNotPreparationLeft,
  testThat "PreparationToPending fails when not signable" env testPreparationToPendingNotSignableLeft,

  testThat "SignDocument fails when doc doesn't exist" env testSignDocumentNotLeft,
  testThat "SignDocument succeeds when doc is Signable and Pending (standard mode)" env testSignDocumentSignablePendingRight,
  testThat "SignDocument succeeds when doc is Signable and Pending (eleg mode)" env testSignDocumentSignablePendingElegRight,
  testThat "SignDocument fails when the document is Signable but not in Pending" env testSignDocumentSignableNotPendingLeft,
  testThat "SignDocument fails when document is not signable" env testSignDocumentNonSignableLeft,

  testThat "TimeoutDocument fails when doc doesn't exist" env testTimeoutDocumentSignableNotLeft,
  testThat "TimeoutDocument succeeds when doc is Signable and Pending" env testTimeoutDocumentSignablePendingRight,
  testThat "TimeoutDocument fails when the document is Signable but not in Pending" env testTimeoutDocumentSignableNotPendingLeft,
  testThat "create document and check invariants" env testNewDocumentDependencies,
  testThat "can create new document and read it back with the returned id" env testDocumentCanBeCreatedAndFetchedByID,

  --testThat "when I call update document, it doesn't change the document id" env testDocumentUpdateDoesNotChangeID,
  --testThat "when I call update document, i can change the title" env testDocumentUpdateCanChangeTitle,

  testThat "when I attach a file to a real document in preparation, it returns Right" env testDocumentAttachPreparationRight,
  testThat "when I attach a file to a real document not in preparation, it returns Right" env testDocumentAttachNotPreparationLeft,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" env testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" env testDocumentAttachHasAttachment,

  testThat "when I attach a sealed file to a bad docid, it always returns left" env testNoDocumentAppendSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" env testDocumentAppendSealedPendingRight,

  --testThat "when I call updateDocument, it fails when the doc doesn't exist" env testNoDocumentUpdateDocumentAlwaysLeft,
  --testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" env testNotPreparationUpdateDocumentAlwaysLeft,
  --testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" env testPreparationUpdateDocumentAlwaysRight,

  testThat "when I create document from shared template author custom fields are stored" env testCreateFromSharedTemplate,
  testThat "when I create document from template company name is taken from company" env testCreateFromTemplateCompanyField,

  testThat "when I call ResetSignatoryDetails, it fails when the doc doesn't exist" env testNoDocumentResetSignatoryDetailsAlwaysLeft,
  testThat "When I call ResetSignatoryDetails with a doc that is not in Preparation, always returns left" env testNotPreparationResetSignatoryDetailsAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" env testPreparationResetSignatoryDetailsAlwaysRight,
  testThat "ResetSignatoryDetails works as expected" env testPreparationResetSignatoryDetails2Works,

  testThat "addDocumentAttachment fails if not in preparation" env testAddDocumentAttachmentFailsIfNotPreparation,
  testThat "addDocumentAttachment doesn't fail if there's no attachments" env testAddDocumentAttachmentOk,

  testThat "removeDocumentAttachments fails if not in preparation" env testRemoveDocumentAttachmentsFailsIfNotPreparation,
  testThat "removeDocumentAttachments return False if there's no attachments" env testRemoveDocumentAttachmentsOk,

  testThat "UpdateSigAttachments works as advertised" env testUpdateSigAttachmentsAttachmentsOk,

  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "TimeoutDocument fails when document is not signable" env testTimeoutDocumentNonSignableLeft,

  -- archive & doc deletion tests
  testThat "ArchiveDocument fails if the document is pending or awaiting author" env testArchiveDocumentPendingLeft,
  testThat "ArchiveDocument succeeds if the archiving user is the author" env testArchiveDocumentAuthorRight,
  testThat "ArchiveDocument succeeds if the archiving user is a company admin" env testArchiveDocumentCompanyAdminRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the author" env testRestoreArchivedDocumentAuthorRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the company admin" env testRestoreArchiveDocumentCompanyAdminRight,

  testThat "ArchiveDocument fails if the archiving user is an unrelated user" env testArchiveDocumentUnrelatedUserLeft,
  testThat "ArchiveDocument fails if the archiving user is just another standard company user" env testArchiveDocumentCompanyStandardLeft,
  testThat "RestoreArchivedDocument fails if the storing user is an unrelated user" env testRestoreArchivedDocumentUnrelatedUserLeft,
  testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user" env testRestoreArchiveDocumentCompanyStandardLeft,

  testThat "ReallyDeleteDocument works for author" env testReallyDeleteDocument,
  testThat "ReallyDeleteDocument works for author admin" env testReallyDeleteDocumentCompanyAdmin,
  testThat "ReallyDeleteDocument does not work for somebody else" env testReallyDeleteDocumentSomebodyElse,

  testThat "PurgeDocuments purges documents" env testPurgeDocument,
  testThat "PurgeDocuments does not purge documents for saved users" env testPurgeDocumentUserSaved,
  testThat "PurgeDocuments does not purge documents for links waiting to be signed" env testPurgeDocumentActiveSignLink,

  testThat "ArchiveIdleDocuments archives idle documents" env testArchiveIdleDocument,

  testThat "GetDocumentsByAuthor doesn't return archived docs" env testGetDocumentsByAuthorNoArchivedDocs,
  testThat "When document is signed it's status class is signed" env testStatusClassSignedWhenAllSigned,
  testThat "When document is pending and some invitation is undelivered it's status is undelivered" env testStatusClassSignedWhenAllSigned,

  testThat "ChangeAuthenticationToSignMethod works and evidence is as expected" env testChangeAuthenticationToSignMethod
  ]

testNewDocumentForNonCompanyUser :: TestEnv ()
testNewDocumentForNonCompanyUser = replicateM_ 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable) "doc title"
  assertGoodNewDocument Nothing (Signable) "doc title" result

testNewDocumentForACompanyUser :: TestEnv ()
testNewDocumentForACompanyUser = replicateM_ 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable) "doc title"
  assertGoodNewDocument (Just company) (Signable) "doc title" result

testRejectDocumentEvidenceLog :: TestEnv ()
testRejectDocumentEvidenceLog = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending
    && ((<=) 2 . length . documentsignatorylinks)
    && (all ((==) Nothing . maybesigninfo) . filter (not . isAuthor) . documentsignatorylinks)
    ) `withDocumentM` do
      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
      randomUpdate $ \t -> RejectDocument (signatorylinkid sl) Nothing (systemActor t)

      lg <- dbQuery . GetEvidenceLog  =<< theDocumentID
      assertJust $ find (\e -> evType e == Current RejectDocumentEvidence) lg

testRestartDocumentEvidenceLog :: TestEnv ()
testRestartDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable && isPending)
  withDocument doc $ randomUpdate $ \t->CancelDocument (systemActor t)
  cdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  mdoc <- randomUpdate $ \t->RestartDocument cdoc (systemActor t)
  assertJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromJust mdoc)
  assertJust $ find (\e -> evType e == Current RestartDocumentEvidence) lg
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg
  lg2 <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg2

getScreenshots :: TestEnv SignatoryScreenshots.SignatoryScreenshots
getScreenshots = do
  now <- currentTime
  first_ <- liftIO $ BS.readFile $ inTestDir "screenshots/s1.jpg"
  signing <- liftIO $ BS.readFile $ inTestDir "screenshots/s2.jpg"
  let mkss i = Just $ Screenshot.Screenshot{ Screenshot.time = now
                                           , Screenshot.image = i
                                           }
  return $ SignatoryScreenshots.emptySignatoryScreenshots{ SignatoryScreenshots.first = mkss first_
                                     , SignatoryScreenshots.signing = mkss signing
                                     }

testSignDocumentEvidenceLog :: TestEnv ()
testSignDocumentEvidenceLog = do
  author <- addNewRandomUser

  screenshots <- getScreenshots
  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&
      isPending &&
      ((==) 2 . length . documentsignatorylinks) &&
      (all signatoryispartner . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      Just asl <- getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (signatorymagichash asl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) (signatorymagichash asl) Nothing Nothing screenshots (systemActor t)

      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing screenshots (systemActor t)

      randomUpdate $ \t -> CloseDocument (systemActor t)

      lg <- dbQuery . GetEvidenceLog =<< theDocumentID
      assertJust $ find (\e -> evType e == Current SignDocumentEvidence) lg

      mc <- MemCache.new (const 1) 1000
      runAmazonMonadT (AmazonConfig Nothing mc Nothing) $ do
        sealDocument "https://scrive.com"

testSignDocumentSearchData :: TestEnv ()
testSignDocumentSearchData = do
  author <- addNewRandomUser

  screenshots <- getScreenshots
  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&
      isPending &&
      ((==) 2 . length . documentsignatorylinks) &&
      (all signatoryispartner . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do

      Just asl <- getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (signatorymagichash asl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) (signatorymagichash asl) Nothing Nothing screenshots (systemActor t)

      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing screenshots (systemActor t)

      randomUpdate $ \t -> CloseDocument (systemActor t)
      docID <- theDocumentID
      mSearchDataByFunction <- dbQuery $ GetDocumentSearchDataByFunction docID
      mSearchDataByField    <- dbQuery $ GetDocumentSearchDataByField docID
      assertEqual "Search string is updated by triggers upon document creation"
                  mSearchDataByFunction mSearchDataByField
      -- check that change title works with search triggers
      title' <- rand 1 $ arbString 10 25
      void . randomUpdate $ \t -> SetDocumentTitle title' (systemActor t)
      mSearchDataByFunction' <- dbQuery $ GetDocumentSearchDataByFunction docID
      mSearchDataByField'    <- dbQuery $ GetDocumentSearchDataByField docID
      if (mSearchDataByField' == mSearchDataByField)
      then assertFailure "Search field was not updated"
      else assertEqual "Search string is updated by triggers after document title update"
                       mSearchDataByFunction' mSearchDataByField'
      -- check that change slf.value_text works with search triggers. Involved.
      let sfs :: [SignatoryField]
          sfs = join $ map signatoryfields $ [asl, sl]

          -- Unfortunately, since we don't separate the DB ID and the data
          -- itself, we have to pattern match on all constructors in the
          -- `SignatoryField` data type to get the correct projection for the
          -- field ID. Also, cf. `Arbitrary` instance for `SignatoryField` to
          -- explain why `stfValue` is used for a lot of cases. It's somewhat
          -- messy and incoherent, but this needs to be tested and given the
          -- state of things it can't be helped. I guess we _could_ just call
          -- `error` for those constructors that are never generated by
          -- Quickcheck.
          newArbSigFieldValueText :: SignatoryField -> (SignatoryFieldID, Gen String)
          newArbSigFieldValueText sf =
            case sf of
              SignatoryNameField           sft -> (snfID  sft, snfValue  <$> arbitrary)
              SignatoryCompanyField        sft -> (scfID  sft, scfValue  <$> arbitrary)
              SignatoryPersonalNumberField sft -> (spnfID sft, spnfValue <$> arbitrary)
              SignatoryCompanyNumberField  sft -> (scnfID sft, scnfValue <$> arbitrary)
              SignatoryEmailField          sft -> (sefID  sft, sefValue  <$> arbitrary)
              SignatoryMobileField         sft -> (smfID  sft, stfValue  <$> arbitrary)
              SignatoryTextField           sft -> (stfID  sft, stfValue  <$> arbitrary)
              SignatoryCheckboxField       sft -> (schfID sft, stfValue  <$> arbitrary)
              SignatorySignatureField      sft -> (ssfID  sft, stfValue  <$> arbitrary)
              SignatoryRadioGroupField     sft -> (srgfID sft, stfValue  <$> arbitrary)

      forM_ (map newArbSigFieldValueText sfs) $ \(slid, gValueText) -> do
          valueText <- rand 1 gValueText
          dbUpdate $ SetSLFValueTextField slid valueText

      mSearchDataByFunction'' <- dbQuery $ GetDocumentSearchDataByFunction docID
      mSearchDataByField''    <- dbQuery $ GetDocumentSearchDataByField docID

      if (mSearchDataByField'' == mSearchDataByField')
      then assertFailure "Search field was not updated"
      else assertEqual "Search string is updated by triggers after signatory_link_fields.value_text update"
                       mSearchDataByFunction'' mSearchDataByField''

testDocumentAuthorUserID :: TestEnv ()
testDocumentAuthorUserID = do
  author <- addNewRandomUser

  screenshots <- getScreenshots
  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&
      isPending &&
      ((==) 2 . length . documentsignatorylinks) &&
      (all signatoryispartner . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      docID <- theDocumentID
      -- `documents.author_user_id` should be set by now
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is updated upon document creation"
                      mDocAuthorUserID (Just $ userid author)

      Just asl <- getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (signatorymagichash asl) (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document seen by author"
                      mDocAuthorUserID (Just $ userid author)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) (signatorymagichash asl) Nothing Nothing screenshots (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document signed by author"
                      mDocAuthorUserID (Just $ userid author)

      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument

      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document seen by signatory"
                      mDocAuthorUserID (Just $ userid author)

      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing screenshots (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document signed by signatory"
                      mDocAuthorUserID (Just $ userid author)

      randomUpdate $ \t -> CloseDocument (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document closed"
                      mDocAuthorUserID (Just $ userid author)

testTimeoutDocumentEvidenceLog :: TestEnv ()
testTimeoutDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    success <- randomUpdate $ \t->TimeoutDocument (systemActor t)
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current TimeoutDocumentEvidence) lg



testProlongDocument :: TestEnv ()
testProlongDocument = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    success <- randomUpdate $ \t->TimeoutDocument (systemActor t)
    assert success
    randomUpdate $ \t -> ProlongDocument 2 defaultTimeZoneName (systemActor t)
    pending <- (\d ->  (Pending == documentstatus d))  <$> theDocument
    assert pending
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current ProlongDocumentEvidence) lg

testPreparationToPendingEvidenceLog :: TestEnv ()
testPreparationToPendingEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPreparation && ((<=) 2 . length . documentsignatorylinks)) `withDocumentM` do
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ \t->PreparationToPending (systemActor t) tz

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current PreparationToPendingEvidence) lg

testMarkInvitationReadEvidenceLog :: TestEnv ()
testMarkInvitationReadEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    Just sl <- getAuthorSigLink <$> theDocument
    now <- currentTime
    success <- randomUpdate $ MarkInvitationRead (signatorylinkid sl) (mailSystemActor now Nothing (getEmail sl) (signatorylinkid sl))
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    -- The text for MarkInvitationRead in the event log is hard to check
    -- manually since it relies on external mail system notifications,
    -- so we test it explicitly.
    let me = find (\e -> evType e == Current MarkInvitationReadEvidence) lg
    assertJust me
    let Just e = me
    let expectedFull = "Scrive eSign’s external email delivery system reported that the invitation sent to " ++ getEmail sl ++ " was opened."
    assertEqual "Correct event text full" (expectedFull) (unpack $ renderXMLContent $ evText e)
    let expectedSimple = "The invitation to "
                ++ (if signatoryispartner sl then "sign" else "review")
                ++ " the document (sent to " ++ getEmail sl ++ ") was opened."
    sim <- getSignatoryIdentifierMap True [e]
    simpletext <- theDocument >>= \d -> simplyfiedEventText EventForVerificationPages (Just "author") d{ documentlang = LANG_EN } sim e
    assertEqual "Correct simplified event text" (expectedSimple) simpletext

testSaveSigAttachmentEvidenceLog :: TestEnv ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isPreparation && isSignable) `withDocumentM` do
    file <- addNewRandomFile
    let sa = def
    theDocument >>= \d -> randomUpdate $ \t->SetSigAttachments (signatorylinkid $ (documentsignatorylinks d) !! 0)
                          [sa] (systemActor t)
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ \t->PreparationToPending (systemActor t) tz
    theDocument >>= \d -> randomUpdate $ \t->SaveSigAttachment (signatorylinkid $ (documentsignatorylinks d) !! 0) sa file (systemActor t)

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current SaveSigAttachmentEvidence) lg


testDeleteSigAttachmentAlreadySigned :: TestEnv ()
testDeleteSigAttachmentAlreadySigned = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (    isSignable
                                                  && isPreparation
                                                  && ((all (isSignatory && not .hasSigned && (==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod)) . documentsignatorylinks)
                                                  && (((==) 2) . length .documentsignatorylinks)) `withDocumentM` do
    file <- addNewRandomFile
    sl <- (\d -> (documentsignatorylinks d) !! 1) <$> theDocument
    let sa = def
    _<-randomUpdate $ \t->SetSigAttachments (signatorylinkid $ sl)
                          [sa] (systemActor t)
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ \t->PreparationToPending (systemActor t) tz
    randomUpdate $ \t->SaveSigAttachment (signatorylinkid $ sl) sa file (systemActor t)

    randomUpdate $ \t->DeleteSigAttachment (signatorylinkid $ sl) sa (systemActor t)
    randomUpdate $ \t->SaveSigAttachment (signatorylinkid $ sl) sa file (systemActor t)
    randomUpdate $ \t->MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
    randomUpdate $ \t->SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
    assertRaisesKontra (\SignatoryHasAlreadySigned {} -> True) $ do
      randomUpdate $ \t->DeleteSigAttachment (signatorylinkid $ sl) sa (systemActor t)

testDeleteSigAttachmentEvidenceLog :: TestEnv ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    let sa = def
          { signatoryattachmentfile        = Just file
          , signatoryattachmentfilename    = Just "afile.ran"
          }
    theDocument >>= \d -> randomUpdate $ \t->SetSigAttachments (signatorylinkid $ (documentsignatorylinks d) !! 0) [sa] (systemActor t)
    theDocument >>= \d -> randomUpdate $ \t->DeleteSigAttachment (signatorylinkid $ (documentsignatorylinks d) !! 0) sa (systemActor t)

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current DeleteSigAttachmentEvidence) lg

testAppendFirstSealedFileEvidenceLog :: TestEnv ()
testAppendFirstSealedFileEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isClosed `withDocumentM` do
    file <- addNewRandomFile
    randomUpdate $ \t -> AppendSealedFile file
                           Guardtime{ extended = False, private = False } (systemActor t)

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current AttachGuardtimeSealedFileEvidence) lg

testCancelDocumentEvidenceLog :: TestEnv ()
testCancelDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    randomUpdate $ \t-> CancelDocument (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg

testChangeSignatoryEmailEvidenceLog :: TestEnv ()
testChangeSignatoryEmailEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isPending && ((<=) 2 . length . documentsignatorylinks)) `withDocumentM` do
    Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
    success <- randomUpdate $ \t-> ChangeSignatoryEmail (signatorylinkid sl) Nothing "email@email.com" (systemActor t)
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current ChangeSignatoryEmailEvidence) lg

testCloseDocumentEvidenceLog :: TestEnv ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending && (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)) `withDocumentM` do
    documentsignatorylinks <$> theDocument >>= \sls -> forM_  sls $ \sl -> when (isSignatory sl) $ do
      randomUpdate $ \t->MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
      randomUpdate $ \t->SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
    randomUpdate $ \t-> CloseDocument (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> TestEnv (User, UTCTime, Document)
performNewDocumentWithRandomUser mcompany doctype title = do
  user <- maybe addNewRandomUser (\c -> addNewRandomCompanyUser (companyid c) False) mcompany
  ctx <- mkContext def
  let aa = authorActor ctx user
  doc <- randomUpdate $ NewDocument user title doctype defaultTimeZoneName 0 aa
  return (user, get ctxtime ctx, doc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> (User, UTCTime, Document) -> TestEnv ()
assertGoodNewDocument mcompany doctype title (user, time, doc) = do
    assertEqual "Correct title" title (documenttitle doc)
    assertEqual "Correct type" doctype (documenttype doc)
    assertEqual "Doc has user's lang" (getLang user) (getLang doc)
    assertBool "Doc creation time" $ compareTime time (documentctime doc)
    assertBool "Doc modification time" $ compareTime time (documentmtime doc)
    assertEqual "No author attachments" [] (documentauthorattachments doc)
    assertEqual "No sig attachments" [] (concatMap signatoryattachments $ documentsignatorylinks doc)
    assertBool "Uses email identification only" (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) (documentsignatorylinks doc))
    assertEqual "In preparation" Preparation (documentstatus doc)
    assertEqual "1 signatory" 1 (length $ documentsignatorylinks doc)
    let siglink = head $ documentsignatorylinks doc
    assertBool "link is author and possibly signer" $
      (signatoryisauthor $ siglink)
    assertEqual "link first name matches author's" (getFirstName user) (getFirstName siglink)
    assertEqual "link last name matches author's" (getLastName user) (getLastName siglink)
    assertEqual "link email matches author's" (getEmail user) (getEmail siglink)
    assertEqual "link personal number matches author's" (getPersonalNumber user) (getPersonalNumber siglink)
    assertEqual "link company name matches company's" (getCompanyName mcompany) (getCompanyName siglink)
    assertEqual "link company number matches company's" (getCompanyNumber mcompany) (getCompanyNumber siglink)
    assertEqual "link company number matches company's" (getMobile user) (getMobile siglink)
    assertEqual "link signatory matches author id" (Just $ userid user) (maybesignatory siglink)

testCancelDocumentCancelsDocument :: TestEnv ()
testCancelDocumentCancelsDocument = replicateM_ 10 $ do
  user <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition user (isSignable && isPending) `withDocumentM` do
    doc <- theDocument
    randomUpdate $ CancelDocument (authorActor ctx user)

    canceleddoc <- theDocument
    assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
    assertBool "Updated modification time" $ compareTime (get ctxtime ctx) (documentmtime canceleddoc)
    assertBool "Siglinks are unchanged"
      (signatoryLinksListsAreAlmostEqualForTests (documentsignatorylinks doc) (documentsignatorylinks canceleddoc))
    assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: TestEnv ()
testCancelDocumentReturnsLeftIfDocInWrongState = replicateM_ 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable && not . isPending)
  ctx <- mkContext def
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $
               withDocument doc $ randomUpdate $ CancelDocument (authorActor ctx user)

--------------------------------------------------------------------------------
assertOneArchivedSigLink :: MonadIO m => Document -> m ()
assertOneArchivedSigLink doc =
  assertEqual "Expected one archived sig link"
              1
              (length . filter (isJust . signatorylinkdeleted) . documentsignatorylinks $ doc)

assertNoArchivedSigLink :: MonadIO m => Document -> m ()
assertNoArchivedSigLink doc =
  assertEqual "Expected no archived sig link"
              0
              (length . filter (isJust . signatorylinkdeleted) . documentsignatorylinks $ doc)

testArchiveDocumentPendingLeft :: TestEnv ()
testArchiveDocumentPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  _doc0 <- addRandomDocumentWithAuthorAndCondition author isPending
  _doc1 <- addRandomDocumentWithAuthorAndCondition author isPending

  let doc = _doc1

  assertRaisesKontra (\(DocumentStatusShouldBe {}) -> True) $
               withDocument doc $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)


testArchiveDocumentAuthorRight :: TestEnv ()
testArchiveDocumentAuthorRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testArchiveDocumentCompanyAdminRight :: TestEnv ()
testArchiveDocumentCompanyAdminRight = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid adminuser) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testRestoreArchivedDocumentAuthorRight :: TestEnv ()
testRestoreArchivedDocumentAuthorRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument author (systemActor t)
    assertNoArchivedSigLink =<< theDocument

testRestoreArchiveDocumentCompanyAdminRight :: TestEnv ()
testRestoreArchiveDocumentCompanyAdminRight = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument adminuser (systemActor t)

    assertNoArchivedSigLink =<< theDocument

testChangeAuthenticationToSignMethod :: TestEnv ()
testChangeAuthenticationToSignMethod = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (
      isSignable && isPending && ((<=) 2 . length . documentsignatorylinks)
      && ( all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument

      randomUpdate $ \t->ChangeAuthenticationToSignMethod (signatorylinkid sl) SMSPinAuthenticationToSign Nothing Nothing (systemActor t)
      lg1 <- dbQuery . GetEvidenceLog  =<< theDocumentID
      assertJust $ find (\e -> evType e == Current ChangeAuthenticationToSignMethodStandardToSMSEvidence) lg1
      assertNothing $ find (\e -> evType e == Current UpdateFieldMobileEvidence) lg1

      randomUpdate $ \t->ChangeAuthenticationToSignMethod (signatorylinkid sl) SMSPinAuthenticationToSign Nothing (Just "+486543222112") (systemActor t)
      lg2 <- dbQuery . GetEvidenceLog  =<< theDocumentID
      assertEqual "Too many evidence logs for change authentication method"
        (length $ filter (\e -> evType e == Current ChangeAuthenticationToSignMethodStandardToSMSEvidence) lg2) 1
      assertJust $ find (\e -> evType e == Current UpdateFieldMobileEvidence) lg2

--------------------------------------------------------------------------------
testReallyDeleteDocument :: TestEnv ()
testReallyDeleteDocument = replicateM_ 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

  assertRaisesKontra (\DocumentIsNotDeleted {} -> True) $
    withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  withDocument doc $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
  withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  assertRaisesKontra (\DocumentIsReallyDeleted {} -> True) $
    withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByDocumentID (documentid doc)]
                     [] 1
  assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)


testReallyDeleteDocumentCompanyAdmin :: TestEnv ()
testReallyDeleteDocumentCompanyAdmin = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    assertRaisesKontra (\DocumentIsNotDeleted {} -> True) $
      randomUpdate $ \t->ReallyDeleteDocument (userid adminuser) (systemActor t)
    randomUpdate $ \t->ArchiveDocument (userid adminuser) (systemActor t)
    randomUpdate $ \t->ReallyDeleteDocument (userid adminuser) (systemActor t)
    assertOneArchivedSigLink =<< theDocument
    doc <- theDocument
    docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByDocumentID (documentid doc)]
                       [] 1
    assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)

testReallyDeleteDocumentSomebodyElse :: TestEnv ()
testReallyDeleteDocumentSomebodyElse = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  other <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
      randomUpdate $ \t->ReallyDeleteDocument (userid other) (systemActor t)
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
      randomUpdate $ \t->ArchiveDocument (userid other) (systemActor t)
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
      randomUpdate $ \t->ReallyDeleteDocument (userid other) (systemActor t)
    doc <- theDocument
    assertEqual "Expected no archived signatory links"
              0
              (length . filter (isJust . signatorylinkdeleted) . documentsignatorylinks $ doc)
--------------------------------------------------------------------------------


testPurgeDocument :: TestEnv ()
testPurgeDocument = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  now <- currentTime
  archived1 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged zero documents when not deleted" 0 archived1
  withDocument doc $ randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged single document" 1 archived2

  allDocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                         [DocumentFilterByDocumentID (documentid doc)] [] (-1)
  assertEqual "List documents does not include purged ones" [] (map documentid allDocs1)

testPurgeDocumentUserSaved :: TestEnv ()
testPurgeDocumentUserSaved = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  archived1 <- dbUpdate $ PurgeDocuments 1 0
  now <- currentTime
  withDocument doc $ randomUpdate $ \t->ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 1 0
  assertEqual "Purged zero documents before delete" 0 archived1
  assertEqual "Purged zero documents before time passed after delete" 0 archived2

testPurgeDocumentActiveSignLink :: TestEnv ()
testPurgeDocumentActiveSignLink = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (isClosed && (not . null . filter (isSignatory && (not . isAuthor)). documentsignatorylinks)) `withDocumentM` do
    now <- currentTime
    randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
    updateMTimeAndObjectVersion now
    archived <- dbUpdate $ PurgeDocuments 0 1
    assertEqual "Purged zero documents" 0 archived

testArchiveIdleDocument :: TestEnv ()
testArchiveIdleDocument = replicateM_ 10 $ do
  company <- addNewCompany
  _ <- dbUpdate $ SetCompanyInfo (companyid company) ((companyinfo company){ companyidledoctimeout = Just 1 })
  author <- addNewRandomCompanyUser(companyid company) False
  author2 <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isClosed && isSignable)
  _ <- addRandomDocumentWithAuthorAndCondition author (isTemplate || isPending)
  _ <- addRandomDocumentWithAuthorAndCondition author2 (isClosed && isSignable)
  archived1 <- archiveIdleDocuments (documentmtime doc)
  assertEqual "Archived zero idle documents (too early)" 0 archived1
  archived2 <- archiveIdleDocuments (2 `daysAfter` documentmtime doc)
  assertEqual "Archived idle documents for one signatory" 1 archived2

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: TestEnv ()
testArchiveDocumentUnrelatedUserLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    unrelateduser <- addNewRandomUser
    assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True) $
      randomUpdate $ \t -> ArchiveDocument (userid unrelateduser) (systemActor t)

testArchiveDocumentCompanyStandardLeft :: TestEnv ()
testArchiveDocumentCompanyStandardLeft = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
      randomUpdate $ \t->ArchiveDocument (userid standarduser) (systemActor t)

testRestoreArchivedDocumentUnrelatedUserLeft :: TestEnv ()
testRestoreArchivedDocumentUnrelatedUserLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    unrelateduser <- addNewRandomUser
    randomUpdate $ \t -> ArchiveDocument (userid author) (systemActor t)
    assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True)$ do
      randomUpdate $ \t->RestoreArchivedDocument unrelateduser (systemActor t)

testRestoreArchiveDocumentCompanyStandardLeft :: TestEnv ()
testRestoreArchiveDocumentCompanyStandardLeft = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $ do
      randomUpdate $ \t->RestoreArchivedDocument standarduser (systemActor t)

testGetDocumentsByAuthorNoArchivedDocs :: TestEnv ()
testGetDocumentsByAuthorNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsByAuthor . userid)

checkQueryDoesntContainArchivedDocs :: DBQuery (DocumentT TestEnv) q [Document] => (User -> q) -> TestEnv ()
checkQueryDoesntContainArchivedDocs qry = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> (isPreparation d || isClosed d) && (isSignable d)) `withDocumentM` do
    did <- theDocumentID
    docsbeforearchive <- dbQuery (qry author)
    assertEqual "Expecting one doc before archive" [did] (map documentid docsbeforearchive)
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    docsafterarchive <- dbQuery (qry author)
    assertEqual "Expecting no docs after archive" [] (map documentid docsafterarchive)
    randomUpdate $ \t->RestoreArchivedDocument author (systemActor t)
    docsafterestore <- dbQuery (qry author)
    assertEqual "Expecting one doc after restoring" [did] (map documentid docsafterestore)

testSetDocumentLangNotLeft :: TestEnv ()
testSetDocumentLangNotLeft = replicateM_ 10 $ do
  d <- rand 10 arbitrary
  success <- withDocumentID d $ randomUpdate $ \l t -> SetDocumentLang l (systemActor t)
  assert $ not success

testNewDocumentDependencies :: TestEnv ()
testNewDocumentDependencies = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  -- execute
  ctx <- mkContext def
  let aa = authorActor ctx author
  doc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype defaultTimeZoneName 0 aa)
  -- assert
  assertInvariants doc

testDocumentCanBeCreatedAndFetchedByID :: TestEnv ()
testDocumentCanBeCreatedAndFetchedByID = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  ctx <- mkContext def
  let aa = authorActor ctx author
  doc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype defaultTimeZoneName 0 aa)
  -- execute
  ndoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  -- assert

  assert $ documentid doc == documentid ndoc
  assertInvariants ndoc

testDocumentAttachNotPreparationLeft :: TestEnv ()
testDocumentAttachNotPreparationLeft = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    withDocument doc $ randomUpdate $ \t->AttachFile file (systemActor t)

testDocumentAttachPreparationRight :: TestEnv ()
testDocumentAttachPreparationRight = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    --execute
    randomUpdate $ \t -> AttachFile file (systemActor t)

    --assert
    assertInvariants =<< theDocument


testNoDocumentAttachAlwaysLeft :: TestEnv ()
testNoDocumentAttachAlwaysLeft = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    d <- rand 10 arbitrary
    withDocumentID d $ randomUpdate $ (\t -> AttachFile file (systemActor t))
  --assert

testDocumentAttachHasAttachment :: TestEnv ()
testDocumentAttachHasAttachment = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    --execute

    randomUpdate $ \t -> AttachFile file (systemActor t)
    --assert
    -- assertJust $ find ((== a) . filename) (documentfiles $ fromRight edoc)
    assertInvariants =<< theDocument

testNoDocumentAppendSealedAlwaysLeft :: TestEnv ()
testNoDocumentAppendSealedAlwaysLeft = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    docid <- rand 10 arbitrary
    withDocumentID docid $ randomUpdate $ AppendSealedFile file Missing (systemActor time)


testSealDocument :: TestEnv ()
testSealDocument = replicateM_ 1 $ do
  -- setup
  author <- addNewRandomUser
  ctx <- mkContext def
  screenshots <- getScreenshots
  addRandomDocument ((randomDocumentAllowsDefault author) { randomDocumentAllowedStatuses = [Pending]
                                                          , randomDocumentAllowedTypes = [Signable]
                                                          }) `withDocumentM` do
    _file <- addNewRandomFile

    atts <- replicateM 2 $ do
             file <- addNewRandomFile

             let att = def
                   { signatoryattachmentfile = Just file
                   , signatoryattachmentfilename = Just "afile.ran"
                   , signatoryattachmentname = show file
                   }
             return att
    (time, sl) <- rand 10 arbitrary
    sa <- signatoryActor (set ctxtime time ctx) sl
    sls <- documentsignatorylinks <$> theDocument
    dbUpdate $ SetSigAttachments (signatorylinkid $ sls !! 0) atts sa

    forM_ sls $ \slk -> do
      when (signatoryispartner slk) $ do
        if signatorylinkauthenticationtosignmethod slk == SEBankIDAuthenticationToSign
          then do
             randomUpdate $ \info -> SignDocument (signatorylinkid slk) (signatorymagichash slk) (Just info) Nothing screenshots sa
          else do
             dbUpdate $ SignDocument (signatorylinkid slk) (signatorymagichash slk) Nothing Nothing screenshots sa

    randomUpdate $ \t-> CloseDocument (systemActor t)

    mc <- MemCache.new (const 1) 1000
    runAmazonMonadT (AmazonConfig Nothing mc Nothing) $ do
      sealDocument "https://scrive.com"


testDocumentAppendSealedPendingRight :: TestEnv ()
testDocumentAppendSealedPendingRight = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocument ((randomDocumentAllowsDefault author) { randomDocumentAllowedStatuses = [Closed]
                                                          }) `withDocumentM` do
    file <- addNewRandomFile

    --execute

    now <- currentTime
    success <- randomUpdate $ AppendExtendedSealedFile file Missing $ systemActor now

    --assert
    assert success
    nfile <- fmap mainfileid <$> documentsealedfile <$> theDocument
    assertBool "Should have new file attached, but it's not" $ Just file == nfile


testGetTimedOutButPendingDocuments :: TestEnv ()
testGetTimedOutButPendingDocuments = replicateM_ 1 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending && (isJust . documenttimeouttime))
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

  let t = fromJust $ documenttimeouttime doc
  --execute
  docsA <- dbQuery $ GetTimeoutedButPendingDocumentsChunk ((-10) `minutesAfter` t) 100
  docsB <- dbQuery $ GetTimeoutedButPendingDocumentsChunk (10 `minutesAfter` t) 100

  --assert
  assertEqual "Documents do not timeout before time" [] (map documentstatus docsA)
  assertEqual "Documents timeout after time" [Pending] (map documentstatus docsB)

testNotPreparationResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNotPreparationResetSignatoryDetailsAlwaysLeft = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    mt <- rand 10 arbitrary
    sf <- signatoryFieldsFromUser author
    --execute
    success <- dbUpdate $ ResetSignatoryDetails [def { signatoryfields = sf}] (systemActor mt)
    --assert
    assert $ not success

testPreparationResetSignatoryDetailsAlwaysRight :: TestEnv ()
testPreparationResetSignatoryDetailsAlwaysRight = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    mt <- rand 10 arbitrary
    --execute
    success <- dbUpdate $ ResetSignatoryDetails [def { signatoryisauthor = True , maybesignatory = Just $ userid author}] (systemActor mt)
    --assert
    assert success
    assertInvariants =<< theDocument

testPreparationResetSignatoryDetails2Works :: TestEnv ()
testPreparationResetSignatoryDetails2Works = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    mt <- rand 10 arbitrary
    --execute
    let newData1 = def {   signatoryisauthor = True , maybesignatory = Just $ userid author}
    success1 <- dbUpdate $ ResetSignatoryDetails [newData1] (systemActor mt)
    assert success1
    assertInvariants =<< theDocument
    sls <- documentsignatorylinks <$> theDocument
    assertEqual "Proper delivery method set" [EmailDelivery] (map signatorylinkdeliverymethod sls)
    assertEqual "Proper authentication method set" [StandardAuthenticationToSign] (map signatorylinkauthenticationtosignmethod sls)

    let newData2 =  def { signatoryisauthor = True, maybesignatory = Just $ userid author , signatorylinkdeliverymethod = PadDelivery, signatorylinkauthenticationtosignmethod= SEBankIDAuthenticationToSign }
    success2 <- dbUpdate $ ResetSignatoryDetails [newData2] (systemActor mt)
    assert success2
    sls2 <- documentsignatorylinks <$> theDocument
    assertEqual "Proper delivery method set" [PadDelivery] (map signatorylinkdeliverymethod sls2)
    assertEqual "Proper authentication method set" [SEBankIDAuthenticationToSign] (map signatorylinkauthenticationtosignmethod sls2)
    assertInvariants =<< theDocument

testNoDocumentResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNoDocumentResetSignatoryDetailsAlwaysLeft = replicateM_ 10 $ do
  -- setup
  a <- rand 10 arbitrary
  --author <- addNewRandomUser
  mt <- rand 10 arbitrary
  --execute
  -- non-existent docid

  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID a $ dbUpdate $ ResetSignatoryDetails [def { signatoryisauthor = True } ] (systemActor mt)



testGetDocumentsSharedInCompany :: TestEnv ()
testGetDocumentsSharedInCompany = replicateM_ 10 $ do
  -- two companies, two users per company, two users outside of company
  -- each having a document here
  company1 <- addNewCompany
  company2 <- addNewCompany
  user1' <- addNewRandomUser
  user2' <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid user1') (companyid company1)
  Just user1 <- dbQuery $ GetUserByID (userid user1')
  _ <- dbUpdate $ SetUserCompany (userid user2') (companyid company1)
  Just user2 <- dbQuery $ GetUserByID (userid user2')
  user3' <- addNewRandomUser
  user4' <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid user3') (companyid company2)
  Just user3 <- dbQuery $ GetUserByID (userid user3')
  _ <- dbUpdate $ SetUserCompany (userid user4') (companyid company2)
  Just user4 <- dbQuery $ GetUserByID (userid user4')
  user5 <- addNewRandomUser
  user6 <- addNewRandomUser

  -- This test is good only for not admins
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user1) False
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user2) False
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user3) False
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user4) False
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user5) False
  _ <- dbUpdate $ SetUserCompanyAdmin (userid user6) False

  doc1 <- addRandomDocumentWithAuthorAndCondition user1 (isTemplate)
  doc2 <- addRandomDocumentWithAuthorAndCondition user2 (isTemplate)
  doc3 <- addRandomDocumentWithAuthorAndCondition user3 (isTemplate)
  doc4 <- addRandomDocumentWithAuthorAndCondition user4 (isTemplate)
  doc5 <- addRandomDocumentWithAuthorAndCondition user5 (isTemplate)
  doc6 <- addRandomDocumentWithAuthorAndCondition user6 (isTemplate)

  let [docid1, docid2, docid3, docid4, docid5, docid6] =
         documentid <$> [doc1, doc2, doc3, doc4, doc5, doc6]

  -- user1: owns doc1, sees doc2
  -- user2: owns doc2, sees doc1
  -- user3: owns doc3,
  -- user4: owns doc4, sees doc3
  -- user5: owns doc5
  -- user6: owns doc6

  _ <- dbUpdate $ SetDocumentSharing [docid4] False
  _ <- dbUpdate $ SetDocumentSharing [docid1, docid2, docid3, docid5, docid6] True

  dlist1 <- dbQuery $ GetAvailableTemplates (userid user1)
  dlist2 <- dbQuery $ GetAvailableTemplates (userid user2)
  dlist3 <- dbQuery $ GetAvailableTemplates (userid user3)
  dlist4 <- dbQuery $ GetAvailableTemplates (userid user4)
  dlist5 <- dbQuery $ GetAvailableTemplates (userid user5)
  dlist6 <- dbQuery $ GetAvailableTemplates (userid user6)

  assertEqual "Documents not shared in user without company (X) by user 5" 1 (length dlist5)
  assertEqual "Documents not shared in user without company (Y) by user 6" 1 (length dlist6)
  assertEqual "Documents properly shared in company (2) by user 3" 1 (length dlist3)
  assertEqual "Documents properly shared in company (2) by user 4" 2 (length dlist4)
  assertEqual "Documents properly shared in company (1) by user 1" 2 (length dlist1)
  assertEqual "Documents properly shared in company (1) by user 2" 2 (length dlist2)

testProcessSearchStringToFilter :: TestEnv ()
testProcessSearchStringToFilter = do
  let stripDocumentFilter (DocumentFilterByString s) = s
      stripDocumentFilter _ = $unexpectedError "This should not happen!"
      t0 = ""
      e0 = []
      t1 = "h"
      e1 = [Unquoted "h"]
      t2 = "hey"
      e2 = [Unquoted "hey"]
      t3 = "hey there"
      e3 = [Unquoted "hey", Unquoted "there"]
      t4 = "hey there \"street 23\" after"
      e4 = [Unquoted "hey", Unquoted "there", Quoted "street 23", Unquoted "after"]
      t5 = "hey there \"street 23\""
      e5 = [Unquoted "hey", Unquoted "there", Quoted "street 23"]
      t6 = "\"search term\""
      e6 = [Quoted "search term"]
      t7 = "\"search\""
      e7 = [Quoted "search"]
      t8 = "hey \"there 23\" and \"missing some"
      e8 = [Unquoted "hey", Quoted "there 23", Unquoted "and", Unquoted "missing", Unquoted "some"]
      t9 = "hey \"there 23\" missing \""
      e9 = [Unquoted "hey", Quoted "there 23", Unquoted "missing"]
      tA = "hey \"there 23\" missing\""
      eA = [Unquoted "hey", Quoted "there 23", Unquoted "missing"]
      tB = "hey \"there 23\"missing is not"
      eB = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tC = "hey \"there 23 \" missing is not"
      eC = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tD = "hey \" there 23 \" missing is not"
      eD = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tE = "hey \"\" woah"
      eE = [Unquoted "hey", Unquoted "woah"]
      tF = "hey \"  \" woah"
      eF = [Unquoted "hey", Unquoted "woah"]
      tG = "hey \" there      23 \" missing is not"
      eG = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tH = "hey \" there\t23 \" missing is not"
      eH = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tI = "hey \" there\n23 \" missing is not"
      eI = [Unquoted "hey", Quoted "there 23", Unquoted "missing", Unquoted "is", Unquoted "not"]
      tJ = "hey \"there 23\" and \"missing some not\" at all \"boooyah!\""
      eJ = [Unquoted "hey", Quoted "there 23", Unquoted "and", Quoted "missing some not", Unquoted "at"]

  assertEqual "Should match" e0 (map stripDocumentFilter $ processSearchStringToFilter t0)
  assertEqual "Should match" e1 (map stripDocumentFilter $ processSearchStringToFilter t1)
  assertEqual "Should match" e2 (map stripDocumentFilter $ processSearchStringToFilter t2)
  assertEqual "Should match" e3 (map stripDocumentFilter $ processSearchStringToFilter t3)
  assertEqual "Should match" e4 (map stripDocumentFilter $ processSearchStringToFilter t4)
  assertEqual "Should match" e5 (map stripDocumentFilter $ processSearchStringToFilter t5)
  assertEqual "Should match" e6 (map stripDocumentFilter $ processSearchStringToFilter t6)
  assertEqual "Should match" e7 (map stripDocumentFilter $ processSearchStringToFilter t7)
  assertEqual "Should match" e8 (map stripDocumentFilter $ processSearchStringToFilter t8)
  assertEqual "Should match" e9 (map stripDocumentFilter $ processSearchStringToFilter t9)
  assertEqual "Should match" eA (map stripDocumentFilter $ processSearchStringToFilter tA)
  assertEqual "Should match" eB (map stripDocumentFilter $ processSearchStringToFilter tB)
  assertEqual "Should match" eC (map stripDocumentFilter $ processSearchStringToFilter tC)
  assertEqual "Should match" eD (map stripDocumentFilter $ processSearchStringToFilter tD)
  assertEqual "Should match" eE (map stripDocumentFilter $ processSearchStringToFilter tE)
  assertEqual "Should match" eF (map stripDocumentFilter $ processSearchStringToFilter tF)
  assertEqual "Should match" eG (map stripDocumentFilter $ processSearchStringToFilter tG)
  assertEqual "Should match" eH (map stripDocumentFilter $ processSearchStringToFilter tH)
  assertEqual "Should match" eI (map stripDocumentFilter $ processSearchStringToFilter tI)
  assertEqual "Should match" eJ (map stripDocumentFilter $ processSearchStringToFilter tJ)

testGetDocumentsSQLTextFiltered :: TestEnv ()
testGetDocumentsSQLTextFiltered = replicateM_ 1 $ do
  -- setup
  Just author1 <- addNewUser "Bob" "Blue" "bill@zonk.com"
  Just author2 <- addNewUser "Anna" "Max" "herm@qqq.com"
  doc1 <- addRandomDocumentWithAuthorAndCondition author1 (isSignable && isPreparation)
  _doc2 <- addRandomDocumentWithAuthorAndCondition author1 (isSignable && isPreparation)
  _doc3 <- addRandomDocumentWithAuthorAndCondition author1 (isSignable && isPreparation)
  _doc4 <- addRandomDocumentWithAuthorAndCondition author2 (isSignable && isPreparation)

  let domain = DocumentsVisibleToUser $ userid author1
      doc1Title = "Magic Unique Title 42"

  actor <- arbitraryAuthorActor
  success <- withDocument doc1 $ dbUpdate $ SetDocumentTitle doc1Title (actor)
  assert success
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc1
  assertEqual ("Document title really got changed") doc1Title (documenttitle ndoc)

  getDocs0 <- dbQuery $ GetDocuments domain [] [] maxBound
  assertEqual ("GetDocuments fetches all documents by author without filter") 3 (length getDocs0)

  let matchTitleFilter1 = processSearchStringToFilter "Magic Unique Title 42"
  matchTitleFilter1Matches <- dbQuery $ GetDocuments domain matchTitleFilter1 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter1) 1 (length matchTitleFilter1Matches)

  let matchTitleFilter2 = processSearchStringToFilter "\"Magic Unique Title 42\""
  matchTitleFilter2Matches <- dbQuery $ GetDocuments domain matchTitleFilter2 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter2) 1 (length matchTitleFilter2Matches)

  let matchTitleFilter3 = processSearchStringToFilter "\"Unique Title 42\""
  matchTitleFilter3Matches <- dbQuery $ GetDocuments domain matchTitleFilter3 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter3) 1 (length matchTitleFilter3Matches)

  let matchTitleFilter4 = processSearchStringToFilter "agic nique itle"
  matchTitleFilter4Matches <- dbQuery $ GetDocuments domain matchTitleFilter4 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter4) 1 (length matchTitleFilter4Matches)

  let matchTitleFilter5 = processSearchStringToFilter "Magic \"Unique Title\" 42"
  matchTitleFilter5Matches <- dbQuery $ GetDocuments domain matchTitleFilter5 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter5) 1 (length matchTitleFilter5Matches)

  let matchTitleFilter6 = processSearchStringToFilter "42 Unique \"Title\" Magic"
  matchTitleFilter6Matches <- dbQuery $ GetDocuments domain matchTitleFilter6 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter6) 1 (length matchTitleFilter6Matches)

  let notMatchTitleFilter1 = processSearchStringToFilter "\"Magic Unique Title 4\""
  notMatchTitleFilter1Matches <- dbQuery $ GetDocuments domain notMatchTitleFilter1 [] maxBound
  assertEqual ("With filter " ++ show notMatchTitleFilter1) 1 (length notMatchTitleFilter1Matches)

  let notMatchTitleFilter2 = processSearchStringToFilter "\"agic Unique Title 42\""
  notMatchTitleFilter2Matches <- dbQuery $ GetDocuments domain notMatchTitleFilter2 [] maxBound
  assertEqual ("With filter " ++ show notMatchTitleFilter2) 1 (length notMatchTitleFilter2Matches)

  let matchFirstNameFilter = processSearchStringToFilter "Bob Blue"
  matchFirstNameFilterMatches <- dbQuery $ GetDocuments domain matchFirstNameFilter [] maxBound
  assertEqual ("With filter " ++ show matchFirstNameFilter) 3 (length matchFirstNameFilterMatches)

  let notMatchFirstNameFilter = processSearchStringToFilter "\"Bob Blue\""
  notMatchFirstNameFilterMatches <- dbQuery $ GetDocuments domain notMatchFirstNameFilter [] maxBound
  assertEqual ("With filter " ++ show notMatchFirstNameFilter) 0 (length notMatchFirstNameFilterMatches)

  let matchEmailFilter1 = processSearchStringToFilter "bill@"
  matchEmailFilter1Matches <- dbQuery $ GetDocuments domain matchEmailFilter1 [] maxBound
  assertEqual ("With filter " ++ show matchEmailFilter1) 3 (length matchEmailFilter1Matches)

  let matchEmailFilter2 = processSearchStringToFilter "\"bill@zonk.com\""
  matchEmailFilter2Matches <- dbQuery $ GetDocuments domain matchEmailFilter2 [] maxBound
  assertEqual ("With filter " ++ show matchEmailFilter2) 3 (length matchEmailFilter2Matches)

  let notMatchEmailFilter1 = processSearchStringToFilter "\"bill@\""
  notMatchEmailFilter1Matches <- dbQuery $ GetDocuments domain notMatchEmailFilter1 [] maxBound
  assertEqual ("With filter " ++ show notMatchEmailFilter1) 3 (length notMatchEmailFilter1Matches)

  let notMatchEmailFilter2 = processSearchStringToFilter "herm@"
  notMatchEmailFilter2Matches <- dbQuery $ GetDocuments domain notMatchEmailFilter2 [] maxBound
  assertEqual ("With filter " ++ show notMatchEmailFilter2) 0 (length notMatchEmailFilter2Matches)

testGetDocumentsSQLSorted :: TestEnv ()
testGetDocumentsSQLSorted = replicateM_ 1 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author (const True)

  let domain = DocumentsVisibleToUser $ userid author
      filters = []
  _docs <- dbQuery $ GetDocuments domain filters
            [ Desc DocumentOrderByTitle
            , Desc DocumentOrderByMTime
            , Desc DocumentOrderByStatusClass
            , Desc DocumentOrderByType
            , Desc DocumentOrderByPartners
            ]
            maxBound
  return ()

testCreateFromSharedTemplate :: TestEnv ()
testCreateFromSharedTemplate = do
  user <- addNewRandomUser
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (\doc -> isPreparation doc)
  tmpdoc <- dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           _ <- withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  newuser <- addNewRandomUser

  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor newuser doc (systemActor mt))
  _ <- withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (systemActor mt)

  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  let [author1] = filter isAuthor $ documentsignatorylinks doc
  let [author2] = filter isAuthor $ documentsignatorylinks ndoc
  if (fmap fieldTextValue $ filter (\f -> fieldType f == TextFT) $ signatoryfields $ author1)
     == (fmap fieldTextValue $ filter (\f -> fieldType f == TextFT) $ signatoryfields $ author2)
    then assertSuccess
    else assertFailure "Replacing signatory details based on user is loosing fields | SKRIVAPADEV-294"


testCreateFromTemplateCompanyField :: TestEnv ()
testCreateFromTemplateCompanyField = replicateM_ 10 $ do
  user <- addNewRandomUser
  company <- addNewCompany
  _ <- dbUpdate $ SetUserCompany (userid user)  (companyid company)
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (\doc -> isPreparation doc)
  tmpdoc <- dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           _ <- withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor user' doc (systemActor mt))
  _ <- withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (systemActor mt)
  doc' <- dbQuery $ GetDocumentByDocumentID docid'
  let [author] = filter isAuthor $ documentsignatorylinks doc'
  assertEqual "Author signatory link company name is not same as his company" (getCompanyName company) (getCompanyName author)



testAddDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testAddDocumentAttachmentFailsIfNotPreparation = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    file <- addNewRandomFile
    --execute
    success <- randomUpdate $ \t->AddDocumentAttachment "file.pdf" False True file (systemActor t)
    --assert
    assert $ not success

testAddDocumentAttachmentOk :: TestEnv ()
testAddDocumentAttachmentOk = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    --execute

    success <- randomUpdate $ \t->AddDocumentAttachment "file.pdf" False True file (systemActor t)

    --assert
    assert success
    assertEqual "Author attachment was really attached" [file]
                    . map authorattachmentfileid . documentauthorattachments =<< theDocument

testRemoveDocumentAttachmentsFailsIfNotPreparation :: TestEnv ()
testRemoveDocumentAttachmentsFailsIfNotPreparation = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    --execute
    success <- randomUpdate $ \t -> RemoveDocumentAttachments (unsafeFileID 0) (systemActor t)
    --assert
    assert $ not success

testRemoveDocumentAttachmentsOk :: TestEnv ()
testRemoveDocumentAttachmentsOk = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    --execute
    success <- randomUpdate $ \t -> RemoveDocumentAttachments (unsafeFileID 0) (systemActor t)
    --assert
    assert $ not success

---------------------------------------------------------------------

testUpdateSigAttachmentsAttachmentsOk :: TestEnv ()
testUpdateSigAttachmentsAttachmentsOk = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file1 <- addNewRandomFile
    file2 <- addNewRandomFile
    --execute
    let att1 = def
          { signatoryattachmentfile = Just file1
          , signatoryattachmentfilename = Just "afile1.ran"
          , signatoryattachmentname = "att1"
          , signatoryattachmentdescription = "att1 description"
          }
    let att2 = def
          { signatoryattachmentname = "att2"
          , signatoryattachmentdescription = "att2 description"
          }
    (time, sl) <- rand 10 arbitrary
    let sa = signatoryActor (set ctxtime time ctx) sl
    sls <- documentsignatorylinks <$> theDocument
    randomUpdate . SetSigAttachments (signatorylinkid $ sls !! 0) [att1, att2] =<< sa

    doc1 <- theDocument
    randomUpdate . DeleteSigAttachment (signatorylinkid $ sls !! 0) att1 =<< sa
    ndoc1 <- theDocument

    randomUpdate . SaveSigAttachment (signatorylinkid $ sls !! 0) att1 file2 =<< sa
    ndoc2 <- theDocument

    --assert
    assertEqual "Both attachments were attached" 2 (length (signatoryattachments $ (documentsignatorylinks doc1) !! 0))

    assertBool "All signatory attachments are not connected to files" (all (isNothing . signatoryattachmentfile)
                                                                             (signatoryattachments $ (documentsignatorylinks ndoc1) !! 0))

    assertBool "Attachment connected to signatory"
                   (Just file2 `elem` map signatoryattachmentfile (signatoryattachments $ (documentsignatorylinks ndoc2) !! 0))

------------------------------------------------

testTimeoutDocumentNonSignableLeft :: TestEnv ()
testTimeoutDocumentNonSignableLeft = replicateM_ 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    withDocument doc $ dbUpdate $ TimeoutDocument (systemActor mt)

testTimeoutDocumentSignableNotPendingLeft :: TestEnv ()
testTimeoutDocumentSignableNotPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable && (not . isPending))
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    withDocument doc $ randomUpdate $ \t->TimeoutDocument (systemActor t)

testTimeoutDocumentSignablePendingRight :: TestEnv ()
testTimeoutDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    --execute
    randomUpdate $ \t->TimeoutDocument (systemActor t)
    assertInvariants =<< theDocument

testTimeoutDocumentSignableNotLeft :: TestEnv ()
testTimeoutDocumentSignableNotLeft = replicateM_ 10 $ do
  actor <- arbitrarySystemActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    d <- rand 10 arbitrary
    withDocument d $ randomUpdate $ TimeoutDocument actor

testSignDocumentNonSignableLeft :: TestEnv ()
testSignDocumentNonSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) (signatorymagichash sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignableNotPendingLeft :: TestEnv ()
testSignDocumentSignableNotPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && (not . isPending)) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) (signatorymagichash sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignablePendingRight :: TestEnv ()
testSignDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending &&
           any ((== StandardAuthenticationToSign) . signatorylinkauthenticationtosignmethod&& isSignatory && (not . hasSigned)) . documentsignatorylinks) `withDocumentM` do
    Just sl <- find ((== StandardAuthenticationToSign) . signatorylinkauthenticationtosignmethod&& isSignatory && (not . hasSigned)) .documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary
    randomUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
    randomUpdate $ SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentSignablePendingElegRight :: TestEnv ()
testSignDocumentSignablePendingElegRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending &&
           any ((== SEBankIDAuthenticationToSign) . signatorylinkauthenticationtosignmethod&& isSignatory && (not . hasSigned)) . documentsignatorylinks) `withDocumentM` do
    Just sl <- find ((== SEBankIDAuthenticationToSign) . signatorylinkauthenticationtosignmethod&& isSignatory && (not . hasSigned)) . documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary
    randomUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
    randomUpdate $ \signinfo -> SignDocument (signatorylinkid sl) (signatorymagichash sl) (Just signinfo) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentNotLeft :: TestEnv ()
testSignDocumentNotLeft = replicateM_ 10 $ do
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    -- our machinery is broken here, baseline condition has only relations
    -- this should be ignored and properly return info about non existing document
    d <- rand 10 arbitrary
    withDocument d $ randomUpdate $ \sl mh si t -> SignDocument sl mh si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testPreparationToPendingNotSignableLeft :: TestEnv ()
testPreparationToPendingNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         } `withDocumentM` do
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      tz <- mkTimeZoneName "Europe/Stockholm"
      randomUpdate $ PreparationToPending (systemActor time) tz

testPreparationToPendingSignableNotPreparationLeft :: TestEnv ()
testPreparationToPendingSignableNotPreparationLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = documentAllStatuses \\ [Preparation]
         } `withDocumentM` do
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      tz <- mkTimeZoneName "Europe/Stockholm"
      randomUpdate $ PreparationToPending (systemActor time) tz

testPreparationToPendingNotLeft :: TestEnv ()
testPreparationToPendingNotLeft = replicateM_ 100 $ do
  (time, did) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    tz <- mkTimeZoneName "Europe/Stockholm"
    withDocumentID did $ randomUpdate $ PreparationToPending (systemActor time) tz

testPreparationToPendingSignablePreparationRight :: TestEnv ()
testPreparationToPendingSignablePreparationRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Preparation]
         , randomDocumentCondition = (any isSignatory . documentsignatorylinks) &&
          (isJust . documentfile) &&
          ((==) 1 . length . filter isAuthor . documentsignatorylinks)
         } `withDocumentM` do
    time <- rand 10 arbitrary
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor time) tz
    assertInvariants =<< theDocument

testRejectDocumentNotSignableLeft :: TestEnv ()
testRejectDocumentNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) Nothing
             (authorActor (set ctxtime time ctx) author)

testRejectDocumentSignableNotPendingLeft :: TestEnv ()
testRejectDocumentSignableNotPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author (isSignable && not . isPending) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) Nothing
             (authorActor (set ctxtime time ctx) author)

testRejectDocumentNotLeft :: TestEnv ()
testRejectDocumentNotLeft = replicateM_ 10 $ do
  _ <- addRandomDocument . randomDocumentAllowsDefault =<< addNewRandomUser
  ctx <- mkContext def
  (did, time, sl) <- rand 10 arbitrary
  let sa = signatoryActor (set ctxtime time ctx) sl
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID did $ randomUpdate . RejectDocument (signatorylinkid sl) Nothing =<< sa

testRejectDocumentSignablePendingRight :: TestEnv ()
testRejectDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . filter (signatoryispartner) . documentsignatorylinks =<< theDocument
    Just sl <- getSigLinkFor slid <$> theDocument
    time <- rand 10 arbitrary
    let sa = signatoryActor (set ctxtime time ctx) sl
    randomUpdate . RejectDocument slid Nothing =<< sa

    assertInvariants =<< theDocument

testMarkInvitationRead :: TestEnv ()
testMarkInvitationRead = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author
         (isPending && (all (isNothing . maybereadinvite) . documentsignatorylinks)) `withDocumentM` do

    sl' <- rand 10 . elements . documentsignatorylinks =<< theDocument
    let slid = signatorylinkid sl'
    time <- currentTime
    success <- dbUpdate . MarkInvitationRead slid
               =<< signatoryActor (set ctxtime time ctx) sl'

    assert success
    Just sl <- getSigLinkFor slid <$> theDocument
    assertEqual "Invitation read time should be set." (Just True) (liftA2 compareTime (Just time) (maybereadinvite sl))

testMarkInvitationReadDocDoesntExist :: TestEnv ()
testMarkInvitationReadDocDoesntExist = replicateM_ 10 $ do
  ctx <- mkContext def
  (did, sl, time) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist{} -> True) $ do
    _ <- withDocumentID did $ randomUpdate . MarkInvitationRead (signatorylinkid sl)
            =<< signatoryActor (set ctxtime time ctx) sl
    return ()
  return ()

testMarkDocumentSeenNotSignableLeft :: TestEnv ()
testMarkDocumentSeenNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         } `withDocumentM` do

    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor (set ctxtime time ctx) sl
        assertRaisesKontra (\DocumentTypeShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa

testMarkDocumentSeenClosedOrPreparationLeft :: TestEnv ()
testMarkDocumentSeenClosedOrPreparationLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         } `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor (set ctxtime time ctx) sl
        assertRaisesKontra (\DocumentStatusShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa

testMarkDocumentSeenNotLeft :: TestEnv ()
testMarkDocumentSeenNotLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  _doc <- addRandomDocument (randomDocumentAllowsDefault author)
  (d, s, m) <- rand 10 arbitrary
  a <- arbitrarySignatoryActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID d $ randomUpdate $ MarkDocumentSeen s m a

forEachSignatoryLink :: Monad m => (SignatoryLink -> m ()) -> Document -> m ()
forEachSignatoryLink fn doc =
  let f [] = return ()
      f (sl:sls) = do
        fn sl
        f sls
  in f (documentsignatorylinks doc)

testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author (isSignable && (not . (isClosed || isPreparation))) `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
                when (not $ hasSeen sl) $ do
                  time <- rand 10 arbitrary
                  let sa = signatoryActor (set ctxtime time ctx) sl
                  randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa
                  Just tsl <- getSigLinkFor (signatorylinkid sl) <$> theDocument
                  assertBool "Signatorylink should be marked seen now." (hasSeen tsl)

testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthorAndCondition author (isSignable && (not . (isClosed || isPreparation))) `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (not $ hasSeen sl) $ do
        mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
        time <- rand 10 arbitrary
        let sa = signatoryActor (set ctxtime time ctx) sl
        assertRaisesKontra (\SignatoryTokenDoesNotMatch {} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) mh =<< sa

testSetInvitationDeliveryStatusNotSignableLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    actor <- arbitrarySystemActor
    Just sl <- getAuthorSigLink <$> theDocument
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      success <- randomUpdate $ \st-> SetEmailInvitationDeliveryStatus (signatorylinkid sl) st actor
      assert $ not success


testSetInvitationDeliveryStatusNotLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotLeft = replicateM_ 10 $ do
  actor <- arbitrarySystemActor
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    d <- rand 10 arbitrary
    success <- withDocumentID d $ randomUpdate $ \s st-> SetEmailInvitationDeliveryStatus s st actor
    assert $ not success

testSetInvitationDeliveryStatusSignableRight :: TestEnv ()
testSetInvitationDeliveryStatusSignableRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isSignable `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . documentsignatorylinks =<< theDocument
    st <- rand 10 arbitrary
    actor <- arbitrarySystemActor
    success <- randomUpdate $ SetEmailInvitationDeliveryStatus slid st actor
    assert success

testSetDocumentTagsRight :: TestEnv ()
testSetDocumentTagsRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  addRandomDocumentWithAuthor' author `withDocumentM` do
    (tags, time) <- first S.fromList <$> rand 10 arbitrary
    let actor = authorActor (set ctxtime time ctx) author
    success <- randomUpdate $ SetDocumentTags tags actor

    assert success
    assertEqual "Tags should be equal" tags . documenttags =<< theDocument

testCloseDocumentSignableButNotEverybodyHasSigned :: TestEnv ()
testCloseDocumentSignableButNotEverybodyHasSigned = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = (\doc -> length (documentsignatorylinks doc) > 1) &&
                                     (not . all (isSignatory --> hasSigned) . documentsignatorylinks)
         } `withDocumentM` do
    sa <- arbitrarySystemActor
    assertRaisesKontra (\(SignatoryHasNotYetSigned {}) -> True) $ do
      randomUpdate $ CloseDocument sa

testCloseDocumentNotSignableNothing :: TestEnv ()
testCloseDocumentNotSignableNothing = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory --> hasSigned) . documentsignatorylinks))
         } `withDocumentM` do
    sa <- arbitrarySystemActor
    assertRaisesKontra (\(DocumentTypeShouldBe {}) -> True) $ do
      randomUpdate $ CloseDocument sa

testCloseDocumentNotNothing :: TestEnv ()
testCloseDocumentNotNothing = replicateM_ 10 $ do
  sa <- arbitrarySystemActor
  did <- rand 10 arbitrary
  assertRaisesKontra (\(DocumentDoesNotExist {}) -> True) $ do
    withDocumentID did $ randomUpdate $ CloseDocument sa

testCancelDocumentNotSignableNothing :: TestEnv ()
testCancelDocumentNotSignableNothing = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext def
  time <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory --> hasSigned) . documentsignatorylinks))
         } `withDocumentM` do

    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $
                 randomUpdate $ CancelDocument
                                (authorActor (set ctxtime time ctx) author)

testCancelDocumentNotNothing :: TestEnv ()
testCancelDocumentNotNothing = replicateM_ 10 $ do
  aa <- arbitraryAuthorActor
  did <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $
             withDocumentID did $ randomUpdate $ CancelDocument aa

testSetDocumentTitleNotLeft :: TestEnv ()
testSetDocumentTitleNotLeft = replicateM_ 10 $ do
  (did, StringNoNUL title) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- withDocumentID did $ randomUpdate $ SetDocumentTitle title actor
  assert $ not success

testSetDocumentTitleRight :: TestEnv ()
testSetDocumentTitleRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed)
         } `withDocumentM` do
    let title = "my new cool title"
    success <- randomUpdate $ SetDocumentTitle title actor

    assert success
    assertEqual "Title is set properly" title . documenttitle =<< theDocument

testSetDocumentDaysToSignNotLeft :: TestEnv ()
testSetDocumentDaysToSignNotLeft = replicateM_ 10 $ do
  (did, d) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- withDocumentID did $ randomUpdate $ SetDaysToSign d actor
  assert $ not success

testSetDocumentDaysToSignRight :: TestEnv ()
testSetDocumentDaysToSignRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = not . isClosed
         } `withDocumentM` do
    let daystosign = 15
    success1 <- randomUpdate $ SetDaysToSign daystosign actor

    assert success1
    assertEqual "Days to sign is set properly" daystosign . documentdaystosign =<< theDocument

testSetShowHeader :: TestEnv ()
testSetShowHeader = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentCondition = isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowHeader targetValue actor
    newValue <- documentshowheader <$> theDocument
    assertEqual "SetShowHeader changes value to target value" targetValue newValue
    assertEqual "SetShowHeader return success" success True

testSetShowPDFDownload :: TestEnv ()
testSetShowPDFDownload = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentCondition = isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowPDFDownload targetValue actor
    newValue <- documentshowpdfdownload <$> theDocument
    assertEqual "SetShowPDFDownload changes value to target value" targetValue newValue
    assertEqual "SetShowPDFDownload return success" success True

testSetShowRejectOption :: TestEnv ()
testSetShowRejectOption = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentCondition = isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowRejectOption targetValue actor
    newValue <- documentshowrejectoption <$> theDocument
    assertEqual "SetShowRejectOption changes value to target value" targetValue newValue
    assertEqual "SetShowRejectOption return success" success True

testSetAllowRejectReason :: TestEnv ()
testSetAllowRejectReason = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentCondition = isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetAllowRejectReason targetValue actor
    newValue <- documentallowrejectreason <$> theDocument
    assertEqual "SetAllowRejectReason changes value to target value" targetValue newValue
    assertEqual "SetAllowRejectReason return success" success True

testSetShowFooter :: TestEnv ()
testSetShowFooter = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentCondition = isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowFooter targetValue actor
    newValue <- documentshowfooter <$> theDocument
    assertEqual "SetShowFooter changes value to target value" targetValue newValue
    assertEqual "SetShowFooter return success" success True

assertInvariants :: (MonadIO m, MonadTime m) => Document -> m ()
assertInvariants document = do
  now <- currentTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a

testGetDocumentsByCompanyWithFilteringCompany :: TestEnv ()
testGetDocumentsByCompanyWithFilteringCompany = replicateM_ 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  withDocumentID did $ do
    time <- currentTime
    let actor = systemActor time
    _ <- dbUpdate $ SetDocumentTags (S.singleton $ DocumentTag name value) actor
    docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound

    assertEqual "Should have 1 document returned" 1 (length docs')


testGetDocumentsByCompanyWithFilteringFilters :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFilters = replicateM_ 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name value]] [] maxBound
  docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound

  assertBool "Should have no documents returned" (null docs)
  assertEqual "Should have 1 document returned" [did] (map documentid docs')

testSetDocumentUnsavedDraft :: TestEnv ()
testSetDocumentUnsavedDraft = replicateM_ 10 $ do
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  withDocumentID did $ do
    isdraft <- (isSignable && isPreparation) <$> theDocument

    docs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] maxBound
    _ <- dbUpdate $ SetDocumentUnsavedDraft True
    docs2 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] maxBound
    _ <- dbUpdate $ SetDocumentUnsavedDraft False
    docs3 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] maxBound
    docs4 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft True, DocumentFilterByDocumentID did] [] maxBound

    assertEqual "Should return the document" [did] (map documentid docs1)
    assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs2)
    assertEqual "Should return the document" [did] (map documentid docs3)
    assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs4)


testGetDocumentsByCompanyWithFilteringFinds :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFinds = replicateM_ 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- currentTime
  let actor = systemActor time
  _ <- withDocumentID did $ dbUpdate $ SetDocumentTags (S.singleton $ DocumentTag name value) actor
  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name value]] [] maxBound
  docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound

  assertEqual "Should have one document returned" [did] (map documentid docs)
  assertEqual "Should have one document returned" [did] (map documentid docs')

testGetDocumentsByCompanyWithFilteringFindsMultiple :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFindsMultiple = replicateM_ 10 $ do
  (StringNoNUL name1, StringNoNUL value1) <- rand 10 arbitrary
  (StringNoNUL name2, StringNoNUL value2) <- rand 10 arbitrary
  (StringNoNUL name3, StringNoNUL value3) <- rand 10 arbitrary
  if (name1 /= name2 && name1 /= name2 && name2 /= name3)
   then do
    company <- addNewCompany
    author <- addNewRandomUser
    time <- currentTime
    let actor = systemActor time
    _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
    Just author' <- dbQuery $ GetUserByID (userid author)
    did <- addRandomDocumentWithAuthor author'

    _ <- withDocumentID did $ dbUpdate $ SetDocumentTags (S.fromList [DocumentTag name1 value1, DocumentTag name2 value2]) actor
    docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name1 value1]] [] maxBound
    docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name2 value2]] [] maxBound
    docs'' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2]] [] maxBound
    docs''' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound
    docs'''' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2, DocumentTag name3 value3]] [] maxBound

    assertEqual "Should have one document returned" [did] (map documentid docs)
    assertEqual "Should have one document returned" [did] (map documentid docs')
    assertEqual "Should have one document returned" [did] (map documentid docs'')
    assertEqual "Should have one document returned" [did] (map documentid docs''')
    assertEqual "Should have zero documents returned" [] (map documentid docs'''')
   else return ()

testStatusClassSignedWhenAllSigned :: TestEnv ()
testStatusClassSignedWhenAllSigned = replicateM_ 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable && isClosed && ((<=) 2 . length . (filter isSignatory) . documentsignatorylinks))
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)

  assertEqual "Statusclass for signed documents is signed" SCSigned (documentstatusclass doc')

-- Moved from Eq instance of SignatoryLink. Instance got dropped as it is not usefull in main server - but it's good to have way to compare SignatoryLinks in tests.
signatoryLinksAreAlmostEqualForTests :: SignatoryLink -> SignatoryLink -> Bool
signatoryLinksAreAlmostEqualForTests a b = and [
      signatorylinkid a == signatorylinkid b
    , fieldsListsAreAlmostEqual (sortFields (signatoryfields a)) (sortFields (signatoryfields b))
    , signatoryisauthor a == signatoryisauthor b
    , signatoryispartner a == signatoryispartner b
    , signatorysignorder a == signatorysignorder b
    , signatorymagichash a == signatorymagichash b
    , maybesignatory a == maybesignatory b
    , maybesigninfo a == maybesigninfo b
    , maybeseeninfo a == maybeseeninfo b
    , maybereadinvite a == maybereadinvite b
    , mailinvitationdeliverystatus a == mailinvitationdeliverystatus b
    , smsinvitationdeliverystatus a == smsinvitationdeliverystatus b
    , signatorylinkdeleted a == signatorylinkdeleted b
    , signatorylinkreallydeleted a == signatorylinkreallydeleted b
    , signatorylinkcsvupload a == signatorylinkcsvupload b
    , signatoryattachments a == signatoryattachments b
    , signatorylinksignredirecturl a == signatorylinksignredirecturl b
    , signatorylinkrejectredirecturl a == signatorylinkrejectredirecturl b
    , signatorylinkrejectiontime a == signatorylinkrejectiontime b
    , signatorylinkrejectionreason a == signatorylinkrejectionreason b
    , signatorylinkauthenticationtosignmethod a == signatorylinkauthenticationtosignmethod b
    , signatorylinkdeliverymethod a == signatorylinkdeliverymethod b
    , signatorylinkconfirmationdeliverymethod a == signatorylinkconfirmationdeliverymethod b
    ]
    where
      sortFields = sortBy (\f1 f2  -> compare (fieldIdentity f1) (fieldIdentity f2))

signatoryLinksListsAreAlmostEqualForTests :: [SignatoryLink] -> [SignatoryLink] -> Bool
signatoryLinksListsAreAlmostEqualForTests (s:ss) (s':ss') = signatoryLinksAreAlmostEqualForTests s s' && signatoryLinksListsAreAlmostEqualForTests ss ss'
signatoryLinksListsAreAlmostEqualForTests [] [] = True
signatoryLinksListsAreAlmostEqualForTests _ _  = False
