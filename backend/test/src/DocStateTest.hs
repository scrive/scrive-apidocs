module DocStateTest (docStateTests, docStateSideEffectsTests) where

import Control.Arrow (first)
import Control.Conditional ((<|), (|>))
import Control.Monad.Catch (try)
import Control.Monad.Reader
import Data.Int
import Data.Text (unpack)
import Log
import Test.Framework
import Test.QuickCheck
import qualified Data.Label.Base as FCP
import qualified Data.Label.Partial as FCP
import qualified Data.Set as S
import qualified Data.Text as T

import Context (ctxpdftoolslambdaenv, ctxtime)
import DataRetentionPolicy
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.API.V2.JSON.SignatoryConsentQuestion
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
import Doc.SignatoryConsentQuestionID
import Doc.SignatoryFieldID
import Doc.TestInvariants
import EID.Signature.Model (ESignature(..))
import EvidenceLog.Model
import File.FileID
import MinutesTime
import PdfToolsLambda.Conf
import TestingUtil
import TestKontra
import TestKontra (mkContext)
import Text.XML.DirtyContent (renderXMLContent)
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots

docStateSideEffectsTests :: TestEnvSt -> Test
docStateSideEffectsTests env =
  testGroup "DocState side effects"
  [ testThat "Author user ID for documents relation is updated"
    env testDocumentAuthorUserID
  , testThat "Triggers update search field in documents relation"
    env testSignDocumentSearchData
  ]

docStateTests :: TestEnvSt -> Test
docStateTests env = testGroup "DocState"
  [ testThat "RejectDocument adds to the log"
    env testRejectDocumentEvidenceLog
  , testThat "RestartDocument adds to the log"
    env testRestartDocumentEvidenceLog
  , testThat "RestartDocument keeps signatorylinkhidepn"
    env testRestartDocumentKeepsHidePN
  , testThat "SignDocument adds to the log"
    env testSignDocumentEvidenceLog
  , testThat "TimeoutDocument adds to the log"
    env testTimeoutDocumentEvidenceLog
  , testThat "ProlongTimeoutedDocument can be executed and adds to a log"
    env testProlongTimeoutedDocument
  , testThat "ProlongPendingDocument can be executed and adds to a log"
    env testProlongPendingDocument

  , testThat "Documents are shared in company properly"
    env testGetDocumentsSharedInCompany
  , testThat "SetDocumentUnsavedDraft and filtering based on unsaved_draft works"
    env testSetDocumentUnsavedDraft
  , testThat "Documents sorting SQL syntax is correct"
    env testGetDocumentsSQLSorted
  , testThat "Search string pre-processing works"
    env testProcessSearchStringToFilter
  , testThat "Documents searching by text works"
    env testGetDocumentsSQLTextFiltered

  , testThat "PreparationToPending adds to the log"
    env testPreparationToPendingEvidenceLog
  , testThat "MarkInvitationRead adds correct text to the log"
    env testMarkInvitationReadEvidenceLog
  , testThat "SaveSigAttachment adds to the log"
    env testSaveSigAttachmentEvidenceLog
  , testThat "DeleteSigAttachment will not work after signing"
    env testDeleteSigAttachmentAlreadySigned
  , testThat "DeleteSigAttachment adds to the log"
    env testDeleteSigAttachmentEvidenceLog
  , testThat "CloseDocument adds to the log"
    env testCloseDocumentEvidenceLog
  , testThat "ChangeSignatoryEmail adds to the log"
    env testChangeSignatoryEmailEvidenceLog
  , testThat "CancelDocument adds to the log"
    env testCancelDocumentEvidenceLog

  , testThat "AppendFirstSealedFile adds to the log"
    env testAppendFirstSealedFileEvidenceLog
  , testThat "GetDocumentsByCompanyWithFiltering filters"
    env testGetDocumentsByCompanyWithFilteringFilters
  , testThat "GetDocumentsByCompanyWithFiltering finds"
    env testGetDocumentsByCompanyWithFilteringFinds
  , testThat "GetDocumentsByCompanyWithFiltering finds with multiple"
    env testGetDocumentsByCompanyWithFilteringFindsMultiple
  , testThat "GetDocumentsByCompanyWithFiltering finds with company filter"
    env testGetDocumentsByCompanyWithFilteringCompany
  , testThat "NewDocument inserts a new contract for a single user successfully"
    env testNewDocumentForNonCompanyUser
  , testThat "NewDocument inserts a new contract for a company user successfully"
    env testNewDocumentForACompanyUser

  , testThat "CancelDocument cancels a document"
    env testCancelDocumentCancelsDocument
  , testThat "CancelDocument fails if doc not pending or awaiting author"
    env testCancelDocumentReturnsLeftIfDocInWrongState

  , testThat "SetDocumentLang fails when doc doesn't exist"
    env testSetDocumentLangNotLeft

  , testThat "SetDocumentTitle fails when doc doesn't exist"
    env testSetDocumentTitleNotLeft
  , testThat "SetDocumentTitle succeeds when doc exists and has proper status"
    env testSetDocumentTitleRight

  , testThat "SetDaysToSign fails when doc doesn't exist"
    env testSetDocumentDaysToSignNotLeft
  , testThat "SetDaysToSign and RemoveDaysToSign succeed when doc exist and has proper status"
    env testSetDocumentDaysToSignRight

  , testThat "CloseDocument fails when doc is not signable"
    env testCloseDocumentNotSignableNothing
  , testThat "CloseDocument fails when doc doesn't exist"
    env testCloseDocumentNotNothing
  , testThat "CloseDocument fails when doc is signable but not everybody has signed"
    env testCloseDocumentSignableButNotEverybodyHasSigned

  , testThat "CancelDocument fails when doc is not signable"
    env testCancelDocumentNotSignableNothing
  , testThat "CancelDocument fails when doc doesn't exist"
    env testCancelDocumentNotNothing

  , testThat "SetDocumentTags succeeds"
    env testSetDocumentTagsRight

  , testThat "SetShowHeader works"
    env testSetShowHeader
  , testThat "SetShowPDFDownload succeeds"
    env testSetShowPDFDownload
  , testThat "SetShowRejectOption succeeds"
    env testSetShowRejectOption
  , testThat "SetAllowRejectReason succeeds"
    env testSetAllowRejectReason
  , testThat "SetShowFooter succeeds"
    env testSetShowFooter
  , testThat "SetShowArrow succeeds"
    env testSetShowArrow

  , testThat "GetTimeoutedButPendingDocuments works as expected"
    env testGetTimedOutButPendingDocuments

  , testThat "SetInvitationDeliveryStatus fails when not signable"
    env testSetInvitationDeliveryStatusNotSignableLeft
  , testThat "SetInvitationDeliveryStatus fails when doc does not exist"
    env testSetInvitationDeliveryStatusNotLeft
  , testThat "SetInvitationDeliveryStatus succeeds if signable"
    env testSetInvitationDeliveryStatusSignableRight

  , testThat "MarkDocumentSeen fails when not signable"
    env testMarkDocumentSeenNotSignableLeft
  , testThat "MarkDocumentSeen fails when closed or preparation"
    env testMarkDocumentSeenClosedOrPreparationLeft
  , testThat "MarkDocumentSeen fails when doc does not exist"
    env testMarkDocumentSeenNotLeft

  , testThat "MarkDocumentSeen succeeds when siglink match"
    env testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight

  , testThat "MarkInvitationRead when has not read"
    env testMarkInvitationRead
  , testThat "MarkInvitationRead never fails when doc doesn't exist"
    env testMarkInvitationReadDocDoesntExist

  , testThat "Seal document works"
    env testSealDocument
  , testThat "RejectDocument succeeds when signable and pending"
    env testRejectDocumentSignablePendingRight
  , testThat "RejectDocument fails when document doesn't exist"
    env testRejectDocumentNotLeft
  , testThat "RejectDocument fails when signable but not pending"
    env testRejectDocumentSignableNotPendingLeft
  , testThat "RejectDocument fails when not signable"
    env testRejectDocumentNotSignableLeft

  , testThat "ApproveDocument succeeds when signable and pending"
    env testApproveDocumentSignablePendingRight

--  , testThat "AuthorSignDocument succeeds when signable and preparation"
--    env testAuthorSignDocumentSignablePreparationRight
--  , testThat "AuthorSignDocument fails when document doesn't exist"
--    env testAuthorSignDocumentNotLeft
--  , testThat "AuthorSignDocument fails when signable but not preparation"
--    env testAuthorSignDocumentSignableNotPreparationLeft
--  , testThat "AuthorSignDocument fails when not signable"
--    env testAuthorSignDocumentNotSignableLeft

  , testThat "PreparationToPending succeeds when signable and preparation"
    env testPreparationToPendingSignablePreparationRight
  , testThat "PreparationToPending fails when document doesn't exist"
    env testPreparationToPendingNotLeft
  , testThat "PreparationToPending fails when signable but not preparation"
    env testPreparationToPendingSignableNotPreparationLeft
  , testThat "PreparationToPending fails when not signable"
    env testPreparationToPendingNotSignableLeft

  , testThat "UpdateConsentResponsesForSigning updates the responses to consent questions"
    env testUpdateConsentResponsesForSigningSuccess
  , testThat "UpdateConsentResponsesForSigning fails when the corresponding question doesn't exist"
    env testUpdateConsentResponsesForSigningWrongQuestionID

  , testThat "SignDocument fails when doc doesn't exist"
    env testSignDocumentNotLeft
  , testThat "SignDocument succeeds when doc is Signable and Pending (standard mode)"
    env testSignDocumentSignablePendingRight
  , testThat "SignDocument succeeds when doc is Signable and Pending (SE BankID)"
    env testSignDocumentSignablePendingSEBankIDRight
  , testThat "SignDocument succeeds when doc is Signable and Pending (NO BankID)"
    env testSignDocumentSignablePendingNOBankIDRight
  , testThat "SignDocument succeeds when doc is Signable and Pending (DK NemID)"
    env testSignDocumentSignablePendingDKNemIDRight
  , testThat "SignDocument fails when the document is Signable but not in Pending"
    env testSignDocumentSignableNotPendingLeft
  , testThat "SignDocument fails when document is not signable"
    env testSignDocumentNonSignableLeft

  , testThat "TimeoutDocument fails when doc doesn't exist"
    env testTimeoutDocumentSignableNotLeft
  , testThat "TimeoutDocument succeeds when doc is Signable and Pending"
    env testTimeoutDocumentSignablePendingRight
  , testThat "TimeoutDocument fails when the document is Signable but not in Pending"
    env testTimeoutDocumentSignableNotPendingLeft
  , testThat "create document and check invariants"
    env testNewDocumentDependencies
  , testThat "can create new document and read it back with the returned id"
    env testDocumentCanBeCreatedAndFetchedByID

  --, testThat "when I call update document, it doesn't change the document id"
  --  env testDocumentUpdateDoesNotChangeID
  --, testThat "when I call update document, i can change the title"
  --  env testDocumentUpdateCanChangeTitle,

  , testThat "when I attach a file to a real document in preparation, it returns Right"
    env testDocumentAttachPreparationRight
  , testThat "when I attach a file to a real document not in preparation, it returns Right"
    env testDocumentAttachNotPreparationLeft
  , testThat "when I attach a file to a bad docid, it ALWAYS returns Left"
    env testNoDocumentAttachAlwaysLeft
  , testThat "when I attach a file, the file is attached"
    env testDocumentAttachHasAttachment

  , testThat "when I attach a sealed file to a bad docid, it always returns left"
    env testNoDocumentAppendSealedAlwaysLeft
  , testThat "when I attach a sealed file to a real doc, it always returns Right"
    env testDocumentAppendSealedPendingRight

  --, testThat "when I call updateDocument, it fails when the doc doesn't exist"
  --  env testNoDocumentUpdateDocumentAlwaysLeft
  --, testThat "When I call updateDocument with a doc that is not in Preparation, always returns left"
  --  env testNotPreparationUpdateDocumentAlwaysLeft
  --, testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right"
  --  env testPreparationUpdateDocumentAlwaysRight

  , testThat "when I create document from shared template author custom fields are stored"
    env testCreateFromSharedTemplate
  , testThat "when I create document from template company name is taken from company"
    env testCreateFromTemplateCompanyField

  , testThat "when I call ResetSignatoryDetails, it fails when the doc doesn't exist"
    env testNoDocumentResetSignatoryDetailsAlwaysLeft
  , testThat "When I call ResetSignatoryDetails with a doc that is not in Preparation, always returns left"
    env testNotPreparationResetSignatoryDetailsAlwaysLeft
  , testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right"
    env testPreparationResetSignatoryDetailsAlwaysRight
  , testThat "ResetSignatoryDetails works as expected"
    env testPreparationResetSignatoryDetails2Works

  , testThat "addDocumentAttachment fails if not in preparation"
    env testAddDocumentAttachmentFailsIfNotPreparation
  , testThat "addDocumentAttachment doesn't fail if there's no attachments"
    env testAddDocumentAttachmentOk

  , testThat "removeDocumentAttachments fails if not in preparation"
    env testRemoveDocumentAttachmentsFailsIfNotPreparation
  , testThat "removeDocumentAttachments return False if there's no attachments"
    env testRemoveDocumentAttachmentsOk

  , testThat "UpdateSigAttachments works as advertised"
    env testUpdateSigAttachmentsAttachmentsOk

  -- We need to do one that tests updateDocumentAttachment where there
  -- is an attachment
  , testThat "TimeoutDocument fails when document is not signable"
    env testTimeoutDocumentNonSignableLeft

  -- Archive & doc deletion tests
  , testThat "ArchiveDocument fails if the document is pending or awaiting author"
    env testArchiveDocumentPendingLeft
  , testThat "ArchiveDocument succeeds if the archiving user is the author"
    env testArchiveDocumentAuthorRight
  , testThat "ArchiveDocument succeeds if the archiving user is a company admin"
    env testArchiveDocumentCompanyAdminRight
  , testThat "RestoreArchivedDocument succeeds if the restoring user is the author"
    env testRestoreArchivedDocumentAuthorRight
  , testThat "RestoreArchivedDocument succeeds if the restoring user is the company admin"
    env testRestoreArchiveDocumentCompanyAdminRight

  , testThat "ArchiveDocument fails if the archiving user is an unrelated user"
    env testArchiveDocumentUnrelatedUserLeft
  , testThat "ArchiveDocument fails if the archiving user is just another standard company user"
    env testArchiveDocumentCompanyStandardLeft
  , testThat "RestoreArchivedDocument fails if the storing user is an unrelated user"
    env testRestoreArchivedDocumentUnrelatedUserLeft
  , testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user"
    env testRestoreArchiveDocumentCompanyStandardLeft

  , testThat "ReallyDeleteDocument works for author"
    env testReallyDeleteDocument
  , testThat "ReallyDeleteDocument works for author admin"
    env testReallyDeleteDocumentCompanyAdmin
  , testThat "ReallyDeleteDocument does not work for somebody else"
    env testReallyDeleteDocumentSomebodyElse

  , testThat "PurgeDocuments purges documents"
    env testPurgeDocument
  , testThat "PurgeDocuments does not purge documents for saved users"
    env testPurgeDocumentUserSaved
  , testThat "PurgeDocuments removes sensitive data"
    env testPurgeDocumentRemovesSensitiveData
  , testThat "PurgeDocuments does not purge shared templates"
    env testPurgeDocumentSharedTemplates
  , testThat "PurgeDocuments does not purge documents before a given number of\
           \ days except if it is the company's data retention policy"
    env testPurgeDocumentImmediateTrash

  , testThat "ArchiveIdleDocuments archives idle documents"
    env testArchiveIdleDocument

  , testThat "GetDocumentsByAuthor doesn't return archived docs"
    env testGetDocumentsByAuthorNoArchivedDocs
  , testThat "When document is signed it's status class is signed"
    env testStatusClassSignedWhenAllSigned
  , testThat "When document is pending and some invitation is undelivered it's status is undelivered"
    env testStatusClassSignedWhenAllSigned

  , testThat "ChangeAuthenticationToSignMethod works and evidence is as expected"
    env testChangeAuthenticationToSignMethod
  ]

testNewDocumentForNonCompanyUser :: TestEnv ()
testNewDocumentForNonCompanyUser = replicateM_ 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable) "doc title"
  assertGoodNewDocument Nothing (Signable) "doc title" result

testNewDocumentForACompanyUser :: TestEnv ()
testNewDocumentForACompanyUser = replicateM_ 10 $ do
  ug     <- addNewUserGroup
  ugwp   <- guardJustM . dbQuery . UserGroupGetWithParents . get ugID $ ug
  result <- performNewDocumentWithRandomUser (Just ug) (Signable) "doc title"
  assertGoodNewDocument (Just ugwp) (Signable) "doc title" result

testRejectDocumentEvidenceLog :: TestEnv ()
testRejectDocumentEvidenceLog = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author
    (   isSignable
     && isPending
     && ((<=) 2 . length . documentsignatorylinks)
     && (all (isSignatoryAndHasNotSigned || isApproverAndHasNotApproved) .
         filter (not . isAuthor) . documentsignatorylinks)) `withDocumentM` do
    sl <- guardJustM $getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$>
          theDocument
    randomUpdate $ \t -> RejectDocument (signatorylinkid sl) (isApprover sl)
                         Nothing (systemActor t)

    lg <- dbQuery . GetEvidenceLog  =<< theDocumentID
    let rejectEvent e = evType e `elem` [
            Current RejectDocumentEvidence
          , Current RejectDocumentByApproverEvidence
          ]
    assertJust $ find rejectEvent lg

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

testRestartDocumentKeepsHidePN :: TestEnv ()
testRestartDocumentKeepsHidePN = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable && isPending)
  withDocument doc $ randomUpdate $ \t->CancelDocument (systemActor t)
  cdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  mdoc <- randomUpdate $ \t->RestartDocument cdoc (systemActor t)
  assertJust mdoc
  assertEqual "After restart signatorylinkhidepn for all signatories is the same"
    (signatorylinkhidepn <$> documentsignatorylinks doc)
    (signatorylinkhidepn <$> documentsignatorylinks (fromJust mdoc))

getScreenshots :: TestEnv SignatoryScreenshots.SignatoryScreenshots
getScreenshots = do
  now <- currentTime
  first_  <- readTestFileAsBS "screenshots/s1.jpg"
  signing <- readTestFileAsBS "screenshots/s2.jpg"
  let mkss i = Just $ Screenshot.Screenshot
        { Screenshot.time  = now
        , Screenshot.image = i
        }
  return $ SignatoryScreenshots.emptySignatoryScreenshots
    { SignatoryScreenshots.first = mkss first_
    , SignatoryScreenshots.signing = mkss signing
    }

testSignDocumentEvidenceLog :: TestEnv ()
testSignDocumentEvidenceLog = do
  pdfSealLambdaEnv  <- asks (get tePdfToolsLambdaEnv)
  author            <- addNewRandomUser
  screenshots       <- getScreenshots

  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&
      isPending &&
      ((==) 2 . length . documentsignatorylinks) &&
      (all isSignatory . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      asl <- guardJustM $ getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) Nothing Nothing screenshots (systemActor t)

      sl <- guardJustM $ getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) Nothing Nothing screenshots (systemActor t)

      randomUpdate $ \t -> CloseDocument (systemActor t)

      lg <- dbQuery . GetEvidenceLog =<< theDocumentID
      assertJust $ find (\e -> evType e == Current SignDocumentEvidence) lg

      runPdfToolsLambdaT pdfSealLambdaEnv $ do
        sealDocument "https://scrive.com"

testSignDocumentSearchData :: TestEnv ()
testSignDocumentSearchData = do
  author <- addNewRandomUser

  screenshots <- getScreenshots
  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&
      isPending &&
      ((==) 2 . length . documentsignatorylinks) &&
      (all isSignatory . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do

      asl <- guardJustM $ getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) Nothing Nothing screenshots (systemActor t)

      sl <- guardJustM $ getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) Nothing Nothing screenshots (systemActor t)

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
      (all isSignatory . documentsignatorylinks) &&
      (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      docID <- theDocumentID
      -- `documents.author_user_id` should be set by now
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is updated upon document creation"
                      mDocAuthorUserID (Just $ userid author)

      asl <- guardJustM $ getSigLinkFor (isAuthor::SignatoryLink->Bool) <$> theDocument
      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid asl) (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document seen by author"
                      mDocAuthorUserID (Just $ userid author)
      randomUpdate $ \t -> SignDocument  (signatorylinkid asl) Nothing Nothing screenshots (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document signed by author"
                      mDocAuthorUserID (Just $ userid author)

      sl <- guardJustM $ getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$>
            theDocument

      randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
      dbQuery (GetAuthorUserID docID) >>= \mDocAuthorUserID ->
          assertEqual "Document author_user_id is not changed after document seen by signatory"
                      mDocAuthorUserID (Just $ userid author)

      randomUpdate $ \t -> SignDocument  (signatorylinkid sl) Nothing Nothing screenshots (systemActor t)
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


testProlongTimeoutedDocument :: TestEnv ()
testProlongTimeoutedDocument = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    success <- randomUpdate $ \t->TimeoutDocument (systemActor t)
    assert success
    randomUpdate $ \t -> ProlongTimeoutedDocument 2 defaultTimeZoneName (systemActor t)
    pending <- (\d ->  (Pending == documentstatus d))  <$> theDocument
    assert pending
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current ProlongDocumentEvidence) lg

testProlongPendingDocument :: TestEnv ()
testProlongPendingDocument = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    randomUpdate $ \t -> ProlongPendingDocument 2 (systemActor t)
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
    sl      <- guardJustM $ getAuthorSigLink <$> theDocument
    now     <- currentTime
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

testSaveSigAttachmentEvidenceLog :: TestEnv ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isPreparation && isSignable) `withDocumentM` do
    file <- addNewRandomFile
    let sa = defaultSignatoryAttachment
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
  addRandomDocumentWithAuthorAndCondition author
    (    isSignable
      && isPreparation
      && ((all (isSignatoryAndHasNotSigned
                && (==) StandardAuthenticationToSign .
                 signatorylinkauthenticationtosignmethod)) .
           documentsignatorylinks)
      && (((==) 2) . length . documentsignatorylinks)) `withDocumentM` do
    file <- addNewRandomFile
    sl <- (\d -> (documentsignatorylinks d) !! 1) <$> theDocument
    let sa = defaultSignatoryAttachment
    void $randomUpdate $ \t->SetSigAttachments (signatorylinkid $ sl)
                          [sa] (systemActor t)
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ \t->PreparationToPending (systemActor t) tz
    randomUpdate $ \t->SaveSigAttachment (signatorylinkid $ sl) sa file (systemActor t)

    randomUpdate $ \t->DeleteSigAttachment (signatorylinkid $ sl) sa (systemActor t)
    randomUpdate $ \t->SaveSigAttachment (signatorylinkid $ sl) sa file (systemActor t)
    randomUpdate $ \t->MarkDocumentSeen (signatorylinkid sl) (systemActor t)
    randomUpdate $ \t->SignDocument (signatorylinkid sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
    assertRaisesKontra (\SignatoryHasAlreadySigned {} -> True) $ do
      randomUpdate $ \t->DeleteSigAttachment (signatorylinkid $ sl) sa (systemActor t)

testDeleteSigAttachmentEvidenceLog :: TestEnv ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    let sa = defaultSignatoryAttachment
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
    sl <- guardJustM $ getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
    success <- randomUpdate $ \t-> ChangeSignatoryEmail (signatorylinkid sl) Nothing "email@email.com" (systemActor t)
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current ChangeSignatoryEmailEvidence) lg

testCloseDocumentEvidenceLog :: TestEnv ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending && (all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)) `withDocumentM` do
    documentsignatorylinks <$> theDocument >>= \sls -> forM_  sls $ \sl -> do
      when (isSignatory sl) $ do
        randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
        randomUpdate $ \t -> SignDocument (signatorylinkid sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
      when (isApprover sl) $ do
        randomUpdate $ \t -> MarkDocumentSeen (signatorylinkid sl) (systemActor t)
        randomUpdate $ \t -> ApproveDocument (signatorylinkid sl) (systemActor t)
    randomUpdate $ \t-> CloseDocument (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe UserGroup -> DocumentType -> String -> TestEnv (User, UTCTime, Document)
performNewDocumentWithRandomUser mug doctype title = do
  user <- maybe addNewRandomUser (\ug -> addNewRandomUserGroupUser (get ugID ug) False) mug
  ctx <- mkContext defaultLang
  let aa = authorActor ctx user
  doc <- randomUpdate $ NewDocument user title doctype defaultTimeZoneName 0 aa
  return (user, get ctxtime ctx, doc)

assertGoodNewDocument :: Maybe UserGroupWithParents -> DocumentType -> String -> (User, UTCTime, Document) -> TestEnv ()
assertGoodNewDocument mugwp doctype title (user, time, doc) = do
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
    assertEqual "link company name matches company's" companyNameFromUserGroup (getCompanyName siglink)
    assertEqual "link company number matches company's" companyNumberFromUserGroup (getCompanyNumber siglink)
    assertEqual "link company number matches company's" (getMobile user) (getMobile siglink)
    assertEqual "link signatory matches author id" (Just $ userid user) (maybesignatory siglink)
      where
        companyNameFromUserGroup = maybe "" (T.unpack . get ugName . ugwpUG) mugwp
        companyNumberFromUserGroup = maybe "" (T.unpack . get ugaCompanyNumber . ugwpAddress) mugwp

testCancelDocumentCancelsDocument :: TestEnv ()
testCancelDocumentCancelsDocument = replicateM_ 10 $ do
  user <- addNewRandomUser
  ctx <- mkContext defaultLang
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
  ctx <- mkContext defaultLang
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
  doc <- addRandomDocumentWithAuthorAndCondition author isPending
  withDocument doc $ do
    res <- randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertEqual "Document that is pending can't be archived" False res

testArchiveDocumentAuthorRight :: TestEnv ()
testArchiveDocumentAuthorRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testArchiveDocumentCompanyAdminRight :: TestEnv ()
testArchiveDocumentCompanyAdminRight = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  adminuser <- addNewRandomCompanyUser (get ugID ug) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t->ArchiveDocument (userid adminuser) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testRestoreArchivedDocumentAuthorRight :: TestEnv ()
testRestoreArchivedDocumentAuthorRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument author (systemActor t)
    assertNoArchivedSigLink =<< theDocument

testRestoreArchiveDocumentCompanyAdminRight :: TestEnv ()
testRestoreArchiveDocumentCompanyAdminRight = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  adminuser <- addNewRandomCompanyUser (get ugID ug) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument adminuser (systemActor t)

    assertNoArchivedSigLink =<< theDocument

testChangeAuthenticationToSignMethod :: TestEnv ()
testChangeAuthenticationToSignMethod = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (
      isSignable && isPending && ((<=) 2 . length . documentsignatorylinks)
      && ( all ((==) StandardAuthenticationToSign . signatorylinkauthenticationtosignmethod) . documentsignatorylinks)
      && ( all isSignatory . documentsignatorylinks)
    ) `withDocumentM` do
      sl <- guardJustM $ getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument

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

  withDocument doc $ do
    res <- randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
    assertEqual "Document that is not deleted can't be really deleted" False res

  withDocument doc $ void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
  withDocument doc $ void $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  withDocument doc $ do
    res <- randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
    assertEqual "Document can't be really deleted twice" False res

  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByDocumentID (documentid doc)]
                     [] 1
  assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)

testReallyDeleteDocumentCompanyAdmin :: TestEnv ()
testReallyDeleteDocumentCompanyAdmin = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  adminuser <- addNewRandomCompanyUser (get ugID ug) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    res <-randomUpdate $ \t->ReallyDeleteDocument (userid adminuser) (systemActor t)
    assertEqual "Document that is not deleted can't be really deleted" False res
    void $ randomUpdate $ \t->ArchiveDocument (userid adminuser) (systemActor t)
    void $ randomUpdate $ \t->ReallyDeleteDocument (userid adminuser) (systemActor t)
    assertOneArchivedSigLink =<< theDocument
    doc <- theDocument
    docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByDocumentID (documentid doc)]
                       [] 1
    assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)

testReallyDeleteDocumentSomebodyElse :: TestEnv ()
testReallyDeleteDocumentSomebodyElse = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  other <- addNewRandomCompanyUser (get ugID ug) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    res1 <- randomUpdate $ \t->ArchiveDocument (userid other) (systemActor t)
    assertEqual "ReallyDeleteDocument can be done by other user" False res1
    res2 <- randomUpdate $ \t->ReallyDeleteDocument (userid other) (systemActor t)
    assertEqual "ReallyDeleteDocument can be done by other user" False res2
    doc <- theDocument
    assertEqual "Expected no archived signatory links"
              0
              (length . filter (isJust . signatorylinkdeleted) . documentsignatorylinks $ doc)
--------------------------------------------------------------------------------


testPurgeDocument :: TestEnv ()
testPurgeDocument = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isClosed d || isCanceled d || isRejected d)
  now <- currentTime
  archived1 <- dbUpdate $ PurgeDocuments 0
  assertEqual "Purged zero documents when not deleted" 0 archived1
  withDocument doc $ void $ randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 0
  assertEqual "Purged single document" 1 archived2

  allDocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                         [DocumentFilterByDocumentID (documentid doc)] [] (-1)
  assertEqual "List documents does not include purged ones" [] (map documentid allDocs1)

testPurgeDocumentUserSaved :: TestEnv ()
testPurgeDocumentUserSaved = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isClosed d || isCanceled d || isRejected d)
  archived1 <- dbUpdate $ PurgeDocuments 1
  now <- currentTime
  withDocument doc $ void $ randomUpdate $ \t->ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 1
  assertEqual "Purged zero documents before delete" 0 archived1
  assertEqual "Purged zero documents before time passed after delete" 0 archived2

testPurgeDocumentRemovesSensitiveData :: TestEnv ()
testPurgeDocumentRemovesSensitiveData = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  now <- currentTime
  withDocument doc $ void $ randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  void $ dbUpdate $ PurgeDocuments 0
  let sidsSql = "SELECT id FROM signatory_links WHERE document_id = " <?> documentid doc

  runSQL_ $ "SELECT count(*) FROM author_attachments " <+>
    "WHERE document_id =" <?> documentid doc
  aac :: Int64 <- fetchOne runIdentity
  assertEqual "All author attachments are removed" 0 aac

  runSQL_ $ "SELECT count(*) FROM document_tags " <+>
    "WHERE document_id =" <?> documentid doc
  dtc :: Int64 <- fetchOne runIdentity
  assertEqual "All document tags are removed" 0 dtc

  runSQL_ $ "SELECT count(*) FROM evidence_log " <+>
    "WHERE document_id =" <?> documentid doc
  delc :: Int64 <- fetchOne runIdentity
  assertEqual "All document evidence log is removed" 0 delc

  runSQL_ $ "SELECT count(*) FROM signatory_attachments" <+>
    "WHERE signatory_link_id IN (" <+> sidsSql <+> ")"
  sac :: Int64 <- fetchOne runIdentity
  assertEqual "All signatory attachments are removed" 0 sac

  runSQL_ $ "SELECT count(*) FROM signatory_link_fields " <+>
    "WHERE (value_text IS NOT NULL AND value_text != '') AND " <+>
    "signatory_link_id IN (" <+> sidsSql <+> ")"
  stc :: Int64 <- fetchOne runIdentity
  assertEqual "All signatory fields have empty texts" 0 stc

  runSQL_ $ "SELECT count(*) FROM signatory_link_fields " <+>
    "WHERE (value_file_id IS NOT NULL) AND " <+>
    "signatory_link_id IN (" <+> sidsSql <+> ")"
  sfc :: Int64 <- fetchOne runIdentity
  assertEqual "All signatory fields don't point to files" 0 sfc

  runSQL_ $ "SELECT count(*) FROM documents " <+>
    "WHERE id =" <?> documentid doc <+>
    "AND title='' AND api_v1_callback_url IS NULL " <+>
    "AND api_v2_callback_url IS NULL AND invite_text =''" <+>
    "AND confirm_text='' AND invite_ip=0"
  dc :: Int64 <- fetchOne runIdentity
  assertEqual "The is one document with given id and removed sensitive data" 1 dc

  runSQL_ $ "SELECT count(*) FROM signatory_links " <+>
    "WHERE document_id=" <?> documentid doc <+>
    "AND csv_contents IS NULL " <+>
    "AND sign_redirect_url IS NULL AND rejection_reason IS NULL " <+>
    "AND reject_redirect_url IS NULL AND consent_title IS NULL " <+>
    "AND seen_ip=0 AND sign_ip=0"
  sc :: Int64 <- fetchOne runIdentity
  assertEqual "No signatory has sensitive data" (length $ documentsignatorylinks doc) (fromIntegral sc)

testPurgeDocumentSharedTemplates :: TestEnv ()
testPurgeDocumentSharedTemplates = do
  ug <- addNewUserGroup
  bob <- addNewRandomCompanyUser (get ugID ug) True
  alice <- addNewRandomCompanyUser (get ugID ug) False

  doc <- addRandomDocumentWithAuthorAndCondition bob (\d -> isPreparation d || isDocumentShared d)
  void $ dbUpdate $ DeleteUser $ userid alice
  void $ dbUpdate $ PurgeDocuments 0

  eDoc <- try $ dbQuery $ GetDocumentByDocumentID $ documentid doc
  case eDoc of
    Left (_ :: SomeDBExtraException) ->
      assertFailure "Shared template should not be purged"
    Right Document{ documentsignatorylinks
                      = SignatoryLink{ maybesignatory = Just uid } : _ } ->
      assertEqual "Shared template should be given to oldest admin"
                  uid (userid bob)
    _ -> assertFailure "Unexpected error"

testPurgeDocumentImmediateTrash :: TestEnv ()
testPurgeDocumentImmediateTrash = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  now <- currentTime
  withDocument doc $ void $ randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })

  do
    archived <- dbUpdate $ PurgeDocuments 1 -- purge after 1 day
    assertEqual "Purged zero documents before 1 day after deletion" 0 archived

    doc' <- randomQuery $ GetDocumentByDocumentID $ documentid doc
    forM_ (documentsignatorylinks doc') $ \sl -> do
      assertBool "Should \"really delete\" deleted documents only" $ not $
        isNothing (signatorylinkdeleted sl)
        && isJust (signatorylinkreallydeleted sl)

  dbUpdate . UserGroupUpdate . fromJust $
    FCP.set (drpImmediateTrash . ugsDataRetentionPolicy . FCP.just . ugSettings) True ug

  do
    archived <- dbUpdate $ PurgeDocuments 1
    assertEqual "Purged single document" 1 archived

  allDocs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                         [DocumentFilterByDocumentID (documentid doc)] [] (-1)
  assertEqual "List documents does not include purged ones" [] (map documentid allDocs1)

testArchiveIdleDocument :: TestEnv ()
testArchiveIdleDocument = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomUserGroupUser (get ugID ug) False
  author2 <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author $ \d ->
           isSignable d && not (isPending d)
  void $ addRandomDocumentWithAuthorAndCondition author (isTemplate || isPending)
  void $ addRandomDocumentWithAuthorAndCondition author2 $ \d ->
         documentstatus d == documentstatus doc

  let oldUGS = fromJust $ get ugSettings ug
      newUGS = set (drpIdleDocTimeout (documentstatus doc) . ugsDataRetentionPolicy) (Just 1) $ oldUGS
  void $ dbUpdate . UserGroupUpdate . set ugSettings (Just newUGS) $ ug

  archived1 <- archiveIdleDocuments (documentmtime doc)
  assertEqual "Archived zero idle documents (too early)" 0 archived1
  archived2 <- archiveIdleDocuments (2 `daysAfter` documentmtime doc)
  assertEqual "Archived idle documents for one signatory" 1 archived2

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: TestEnv ()
testArchiveDocumentUnrelatedUserLeft = replicateM_ 10 $ do
  author        <- addNewRandomUser
  unrelateduser <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    res <- randomUpdate $ \t -> ArchiveDocument (userid unrelateduser) (systemActor t)
    assertEqual "ArchiveDocument can be done by unrelated user" False res

testArchiveDocumentCompanyStandardLeft :: TestEnv ()
testArchiveDocumentCompanyStandardLeft = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  standarduser <- addNewRandomCompanyUser (get ugID ug) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    res <- randomUpdate $ \t->ArchiveDocument (userid standarduser) (systemActor t)
    assertEqual "ArchiveDocument can be done by user that is not company admin" False res

testRestoreArchivedDocumentUnrelatedUserLeft :: TestEnv ()
testRestoreArchivedDocumentUnrelatedUserLeft = replicateM_ 10 $ do
  author        <- addNewRandomUser
  unrelateduser <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t -> ArchiveDocument (userid author) (systemActor t)
    assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True)$ do
      randomUpdate $ \t->RestoreArchivedDocument unrelateduser (systemActor t)

testRestoreArchiveDocumentCompanyStandardLeft :: TestEnv ()
testRestoreArchiveDocumentCompanyStandardLeft = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) False
  standarduser <- addNewRandomCompanyUser (get ugID ug) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $ do
      randomUpdate $ \t->RestoreArchivedDocument standarduser (systemActor t)

testGetDocumentsByAuthorNoArchivedDocs :: TestEnv ()
testGetDocumentsByAuthorNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsByAuthor . userid)

checkQueryDoesntContainArchivedDocs :: DBQuery (DocumentT TestEnv) q [Document] => (User -> q) -> TestEnv ()
checkQueryDoesntContainArchivedDocs qry = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomCompanyUser (get ugID ug) True
  addRandomDocumentWithAuthorAndCondition author (\d -> (isPreparation d || isClosed d) && (isSignable d)) `withDocumentM` do
    did <- theDocumentID
    docsbeforearchive <- dbQuery (qry author)
    assertEqual "Expecting one doc before archive" [did] (map documentid docsbeforearchive)
    void $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
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
  ctx <- mkContext defaultLang
  let aa = authorActor ctx author
  doc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype defaultTimeZoneName 0 aa)
  -- assert
  assertInvariants doc

testDocumentCanBeCreatedAndFetchedByID :: TestEnv ()
testDocumentCanBeCreatedAndFetchedByID = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
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
  author      <- addNewRandomUser
  ctx         <- mkContext defaultLang
  screenshots <- getScreenshots
  addRandomDocument
    ((randomDocumentAllowsDefault author)
      { randomDocumentAllowedStatuses = [Pending]
      , randomDocumentAllowedTypes    = [Signable]
      }) `withDocumentM` do
    void $ addNewRandomFile
    atts  <- replicateM 2 $ do
             file <- addNewRandomFile

             let att = defaultSignatoryAttachment
                   { signatoryattachmentfile     = Just file
                   , signatoryattachmentfilename = Just "afile.ran"
                   , signatoryattachmentname     = show file
                   }
             return att
    (time, sl) <- rand 10 arbitrary
    sa  <- signatoryActor (set ctxtime time ctx) sl
    sls <- documentsignatorylinks <$> theDocument
    dbUpdate $ SetSigAttachments (signatorylinkid $ sls !! 0) atts sa

    forM_ sls $ \slk -> do
      when (isSignatory slk) $ do
        case signatorylinkauthenticationtosignmethod slk of
          SEBankIDAuthenticationToSign -> do
             randomUpdate $ \esig ->
               SignDocument (signatorylinkid slk)
               (Just (CGISEBankIDSignature_ esig)) Nothing screenshots sa
          StandardAuthenticationToSign -> do
             dbUpdate $
               SignDocument (signatorylinkid slk)
               Nothing Nothing screenshots sa
          SMSPinAuthenticationToSign -> do
             dbUpdate $
               SignDocument (signatorylinkid slk)
               Nothing (Just "Arbitrary PIN") screenshots sa
          NOBankIDAuthenticationToSign -> do
             randomUpdate $ \esig ->
               SignDocument (signatorylinkid slk)
               (Just (NetsNOBankIDSignature_ esig)) Nothing screenshots sa
          DKNemIDAuthenticationToSign -> do
             randomUpdate $ \esig ->
               SignDocument (signatorylinkid slk)
               (Just (NetsDKNemIDSignature_ esig)) Nothing screenshots sa
      when (isApprover slk) $ do
        dbUpdate $
          ApproveDocument (signatorylinkid slk) sa

    randomUpdate $ \t -> CloseDocument (systemActor t)

    runPdfToolsLambdaT (get ctxpdftoolslambdaenv ctx) $
      sealDocument "https://scrive.com"

testDocumentAppendSealedPendingRight :: TestEnv ()
testDocumentAppendSealedPendingRight = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocument
    ((randomDocumentAllowsDefault author)
     { randomDocumentAllowedStatuses = [Closed] }) `withDocumentM` do
    file <- addNewRandomFile

    --execute

    now <- currentTime
    success <- randomUpdate $
      AppendExtendedSealedFile file Missing $ systemActor now

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
    success <- dbUpdate $ ResetSignatoryDetails
      [defaultSignatoryLink { signatoryfields = sf}]
      (systemActor mt)
    --assert
    assert $ not success

testPreparationResetSignatoryDetailsAlwaysRight :: TestEnv ()
testPreparationResetSignatoryDetailsAlwaysRight = replicateM_ 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    mt <- rand 10 arbitrary
    --execute
    success <- dbUpdate $ ResetSignatoryDetails
      [defaultSignatoryLink {
            signatoryisauthor = True
          , maybesignatory = Just $ userid author
      }]
      (systemActor mt)
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
    let newData1 = defaultSignatoryLink {
            signatoryisauthor = True
          , maybesignatory = Just $ userid author
          }
    success1 <- dbUpdate $ ResetSignatoryDetails [newData1] (systemActor mt)
    assert success1
    assertInvariants =<< theDocument
    sls <- documentsignatorylinks <$> theDocument
    assertEqual "Proper delivery method set" [EmailDelivery] (map signatorylinkdeliverymethod sls)
    assertEqual "Proper authentication method set" [StandardAuthenticationToSign] (map signatorylinkauthenticationtosignmethod sls)

    let newData2 =  defaultSignatoryLink {
              signatoryisauthor = True
            , maybesignatory = Just $ userid author
            , signatorylinkdeliverymethod = PadDelivery
            , signatorylinkauthenticationtosignmethod =
                SEBankIDAuthenticationToSign
            }
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
    withDocumentID a $ dbUpdate $ ResetSignatoryDetails
      [defaultSignatoryLink { signatoryisauthor = True } ]
      (systemActor mt)

testGetDocumentsSharedInCompany :: TestEnv ()
testGetDocumentsSharedInCompany = replicateM_ 10 $ do
  -- two companies, two users per company, two users outside of company
  -- each having a document here
  ug1 <- addNewUserGroup
  ug2 <- addNewUserGroup
  user1' <- addNewRandomUser
  user2' <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid user1') (get ugID ug1)
  Just user1 <- dbQuery $ GetUserByID (userid user1')
  void $ dbUpdate $ SetUserUserGroup (userid user2') (get ugID ug1)
  Just user2 <- dbQuery $ GetUserByID (userid user2')
  user3' <- addNewRandomUser
  user4' <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid user3') (get ugID ug2)
  Just user3 <- dbQuery $ GetUserByID (userid user3')
  void $ dbUpdate $ SetUserUserGroup (userid user4') (get ugID ug2)
  Just user4 <- dbQuery $ GetUserByID (userid user4')
  user5 <- addNewRandomUser
  user6 <- addNewRandomUser

  -- This test is good only for not admins
  void $ dbUpdate $ SetUserCompanyAdmin (userid user1) False
  void $ dbUpdate $ SetUserCompanyAdmin (userid user2) False
  void $ dbUpdate $ SetUserCompanyAdmin (userid user3) False
  void $ dbUpdate $ SetUserCompanyAdmin (userid user4) False
  void $ dbUpdate $ SetUserCompanyAdmin (userid user5) False
  void $ dbUpdate $ SetUserCompanyAdmin (userid user6) False

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

  void $ dbUpdate $ SetDocumentSharing [docid4] False
  void $ dbUpdate $ SetDocumentSharing [docid1, docid2, docid3, docid5, docid6] True

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
  let stripDocumentFilter (DocumentFilterByTSQuery s) = s
      stripDocumentFilter _ = unexpectedError "This should not happen!"
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

  assertEqual "Should match" e0 (stripDocumentFilter $ processSearchStringToFilter t0)
  assertEqual "Should match" e1 (stripDocumentFilter $ processSearchStringToFilter t1)
  assertEqual "Should match" e2 (stripDocumentFilter $ processSearchStringToFilter t2)
  assertEqual "Should match" e3 (stripDocumentFilter $ processSearchStringToFilter t3)
  assertEqual "Should match" e4 (stripDocumentFilter $ processSearchStringToFilter t4)
  assertEqual "Should match" e5 (stripDocumentFilter $ processSearchStringToFilter t5)
  assertEqual "Should match" e6 (stripDocumentFilter $ processSearchStringToFilter t6)
  assertEqual "Should match" e7 (stripDocumentFilter $ processSearchStringToFilter t7)
  assertEqual "Should match" e8 (stripDocumentFilter $ processSearchStringToFilter t8)
  assertEqual "Should match" e9 (stripDocumentFilter $ processSearchStringToFilter t9)
  assertEqual "Should match" eA (stripDocumentFilter $ processSearchStringToFilter tA)
  assertEqual "Should match" eB (stripDocumentFilter $ processSearchStringToFilter tB)
  assertEqual "Should match" eC (stripDocumentFilter $ processSearchStringToFilter tC)
  assertEqual "Should match" eD (stripDocumentFilter $ processSearchStringToFilter tD)
  assertEqual "Should match" eE (stripDocumentFilter $ processSearchStringToFilter tE)
  assertEqual "Should match" eF (stripDocumentFilter $ processSearchStringToFilter tF)
  assertEqual "Should match" eG (stripDocumentFilter $ processSearchStringToFilter tG)
  assertEqual "Should match" eH (stripDocumentFilter $ processSearchStringToFilter tH)
  assertEqual "Should match" eI (stripDocumentFilter $ processSearchStringToFilter tI)
  assertEqual "Should match" eJ (stripDocumentFilter $ processSearchStringToFilter tJ)

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

  let matchTitleFilter1 = [processSearchStringToFilter "Magic Unique Title 42"]
  matchTitleFilter1Matches <- dbQuery $ GetDocuments domain matchTitleFilter1 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter1) 1 (length matchTitleFilter1Matches)

  let matchTitleFilter2 = [processSearchStringToFilter "\"Magic Unique Title 42\""]
  matchTitleFilter2Matches <- dbQuery $ GetDocuments domain matchTitleFilter2 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter2) 1 (length matchTitleFilter2Matches)

  let matchTitleFilter3 = [processSearchStringToFilter "\"Unique Title 42\""]
  matchTitleFilter3Matches <- dbQuery $ GetDocuments domain matchTitleFilter3 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter3) 1 (length matchTitleFilter3Matches)

  let matchTitleFilter5 = [processSearchStringToFilter "Magic \"Unique Title\" 42"]
  matchTitleFilter5Matches <- dbQuery $ GetDocuments domain matchTitleFilter5 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter5) 1 (length matchTitleFilter5Matches)

  let matchTitleFilter6 = [processSearchStringToFilter "42 Unique \"Title\" Magic"]
  matchTitleFilter6Matches <- dbQuery $ GetDocuments domain matchTitleFilter6 [] maxBound
  assertEqual ("With filter " ++ show matchTitleFilter6) 1 (length matchTitleFilter6Matches)

  let notMatchTitleFilter1 = [processSearchStringToFilter "\"Magic Unique Title 4*\""]
  notMatchTitleFilter1Matches <- dbQuery $ GetDocuments domain notMatchTitleFilter1 [] maxBound
  assertEqual ("With filter " ++ show notMatchTitleFilter1) 1 (length notMatchTitleFilter1Matches)

  let matchFirstNameFilter = [processSearchStringToFilter "Bob Blue"]
  matchFirstNameFilterMatches <- dbQuery $ GetDocuments domain matchFirstNameFilter [] maxBound
  assertEqual ("With filter " ++ show matchFirstNameFilter) 3 (length matchFirstNameFilterMatches)

  let matchEmailFilter1 = [processSearchStringToFilter "bill@*"]
  matchEmailFilter1Matches <- dbQuery $ GetDocuments domain matchEmailFilter1 [] maxBound
  assertEqual ("With filter " ++ show matchEmailFilter1) 3 (length matchEmailFilter1Matches)

  let matchEmailFilter2 = [processSearchStringToFilter "\"bill@zonk.com\""]
  matchEmailFilter2Matches <- dbQuery $ GetDocuments domain matchEmailFilter2 [] maxBound
  assertEqual ("With filter " ++ show matchEmailFilter2) 3 (length matchEmailFilter2Matches)

  let notMatchEmailFilter2 = [processSearchStringToFilter "herm@*"]
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
           void $ withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  newuser <- addNewRandomUser

  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor (Just newuser) doc (systemActor mt) id)
  void $ withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (documentid doc) (systemActor mt)

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
  ug <- addNewUserGroup
  void $ dbUpdate $ SetUserUserGroup (userid user) (get ugID ug)
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (\doc -> isPreparation doc)
  tmpdoc <- dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           void $ withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor (Just user') doc (systemActor mt) id)
  void $ withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (documentid doc) (systemActor mt)
  doc' <- dbQuery $ GetDocumentByDocumentID docid'
  let [author] = filter isAuthor $ documentsignatorylinks doc'
  assertEqual "Author signatory link user group name is not same as his user group" (get ugName ug) (T.pack $ getCompanyName author)

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
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file1 <- addNewRandomFile
    file2 <- addNewRandomFile
    --execute
    let att1 = defaultSignatoryAttachment
          { signatoryattachmentfile = Just file1
          , signatoryattachmentfilename = Just "afile1.ran"
          , signatoryattachmentname = "att1"
          , signatoryattachmentdescription = "att1 description"
          }
    let att2 = defaultSignatoryAttachment
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

hasConsentModule :: SignatoryLinkIdentity a => a -> Document -> Bool
hasConsentModule i doc = case getSigLinkFor i doc of
  Nothing -> False
  Just sl -> not $ null $ signatorylinkconsentquestions sl

testUpdateConsentResponsesForSigningSuccess :: TestEnv ()
testUpdateConsentResponsesForSigningSuccess = do
  author <- addNewRandomUser
  doc    <- addRandomDocumentWithAuthorAndCondition author
              (isSignable && isPending && hasConsentModule author)

  withDocument doc $ do
    sl <- guardJustM $ getSigLinkFor author <$> theDocument
    let questions = signatorylinkconsentquestions sl
    responses <- mapM (\q -> (scqID q,) <$> rand 10 arbitrary) questions

    randomUpdate $ \t -> UpdateConsentResponsesForSigning sl
                           (SignatoryConsentResponsesForSigning responses)
                           (systemActor t)

    sl' <- guardJustM $  getSigLinkFor author <$> theDocument
    let questions' = signatorylinkconsentquestions sl'
        responses' = map (\q -> (scqID q, scqResponse q)) questions'
    assertBool "Some responses are missing or wrong"
               (all id (zipWith (\(i,r) (i',r') -> i == i' && Just r == r')
                                responses responses'))

    events <- query $ GetEvidenceLog $ documentid doc
    let check scq e = case scqDescription scq of
          Nothing -> evType e == Current ConsentQuestionAnswered
          Just _  -> evType e == Current ConsentQuestionAnsweredWithDescription
    assertBool "There are some missing evidence logs"
               (all (\q -> any (check q) events) questions)

testUpdateConsentResponsesForSigningWrongQuestionID :: TestEnv ()
testUpdateConsentResponsesForSigningWrongQuestionID = do
  author <- addNewRandomUser
  doc    <- addRandomDocumentWithAuthorAndCondition author
              (isSignable && isPending && hasConsentModule author)

  withDocument doc $ do
    sl <- guardJustM $ getSigLinkFor author <$> theDocument
    let questions = signatorylinkconsentquestions sl
        goodResponses = map (\q -> (scqID q, True)) questions
        badResponse =
          ( unsafeSignatoryConsentQuestionID
              (fromSignatoryConsentQuestionID (scqID (head questions)) + 1000)
          , True )

    assertRaisesKontra (\(SignatoryConsentQuestionDoesNotExist _) -> True) $
      randomUpdate $ \t -> UpdateConsentResponsesForSigning sl
        (SignatoryConsentResponsesForSigning (badResponse : goodResponses))
        (systemActor t)

testSignDocumentNonSignableLeft :: TestEnv ()
testSignDocumentNonSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    sl <- guardJustM $ getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignableNotPendingLeft :: TestEnv ()
testSignDocumentSignableNotPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable && (not . isPending)) `withDocumentM` do
    sl <- guardJustM $ getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignablePendingRight :: TestEnv ()
testSignDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author
    (   isSignable
     && isPending
     && any ((== StandardAuthenticationToSign) .
             signatorylinkauthenticationtosignmethod
             && isSignatoryAndHasNotSigned) .
      documentsignatorylinks
    ) `withDocumentM` do
    sl <- guardJustM $ find (   (== StandardAuthenticationToSign) .
                                    signatorylinkauthenticationtosignmethod
                             && isSignatoryAndHasNotSigned ) .
                       documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary

    randomUpdate $
      MarkDocumentSeen (signatorylinkid sl)
      (systemActor time)

    randomUpdate $ SignDocument (signatorylinkid sl)
      Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots
      (systemActor time)

testSignDocumentSignablePendingSEBankIDRight :: TestEnv ()
testSignDocumentSignablePendingSEBankIDRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author
    (   isSignable
     && isPending
     && any (   (== SEBankIDAuthenticationToSign) .
                signatorylinkauthenticationtosignmethod
             && isSignatoryAndHasNotSigned ) .
      documentsignatorylinks
    ) `withDocumentM` do

    sl <- guardJustM $ find (   (== SEBankIDAuthenticationToSign) .
                                    signatorylinkauthenticationtosignmethod
                             && isSignatoryAndHasNotSigned ) .
                       documentsignatorylinks <$> theDocument
    time    <- rand 10 arbitrary

    randomUpdate $ MarkDocumentSeen (signatorylinkid sl)
      (systemActor time)

    randomUpdate $ \esig -> SignDocument (signatorylinkid sl)
      (Just (CGISEBankIDSignature_ esig)) Nothing
      SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentSignablePendingNOBankIDRight :: TestEnv ()
testSignDocumentSignablePendingNOBankIDRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author
    (    isSignable
      && isPending
      && any (   (== NOBankIDAuthenticationToSign) .
                 signatorylinkauthenticationtosignmethod
              && isSignatoryAndHasNotSigned ) .
      documentsignatorylinks
    ) `withDocumentM` do
    sl <- guardJustM $ find (   (== NOBankIDAuthenticationToSign) .
                                     signatorylinkauthenticationtosignmethod
                             && isSignatoryAndHasNotSigned ) .
                       documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary

    randomUpdate $ MarkDocumentSeen (signatorylinkid sl) (systemActor time)
    randomUpdate $ \esig -> SignDocument (signatorylinkid sl)
      (Just (NetsNOBankIDSignature_ esig))
      Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentSignablePendingDKNemIDRight :: TestEnv ()
testSignDocumentSignablePendingDKNemIDRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author
    (    isSignable
      && isPending
      && any (   (== DKNemIDAuthenticationToSign) .
                 signatorylinkauthenticationtosignmethod
              && isSignatoryAndHasNotSigned ) .
      documentsignatorylinks
    ) `withDocumentM` do
    sl <- guardJustM $ find (   (== DKNemIDAuthenticationToSign) .
                                    signatorylinkauthenticationtosignmethod
                             && isSignatoryAndHasNotSigned ) .
                       documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary
    randomUpdate $ MarkDocumentSeen (signatorylinkid sl)
      (systemActor time)
    randomUpdate $ \esig -> SignDocument (signatorylinkid sl)
      (Just (NetsDKNemIDSignature_ esig))
      Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentNotLeft :: TestEnv ()
testSignDocumentNotLeft = replicateM_ 10 $ do
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    -- our machinery is broken here, baseline condition has only relations
    -- this should be ignored and properly return info about non existing document
    d <- rand 10 arbitrary
    withDocument d $ randomUpdate $ \sl si t -> SignDocument sl si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

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
         , randomDocumentChecker = onCondition $ (any isSignatory . documentsignatorylinks) &&
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
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    sl   <- guardJustM $ getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) (isApprover sl) Nothing
             (authorActor (set ctxtime time ctx) author)

testRejectDocumentSignableNotPendingLeft :: TestEnv ()
testRejectDocumentSignableNotPendingLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author (isSignable && not . isPending) `withDocumentM` do
    sl   <- guardJustM $ getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) (isApprover sl) Nothing
             (authorActor (set ctxtime time ctx) author)

testRejectDocumentNotLeft :: TestEnv ()
testRejectDocumentNotLeft = replicateM_ 10 $ do
  void $ addRandomDocument . randomDocumentAllowsDefault =<< addNewRandomUser
  ctx <- mkContext defaultLang
  (did, time, sl) <- rand 10 arbitrary
  let sa = signatoryActor (set ctxtime time ctx) sl
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID did $ randomUpdate .
      RejectDocument (signatorylinkid sl) (isApprover sl)  Nothing =<< sa

testRejectDocumentSignablePendingRight :: TestEnv ()
testRejectDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author (isSignable && isPending) `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . filter isSignatory . documentsignatorylinks =<< theDocument
    sl   <- guardJustM $ getSigLinkFor slid <$> theDocument
    time <- rand 10 arbitrary
    let sa = signatoryActor (set ctxtime time ctx) sl
    randomUpdate . RejectDocument slid (isApprover sl) Nothing =<< sa

    assertInvariants =<< theDocument

  -- Also test that an approver can reject the document.
  addRandomDocumentWithAuthorAndCondition author
    (isSignable && isPending
     && any isApprover . documentsignatorylinks)
    `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . filter isApprover . documentsignatorylinks =<< theDocument
    sl   <- guardJustM $ getSigLinkFor slid <$> theDocument
    time <- rand 10 arbitrary
    let sa = signatoryActor (set ctxtime time ctx) sl
    randomUpdate . RejectDocument slid (isApprover sl) Nothing =<< sa

    assertInvariants =<< theDocument

testApproveDocumentSignablePendingRight :: TestEnv ()
testApproveDocumentSignablePendingRight = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx    <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author
    (isSignable && isPending
    && any isApprover . documentsignatorylinks)
    `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid .
            filter isApprover . documentsignatorylinks =<< theDocument
    sl   <- guardJustM $ getSigLinkFor slid <$> theDocument
    time <- rand 10 arbitrary
    let sa = signatoryActor (set ctxtime time ctx) sl
    randomUpdate . ApproveDocument slid =<< sa

    assertInvariants =<< theDocument

testMarkInvitationRead :: TestEnv ()
testMarkInvitationRead = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author
         (isPending && (all (isNothing . maybereadinvite) . documentsignatorylinks)) `withDocumentM` do

    sl' <- rand 10 . elements . documentsignatorylinks =<< theDocument
    let slid = signatorylinkid sl'
    time <- currentTime
    success <- dbUpdate . MarkInvitationRead slid
               =<< signatoryActor (set ctxtime time ctx) sl'

    assert success
    sl <- guardJustM $ getSigLinkFor slid <$> theDocument
    assertEqual "Invitation read time should be set." (Just True) (liftA2 compareTime (Just time) (maybereadinvite sl))

testMarkInvitationReadDocDoesntExist :: TestEnv ()
testMarkInvitationReadDocDoesntExist = replicateM_ 10 $ do
  ctx <- mkContext defaultLang
  (did, sl, time) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist{} -> True) $ do
    void $ withDocumentID did $ randomUpdate . MarkInvitationRead (signatorylinkid sl)
            =<< signatoryActor (set ctxtime time ctx) sl
    return ()
  return ()

testMarkDocumentSeenNotSignableLeft :: TestEnv ()
testMarkDocumentSeenNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         } `withDocumentM` do

    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor (set ctxtime time ctx) sl
        assertRaisesKontra (\DocumentTypeShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) =<< sa

testMarkDocumentSeenClosedOrPreparationLeft :: TestEnv ()
testMarkDocumentSeenClosedOrPreparationLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultLang
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         } `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor (set ctxtime time ctx) sl
        assertRaisesKontra (\DocumentStatusShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) =<< sa

testMarkDocumentSeenNotLeft :: TestEnv ()
testMarkDocumentSeenNotLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  _doc <- addRandomDocument (randomDocumentAllowsDefault author)
  (d, s) <- rand 10 arbitrary
  a <- arbitrarySignatoryActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID d $ randomUpdate $ MarkDocumentSeen s a

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
  ctx <- mkContext defaultLang
  addRandomDocumentWithAuthorAndCondition author (isSignable && (not . (isClosed || isPreparation))) `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
                unless (hasSeen sl) $ do
                  time <- rand 10 arbitrary
                  let sa = signatoryActor (set ctxtime time ctx) sl
                  randomUpdate . MarkDocumentSeen (signatorylinkid sl) =<< sa
                  tsl <- guardJustM $ getSigLinkFor (signatorylinkid sl) <$>
                         theDocument
                  assertBool "Signatorylink should be marked seen now." (hasSeen tsl)

testSetInvitationDeliveryStatusNotSignableLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotSignableLeft = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    actor <- arbitrarySystemActor
    sl    <- guardJustM $ getAuthorSigLink <$> theDocument
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
  ctx <- mkContext defaultLang
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
         , randomDocumentChecker = onCondition $
           (\doc -> length (documentsignatorylinks doc) > 1) &&
           (not . all (isSignatory --> isSignatoryAndHasSigned) .
            documentsignatorylinks)
         } `withDocumentM` do
    sa <- arbitrarySystemActor
    assertRaisesKontra (\(SigningPartyHasNotYetSignedOrApproved {}) -> True) $ do
      randomUpdate $ CloseDocument sa

testCloseDocumentNotSignableNothing :: TestEnv ()
testCloseDocumentNotSignableNothing = replicateM_ 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentChecker = onCondition $
             not . all (isSignatory --> isSignatoryAndHasSigned) .
             documentsignatorylinks
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
  ctx <- mkContext defaultLang
  time <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentChecker = onCondition $
             not . all (isSignatory --> isSignatoryAndHasSigned) .
             documentsignatorylinks
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
         { randomDocumentChecker = onCondition (not . isClosed)
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
         { randomDocumentChecker = onCondition $ not . isClosed
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
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowHeader targetValue actor
    newValue <- documentshowheader <$> theDocument
    assertEqual "SetShowHeader changes value to target value" targetValue newValue
    assertEqual "SetShowHeader return success" success True

testSetShowPDFDownload :: TestEnv ()
testSetShowPDFDownload = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowPDFDownload targetValue actor
    newValue <- documentshowpdfdownload <$> theDocument
    assertEqual "SetShowPDFDownload changes value to target value" targetValue newValue
    assertEqual "SetShowPDFDownload return success" success True

testSetShowRejectOption :: TestEnv ()
testSetShowRejectOption = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowRejectOption targetValue actor
    newValue <- documentshowrejectoption <$> theDocument
    assertEqual "SetShowRejectOption changes value to target value" targetValue newValue
    assertEqual "SetShowRejectOption return success" success True

testSetAllowRejectReason :: TestEnv ()
testSetAllowRejectReason = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetAllowRejectReason targetValue actor
    newValue <- documentallowrejectreason <$> theDocument
    assertEqual "SetAllowRejectReason changes value to target value" targetValue newValue
    assertEqual "SetAllowRejectReason return success" success True

testSetShowFooter :: TestEnv ()
testSetShowFooter = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowFooter targetValue actor
    newValue <- documentshowfooter <$> theDocument
    assertEqual "SetShowFooter changes value to target value" targetValue newValue
    assertEqual "SetShowFooter return success" success True

testSetShowArrow :: TestEnv ()
testSetShowArrow = replicateM_ 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  targetValue <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)  { randomDocumentChecker = onCondition isPreparation } `withDocumentM` do
    success <- randomUpdate $ SetShowArrow targetValue actor
    newValue <- documentshowarrow <$> theDocument
    assertEqual "SetShowArrow changes value to target value" targetValue newValue
    assertEqual "SetShowArrow return success" success True

assertInvariants :: (MonadIO m, MonadTime m) => Document -> m ()
assertInvariants document = do
  now <- currentTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a

testGetDocumentsByCompanyWithFilteringCompany :: TestEnv ()
testGetDocumentsByCompanyWithFilteringCompany = replicateM_ 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  ug <- addNewUserGroup
  author <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid author) (get ugID ug)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  withDocumentID did $ do
    time <- currentTime
    let actor = systemActor time
    void $ dbUpdate $ SetDocumentTags (S.singleton $ DocumentTag name value) actor
    docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound

    assertEqual "Should have 1 document returned" 1 (length docs')


testGetDocumentsByCompanyWithFilteringFilters :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFilters = replicateM_ 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  ug <- addNewUserGroup
  author <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid author) (get ugID ug)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  docs <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [DocumentFilterByTags [DocumentTag name value]] [] maxBound
  docs' <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author) [] [] maxBound

  assertBool "Should have no documents returned" (null docs)
  assertEqual "Should have 1 document returned" [did] (map documentid docs')

testSetDocumentUnsavedDraft :: TestEnv ()
testSetDocumentUnsavedDraft = replicateM_ 10 $ do
  ug <- addNewUserGroup
  author <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid author) (get ugID ug)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  withDocumentID did $ do
    isdraft <- (isSignable && isPreparation) <$> theDocument

    docs1 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] maxBound
    void $ dbUpdate $ SetDocumentUnsavedDraft True
    docs2 <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ userid author)
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] maxBound
    void $ dbUpdate $ SetDocumentUnsavedDraft False
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
  ug <- addNewUserGroup
  author <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup (userid author) (get ugID ug)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- currentTime
  let actor = systemActor time
  void $ withDocumentID did $ dbUpdate $ SetDocumentTags (S.singleton $ DocumentTag name value) actor
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
    ug <- addNewUserGroup
    author <- addNewRandomUser
    time <- currentTime
    let actor = systemActor time
    void $ dbUpdate $ SetUserUserGroup (userid author) (get ugID ug)
    Just author' <- dbQuery $ GetUserByID (userid author)
    did <- addRandomDocumentWithAuthor author'

    void $ withDocumentID did $ dbUpdate $ SetDocumentTags (S.fromList [DocumentTag name1 value1, DocumentTag name2 value2]) actor
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
    , signatoryrole a == signatoryrole b
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
