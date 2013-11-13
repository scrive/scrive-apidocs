module DocStateTest (docStateTests) where

import qualified Amazon as AWS
import AppConf (AppConf(dbConfig))
import Configuration (confDefault)
import Context (ctxtime)
import Control.Arrow (first)
import Control.Concurrent (newMVar)
import Control.Logic
import qualified CronEnv
import qualified Data.ByteString as BS
import DB
import qualified MemCache
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import Doc.ExtendSignature (sealMissingSignaturesNewerThan, extendSignatures)
import Templates (getTemplatesModTime, readGlobalTemplates)
import ActionQueue.Monad (ActionQueueT)
import ActionQueue.Scheduler (SchedulerData(..))
import Doc.SealStatus (SealStatus(..))
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Utils.Default
import TestingUtil
import TestKontra
import DB.SQL2
import Company.Model
import Doc.TestInvariants
import MinutesTime
import Test.HUnit.Base (Assertion)
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo

import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import File.FileID
import Doc.Conditions
import qualified Data.Set as S
import Util.Actor
import EvidenceLog.Model

docStateTests :: TestEnvSt -> Test
docStateTests env = testGroup "DocState" [
  dataStructureProperties,
  testThat "Document with seal status Missing gets sealed" env testSealMissingSignatures,
  testThat "Document with extensible seal can be extended" env testExtendSignatures,
  testThat "Document with extensible but broken seal seal can be extended by special Guardtime tools" env testExtendSignaturesCore1,
  testThat "RejectDocument adds to the log" env testRejectDocumentEvidenceLog,
  testThat "RestartDocument adds to the log" env testRestartDocumentEvidenceLog,
  testThat "SetDocumentInviteTime adds to the log" env testSetDocumentInviteTimeEvidenceLog,
  testThat "SignDocument adds to the log" env testSignDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" env testTimeoutDocumentEvidenceLog,
  testThat "Documents are shared in company properly" env testGetDocumentsSharedInCompany,
  testThat "SetDocumentUnsavedDraft and filtering based on unsaved_draft works" env testSetDocumentUnsavedDraft,
  testThat "Documents sorting SQL syntax is correct" env testGetDocumentsSQLSorted,
  testThat "Documents searching by text works" env testGetDocumentsSQLTextFiltered,

  testThat "PreparationToPending adds to the log" env testPreparationToPendingEvidenceLog,
  testThat "MarkInvitationRead adds correct text to the log" env testMarkInvitationReadEvidenceLog,
  testThat "SaveSigAttachment adds to the log" env testSaveSigAttachmentEvidenceLog,
  testThat "DeleteSigAttachment will not work after signing" env testDeleteSigAttachmentAlreadySigned,
  testThat "DeleteSigAttachment adds to the log" env testDeleteSigAttachmentEvidenceLog,
  testThat "CloseDocument adds to the log" env testCloseDocumentEvidenceLog,
  testThat "ChangeSignatoryEmailWhenUndelivered adds to the log" env testChangeSignatoryEmailWhenUndeliveredEvidenceLog,
  testThat "CancelDocument adds to the log" env testCancelDocumentEvidenceLog,
  testThat "ELegAbortDocument adds to the log" env testELegAbortDocumentDocumentEvidenceLog,

  testThat "AppendFirstSealedFile adds to the log" env testAppendFirstSealedFileEvidenceLog,
  testThat "AddInvitationEvidence adds to the log" env testAddInvitationEvidenceLog,
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

  testThat "removeDocumentAttachment fails if not in preparation" env testRemoveDocumentAttachmentFailsIfNotPreparation,
  testThat "removeDocumentAttachment return False if there's no attachments" env testRemoveDocumentAttachmentOk,

  testThat "UpdateSigAttachments works as advertised" env testUpdateSigAttachmentsAttachmentsOk,

  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "TimeoutDocument fails when document is not signable" env testTimeoutDocumentNonSignableLeft,

  -- archive & doc deletion tests
  testThat "ArchiveDocument fails if the document is pending or awaiting author" env testArchiveDocumentPendingLeft,
  testThat "ArchiveDocument succeeds if the archiving user is the author" env testArchiveDocumentAuthorRight,
  testThat "ArchiveDocument succeeds if the archiving user is a company admin" env testArchiveDocumentCompanyAdminRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the author" env testRestoreArchivedDocumentAuthorRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the company admin" env testRestoreArchiveDocumentCompanyAdminRight,
  -- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged

  testThat "ArchiveDocument fails if the archiving user is an unrelated user" env testArchiveDocumentUnrelatedUserLeft,
  testThat "ArchiveDocument fails if the archiving user is just another standard company user" env testArchiveDocumentCompanyStandardLeft,
  testThat "RestoreArchivedDocument fails if the storing user is an unrelated user" env testRestoreArchivedDocumentUnrelatedUserLeft,
  testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user" env testRestoreArchiveDocumentCompanyStandardLeft,

  testThat "PurgeDocuments purges documents" env testPurgeDocument,
  testThat "PurgeDocuments does not purge documents for saved users" env testPurgeDocumentUserSaved,
  testThat "PurgeDocuments does not purge documents for links waiting to be signed" env testPurgeDocumentActiveSignLink,

  testThat "GetDocumentsByAuthor doesn't return archived docs" env testGetDocumentsByAuthorNoArchivedDocs,
  testThat "When document is signed it's status class is signed" env testStatusClassSignedWhenAllSigned,
  testThat "When document is pending and some invitation is undelivered it's status is undelivered" env testStatusClassSignedWhenAllSigned
  ]

dataStructureProperties :: Test
dataStructureProperties = testGroup "data structure properties" [
  testCase "given example" testSignatories1,
  testProperty "signatories are different with different fields" propSignatoryDetailsNEq,
  testProperty "signatories are equal with same fields" propSignatoryDetailsEq
  ]

testSealMissingSignatures :: TestEnv ()
testSealMissingSignatures = do
  author <- addNewRandomUser
  let filename = "test/pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent
  doc <- addRandomDocumentWithAuthorAndConditionAndFile author isClosed file
  runScheduler $ sealMissingSignaturesNewerThan Nothing
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  case documentsealstatus doc' of
    Just (Guardtime{}) -> assertSuccess
    s -> assertFailure $ "Unexpected seal status: " ++ show s

testExtendSignatures :: TestEnv ()
testExtendSignatures = do
  author <- addNewRandomUser
  let filename = "test/pdfs/extensible.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent
  file1 <- addNewFile filename filecontent
  file2 <- addNewFile filename filecontent
  doc <- addRandomDocumentWithAuthorAndConditionAndFile author (isSignable &&^ isClosed) file
  now <- getMinutesTime
  let actor = systemActor (2 `monthsBefore` now)
  -- Append a file to tweak the modification time
  dbUpdate $ AppendFirstSealedFile (documentid doc) file1 Guardtime{ extended = False, private = False } actor
  dbUpdate $ AppendExtendedSealedFile (documentid doc) file2 Guardtime{ extended = False, private = False } actor
  runScheduler extendSignatures
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  case documentsealstatus doc' of
    Just (Guardtime{ extended = True }) -> assertSuccess
    s -> assertFailure $ "Unexpected extension status: " ++ show s

testExtendSignaturesCore1 :: TestEnv ()
testExtendSignaturesCore1 = do
  author <- addNewRandomUser
  let filename = "GuardTime/fix-incorrect-core/broken.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent
  doc <- addRandomDocumentWithAuthorAndConditionAndFile author isClosed file
  now <- getMinutesTime
  dbUpdate $ AppendExtendedSealedFile (documentid doc) file Guardtime{ extended = False, private = False } $ systemActor now
  runScheduler extendSignatures
  return ()
{-

Expected output before October 15th:

Verification failed with SYNTACTIC_CHECK_FAILURE - trying temporary Guardtime extension tool
GuardTime verification after extension failed for document #x: Invalid "PUBLICATION_FAILURE"

After October 20th: verification should pass, and this test should be tweaked.

-}

testNewDocumentForNonCompanyUser :: TestEnv ()
testNewDocumentForNonCompanyUser = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable) "doc title"
  assertGoodNewDocument Nothing (Signable) "doc title" result

testNewDocumentForACompanyUser :: TestEnv ()
testNewDocumentForACompanyUser = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable) "doc title"
  assertGoodNewDocument (Just company) (Signable) "doc title" result

testRejectDocumentEvidenceLog :: TestEnv ()
testRejectDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  randomUpdate $ \m t->RejectDocument (documentid doc) (signatorylinkid sl) m (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current RejectDocumentEvidence) lg

testRestartDocumentEvidenceLog :: TestEnv ()
testRestartDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  randomUpdate $ \t->CancelDocument (documentid doc) (systemActor t)
  cdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  mdoc <- randomUpdate $ \t->RestartDocument cdoc (systemActor t)
  assertJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromJust mdoc)
  assertJust $ find (\e -> evType e == Current RestartDocumentEvidence) lg
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg
  lg2 <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg2

testSetDocumentInviteTimeEvidenceLog :: TestEnv ()
testSetDocumentInviteTimeEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  now <- getMinutesTime
  let t = 1 `minutesAfter` fromMaybe now (signtime <$> documentinvitetime doc)
  success <- dbUpdate $ SetDocumentInviteTime (documentid doc) t (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current SetDocumentInviteTimeEvidence) lg

getScreenshots :: (MonadIO m, MonadDB m) => m SignatoryScreenshots.SignatoryScreenshots
getScreenshots = do
  now <- getMinutesTime
  first_ <- liftIO $ BS.readFile "test/screenshots/s1.jpg"
  signing <- liftIO $ BS.readFile "test/screenshots/s2.jpg"
  let mkss i = Just (now, Screenshot.Screenshot{  Screenshot.image = Binary i
                                               })
  return $ SignatoryScreenshots.emptySignatoryScreenshots{ SignatoryScreenshots.first = mkss first_
                                     , SignatoryScreenshots.signing = mkss signing
                                     }

testSignDocumentEvidenceLog :: TestEnv ()
testSignDocumentEvidenceLog = do
  author <- addNewRandomUser
  screenshots <- getScreenshots
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks) &&^ (all ((==) StandardAuthentication . signatorylinkauthenticationmethod) . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing screenshots (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current SignDocumentEvidence) lg

testTimeoutDocumentEvidenceLog :: TestEnv ()
testTimeoutDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  success <- randomUpdate $ \t->TimeoutDocument (documentid doc) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current TimeoutDocumentEvidence) lg

testPreparationToPendingEvidenceLog :: TestEnv ()
testPreparationToPendingEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current PreparationToPendingEvidence) lg

testMarkInvitationReadEvidenceLog :: TestEnv ()
testMarkInvitationReadEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = getAuthorSigLink doc
  success <- randomUpdate $ \t->MarkInvitationRead (documentid doc) (signatorylinkid sl) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  -- The text for MarkInvitationRead in the event log is hard to check
  -- manually since it relies on external mail system notifications,
  -- so we test it explicitly.
  let me = find (\e -> evType e == Current MarkInvitationReadEvidence) lg
      expected = "Scrive’s notification system reported that the invitation to "
              ++ (if signatoryispartner sl then "sign" else "view")
              ++ " (sent to " ++ getEmail sl ++ ") was opened."
  assertEqual "Correct event text" (Just expected) (evText <$> me)

testSaveSigAttachmentEvidenceLog :: TestEnv ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ isSignable)
  file <- addNewRandomFile
  let sa = SignatoryAttachment { signatoryattachmentfile        = Nothing
                               , signatoryattachmentname        = "attachment"
                               , signatoryattachmentdescription = "gimme!"
                               }
  randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [sa] (systemActor t)
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing
  randomUpdate $ \t->SaveSigAttachment doc (signatorylinkid $ (documentsignatorylinks doc) !! 0) sa file (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current SaveSigAttachmentEvidence) lg


testDeleteSigAttachmentAlreadySigned :: TestEnv ()
testDeleteSigAttachmentAlreadySigned = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author $ (    isSignable
                                                           &&^ isPreparation
                                                           &&^ ((all (isSignatory &&^ not .hasSigned &&^ (==) StandardAuthentication . signatorylinkauthenticationmethod)) . documentsignatorylinks)
                                                           &&^ (((==) 2) . length .documentsignatorylinks))
  file <- addNewRandomFile
  let sl = (documentsignatorylinks doc) !! 1
      sa = SignatoryAttachment { signatoryattachmentfile        = Nothing
                               , signatoryattachmentname        = "attachment"
                               , signatoryattachmentdescription = "gimme!"
                               }
  _<-randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ sl)
                        [sa] (systemActor t)
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing
  randomUpdate $ \t->SaveSigAttachment doc (signatorylinkid $ sl) sa file (systemActor t)

  randomUpdate $ \t->DeleteSigAttachment doc (signatorylinkid $ sl) file (systemActor t)
  randomUpdate $ \t->SaveSigAttachment doc (signatorylinkid $ sl) sa file (systemActor t)
  randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
  assertRaisesKontra (\SignatoryHasAlreadySigned {} -> True) $ do
    randomUpdate $ \t->DeleteSigAttachment doc (signatorylinkid $ sl) file (systemActor t)

testDeleteSigAttachmentEvidenceLog :: TestEnv ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _<-randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Just file
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (systemActor t)
  randomUpdate $ \t->DeleteSigAttachment doc (signatorylinkid $ (documentsignatorylinks doc) !! 0) file (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current DeleteSigAttachmentEvidence) lg

testAddInvitationEvidenceLog :: TestEnv ()
testAddInvitationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  success <- randomUpdate $ \t->AddInvitationEvidence (documentid doc) (signatorylinkid sl) Nothing (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current InvitationEvidence) lg

testAppendFirstSealedFileEvidenceLog :: TestEnv ()
testAppendFirstSealedFileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  file <- addNewRandomFile
  randomUpdate $ \t -> AppendFirstSealedFile (documentid doc) file
                         Guardtime{ extended = False, private = False } (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current AttachGuardtimeSealedFileEvidence) lg

testCancelDocumentEvidenceLog :: TestEnv ()
testCancelDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  randomUpdate $ \t-> CancelDocument (documentid doc) (systemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg

testELegAbortDocumentDocumentEvidenceLog :: TestEnv ()
testELegAbortDocumentDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = getAuthorSigLink doc
  randomUpdate $ \t-> ELegAbortDocument (documentid doc) (signatorylinkid sl) "message" "first" "last" "198404011234" (systemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CancelDocumenElegEvidence) lg

testChangeSignatoryEmailWhenUndeliveredEvidenceLog :: TestEnv ()
testChangeSignatoryEmailWhenUndeliveredEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  success <- randomUpdate $ \t-> ChangeSignatoryEmailWhenUndelivered (documentid doc) (signatorylinkid sl) Nothing "email@email.com" (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current ChangeSignatoryEmailWhenUndeliveredEvidence) lg

testCloseDocumentEvidenceLog :: TestEnv ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (all ((==) StandardAuthentication . signatorylinkauthenticationmethod) . documentsignatorylinks))
  forM_ (documentsignatorylinks doc) $ \sl -> when (isSignatory sl) $ do
    randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
    randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
  randomUpdate $ \t-> CloseDocument (documentid doc) (systemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> TestEnv (User, MinutesTime, Either String Document)
performNewDocumentWithRandomUser mcompany doctype title = do
  user <- maybe addNewRandomUser (\c -> addNewRandomCompanyUser (companyid c) False) mcompany
  ctx <- mkContext defaultValue
  let aa = authorActor ctx user
  mdoc <- randomUpdate $ NewDocument user title doctype 0 aa
  return (user, ctxtime ctx, maybe (Left "no document") Right mdoc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> (User, MinutesTime, Either String Document) -> TestEnv ()
assertGoodNewDocument mcompany doctype title (user, time, edoc) = do
    let (Right doc) = edoc
    assertRight edoc
    assertEqual "Correct title" title (documenttitle doc)
    assertEqual "Correct type" doctype (documenttype doc)
    assertEqual "Doc has user's lang" (getLang user) (getLang doc)
    assertEqual "Doc creation time" time (documentctime doc)
    assertEqual "Doc modification time" time (documentmtime doc)
    assertEqual "No author attachments" [] (documentauthorattachments doc)
    assertEqual "No sig attachments" [] (concatMap signatoryattachments $ documentsignatorylinks doc)
    assertBool "Uses email identification only" (all ((==) StandardAuthentication . signatorylinkauthenticationmethod) (documentsignatorylinks doc))
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
testCancelDocumentCancelsDocument = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ isPending)
  ctx <- mkContext defaultValue
  randomUpdate $ CancelDocument (documentid doc) (authorActor ctx user)

  canceleddoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  let doNotCompareStatusClass x = x { signatorylinkstatusclass = SCDraft }
  assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
  assertEqual "Updated modification time" (ctxtime ctx) (documentmtime canceleddoc)
  assertBool "Matching cancellation reason" (all (not . isJust . signatorylinkelegdatamismatchmessage) . documentsignatorylinks $ canceleddoc)
  assertEqual "Siglinks are unchanged"
                  (map doNotCompareStatusClass (documentsignatorylinks doc))
                  (map doNotCompareStatusClass (documentsignatorylinks canceleddoc))
  assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: TestEnv ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ not . isPending)
  ctx <- mkContext defaultValue
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $
               randomUpdate $ CancelDocument (documentid doc)
                              (authorActor ctx user)

testSignatories1 :: Assertion
testSignatories1 =
  let s1 = defaultValue { signatoryfields =
              [ SignatoryField FirstNameFT "Eric" True False []
                ,SignatoryField LastNameFT "Normand" True False []
                ]}
      s2 = defaultValue { signatoryfields =
              [SignatoryField LastNameFT "Normand" True False []
              ,SignatoryField FirstNameFT "Eric" True False []
              ]}
  in assertBool "Signatories fields should be equal" (s1 == s2)

propSignatoryDetailsEq :: SignOrder -> SignatoryLink -> Property
propSignatoryDetailsEq o1 sd =
   (o1 == o1) ==> sd{signatorysignorder = o1} == sd{signatorysignorder = o1}

propSignatoryDetailsNEq :: SignOrder -> SignOrder -> SignatoryLink -> Property
propSignatoryDetailsNEq o1 o2 sd =
  (o1 /= o2) ==> sd{signatorysignorder = o1} /= sd{signatorysignorder = o2}

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
testArchiveDocumentPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  _doc0 <- addRandomDocumentWithAuthorAndCondition author isPending
  _doc1 <- addRandomDocumentWithAuthorAndCondition author isPending

  let doc = _doc1

  assertRaisesKontra (\(DocumentStatusShouldBe {}) -> True) $
               randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)


testArchiveDocumentAuthorRight :: TestEnv ()
testArchiveDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertOneArchivedSigLink ndoc

testArchiveDocumentCompanyAdminRight :: TestEnv ()
testArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid adminuser) (documentid doc) (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertOneArchivedSigLink ndoc

testRestoreArchivedDocumentAuthorRight :: TestEnv ()
testRestoreArchivedDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertNoArchivedSigLink ndoc

testRestoreArchiveDocumentCompanyAdminRight :: TestEnv ()
testRestoreArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  randomUpdate $ \t->RestoreArchivedDocument adminuser (documentid doc) (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertNoArchivedSigLink ndoc


testPurgeDocument :: TestEnv ()
testPurgeDocument = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  now <- getMinutesTime
  archived1 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged zero documents when not deleted" 0 archived1
  randomUpdate $ \t -> ArchiveDocument (userid author) (documentid doc) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged single document" 1 archived2

testPurgeDocumentUserSaved :: TestEnv ()
testPurgeDocumentUserSaved = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  archived1 <- dbUpdate $ PurgeDocuments 1 0
  now <- getMinutesTime
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 1 0
  assertEqual "Purged zero documents before delete" 0 archived1
  assertEqual "Purged zero documents before time passed after delete" 0 archived2

testPurgeDocumentActiveSignLink :: TestEnv ()
testPurgeDocumentActiveSignLink = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (isClosed &&^ (not . null . filter (isSignatory &&^ (not . isAuthor)). documentsignatorylinks))
  now <- getMinutesTime
  randomUpdate $ \t -> ArchiveDocument (userid author) (documentid doc) ((systemActor t) { actorTime = now })
  updateMTimeAndObjectVersion (documentid doc) now
  archived <- dbUpdate $ PurgeDocuments 0 1
  assertEqual "Purged zero documents" 0 archived

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: TestEnv ()
testArchiveDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomUser
  assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True) $
    randomUpdate $ \t -> ArchiveDocument (userid unrelateduser) (documentid doc) (systemActor t)

testArchiveDocumentCompanyStandardLeft :: TestEnv ()
testArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
    randomUpdate $ \t->ArchiveDocument (userid standarduser) (documentid doc) (systemActor t)

testRestoreArchivedDocumentUnrelatedUserLeft :: TestEnv ()
testRestoreArchivedDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomUser
  randomUpdate $ \t -> ArchiveDocument (userid author) (documentid doc) (systemActor t)
  assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True)$ do
    randomUpdate $ \t->RestoreArchivedDocument unrelateduser (documentid doc) (systemActor t)

testRestoreArchiveDocumentCompanyStandardLeft :: TestEnv ()
testRestoreArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $ do
    randomUpdate $ \t->RestoreArchivedDocument standarduser (documentid doc) (systemActor t)

testGetDocumentsByAuthorNoArchivedDocs :: TestEnv ()
testGetDocumentsByAuthorNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsByAuthor . userid)

checkQueryDoesntContainArchivedDocs :: DBQuery TestEnv q [Document] => (User -> q) -> TestEnv ()
checkQueryDoesntContainArchivedDocs qry = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> (isPreparation d || isClosed d) && (isSignable d))
  docsbeforearchive <- dbQuery (qry author)
  assertEqual "Expecting one doc before archive" [documentid doc] (map documentid docsbeforearchive)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  docsafterarchive <- dbQuery (qry author)
  assertEqual "Expecting no docs after archive" [] (map documentid docsafterarchive)
  randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (systemActor t)
  docsafterestore <- dbQuery (qry author)
  assertEqual "Expecting one doc after restoring" [documentid doc] (map documentid docsafterestore)

testSetDocumentLangNotLeft :: TestEnv ()
testSetDocumentLangNotLeft = doTimes 10 $ do
  success <- randomUpdate $ \d l t -> SetDocumentLang d l (systemActor t)
  assert $ not success

testNewDocumentDependencies :: TestEnv ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  -- execute
  ctx <- mkContext defaultValue
  let aa = authorActor ctx author
  mdoc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype 0 aa)
  -- assert
  assertJust mdoc
  assertInvariants $ fromJust mdoc

testDocumentCanBeCreatedAndFetchedByID :: TestEnv ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  let aa = authorActor ctx author
  mdoc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype 0 aa)
  let doc = case mdoc of
          Nothing -> error "No document"
          Just d  -> d
  -- execute
  ndoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  -- assert

  assert $ documentid doc  == documentid ndoc
  assertInvariants ndoc

testDocumentAttachNotPreparationLeft :: TestEnv ()
testDocumentAttachNotPreparationLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \t->AttachFile (documentid doc) file (systemActor t)

testDocumentAttachPreparationRight :: TestEnv ()
testDocumentAttachPreparationRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  randomUpdate $ \t -> AttachFile (documentid doc) file (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  --assert
  assertInvariants ndoc


testNoDocumentAttachAlwaysLeft :: TestEnv ()
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ (\docid t -> AttachFile docid file (systemActor t))
  --assert

testDocumentAttachHasAttachment :: TestEnv ()
testDocumentAttachHasAttachment = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute

  randomUpdate $ \t -> AttachFile (documentid doc) file (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  -- assertJust $ find ((== a) . filename) (documentfiles $ fromRight edoc)
  assertInvariants ndoc

testNoDocumentAppendSealedAlwaysLeft :: TestEnv ()
testNoDocumentAppendSealedAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ (\docid -> AppendFirstSealedFile docid file Missing (systemActor time))

testDocumentAppendSealedPendingRight :: TestEnv ()
testDocumentAppendSealedPendingRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocument ((randomDocumentAllowsDefault author) { randomDocumentAllowedStatuses = [Closed]
                                                                 })
  file <- addNewRandomFile

  --execute

  now <- getMinutesTime
  success <- randomUpdate $ AppendExtendedSealedFile (documentid doc) file Missing $ systemActor now
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  --assert
  assert success
  assertBool "Should have new file attached, but it's not" $ Just file == documentsealedfile ndoc


testGetTimedOutButPendingDocuments :: TestEnv ()
testGetTimedOutButPendingDocuments = doTimes 1 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ (isJust . documenttimeouttime))
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

  let t = fromJust $ documenttimeouttime doc
  --execute
  docsA <- dbQuery $ GetTimeoutedButPendingDocumentsChunk ((-10) `minutesAfter` t) 100
  docsB <- dbQuery $ GetTimeoutedButPendingDocumentsChunk (10 `minutesAfter` t) 100

  --assert
  assertEqual "Documents do not timeout before time" [] (map documentstatus docsA)
  assertEqual "Documents timeout after time" [Pending] (map documentstatus docsB)

testNotPreparationResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNotPreparationResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  mt <- rand 10 arbitrary
  sf <- signatoryFieldsFromUser author
  --execute
  success <- dbUpdate $ ResetSignatoryDetails (documentid doc) [defaultValue { signatoryfields = sf}] (systemActor mt)
  --assert
  assert $ not success

testPreparationResetSignatoryDetailsAlwaysRight :: TestEnv ()
testPreparationResetSignatoryDetailsAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  mt <- rand 10 arbitrary
  --execute
  success <- dbUpdate $ ResetSignatoryDetails (documentid doc) [defaultValue { signatoryisauthor = True , maybesignatory = Just $ userid author}] (systemActor mt)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  assert success
  assertInvariants ndoc

testPreparationResetSignatoryDetails2Works :: TestEnv ()
testPreparationResetSignatoryDetails2Works = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  mt <- rand 10 arbitrary
  --execute
  let newData1 = defaultValue {   signatoryisauthor = True , maybesignatory = Just $ userid author}
  success1 <- dbUpdate $ ResetSignatoryDetails (documentid doc) [newData1] (systemActor mt)
  assert success1
  ndoc1 <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertEqual "Proper delivery method set" [EmailDelivery] (map signatorylinkdeliverymethod (documentsignatorylinks ndoc1))
  assertEqual "Proper authentication method set" [StandardAuthentication] (map signatorylinkauthenticationmethod (documentsignatorylinks ndoc1))

  let newData2 =  defaultValue { signatoryisauthor = True, maybesignatory = Just $ userid author , signatorylinkdeliverymethod = PadDelivery, signatorylinkauthenticationmethod = ELegAuthentication }
  success2 <- dbUpdate $ ResetSignatoryDetails (documentid doc) [newData2] (systemActor mt)
  assert success2
  ndoc2 <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertEqual "Proper delivery method set" [PadDelivery] (map signatorylinkdeliverymethod (documentsignatorylinks ndoc2))
  assertEqual "Proper authentication method set" [ELegAuthentication] (map signatorylinkauthenticationmethod (documentsignatorylinks ndoc2))

  --assert
  assertInvariants ndoc1
  assertInvariants ndoc2

testNoDocumentResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNoDocumentResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  --author <- addNewRandomUser
  mt <- rand 10 arbitrary
  --execute
  -- non-existent docid

  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    dbUpdate $ ResetSignatoryDetails a [defaultValue { signatoryisauthor = True } ] (systemActor mt)



testGetDocumentsSharedInCompany :: TestEnv ()
testGetDocumentsSharedInCompany = doTimes 10 $ do
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

  -- | This test is good only for not admins
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


testGetDocumentsSQLTextFiltered :: TestEnv ()
testGetDocumentsSQLTextFiltered = doTimes 1 $ do
  -- setup
  Just author <- addNewUser "Bob" "Blue" "bill@zonk.com"
  Just author2 <- addNewUser "Anna" "Max" "herm@qqq.com"
  doc1 <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  _doc3 <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  _doc4 <- addRandomDocumentWithAuthorAndCondition author2 (isSignable &&^ isPreparation)

  let domains = [ DocumentsVisibleToUser (userid author)]
      first_name = getFirstName (head (documentsignatorylinks doc1))
      last_name = getLastName (head (documentsignatorylinks doc1))
      email = getEmail (head (documentsignatorylinks doc1))
      filters1 = [DocumentFilterByString "Bob"]
      filters2 = [DocumentFilterByString "Blue"]
      filters3 = [DocumentFilterByString "bill@"]
      filters4 = [DocumentFilterByString title]
      filters5 = [DocumentFilterByString title1]
      filters6 = [DocumentFilterByString title2]
      -- we want to check case-insensitivity and Swedish characters
      title  = "thisshouldbeuniquetitleöåä"
      title1 = "thisshouldbeuniquetitle"
      title2 = "THISshouldbeuniquetitleÖÅÄ"

  actor <- arbitraryAuthorActor

  success <- dbUpdate $ SetDocumentTitle (documentid doc1) title (actor)
  assert success
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc1

  docs0 <- dbQuery $ GetDocuments domains [] [] (0,maxBound)
  docs1 <- dbQuery $ GetDocuments domains filters1 [] (0,maxBound)
  docs2 <- dbQuery $ GetDocuments domains filters2 [] (0,maxBound)
  docs3 <- dbQuery $ GetDocuments domains filters3 [] (0,maxBound)
  docs4 <- dbQuery $ GetDocuments domains filters4 [] (0,maxBound)
  docs5 <- dbQuery $ GetDocuments domains filters5 [] (0,maxBound)
  docs6 <- dbQuery $ GetDocuments domains filters6 [] (0,maxBound)

  assertEqual ("GetDocuments fetches all documents without filter") 3 (length docs0)
  assertEqual ("Document title really got changed") title (documenttitle ndoc)
  assertEqual ("GetDocuments and filter by title: " ++ title1) 1 (length docs5)
  assertEqual ("GetDocuments and filter by title: " ++ title) 1 (length docs4)
  assertEqual ("GetDocuments and filter by title: " ++ title2) 1 (length docs6)
  assertEqual ("GetDocuments and filter by first name: " ++ first_name) 3 (length docs1)
  assertEqual ("GetDocuments and filter by last name: " ++ last_name) 3 (length docs2)
  assertEqual ("GetDocuments and filter by email: " ++ email) 3 (length docs3)

testGetDocumentsSQLSorted :: TestEnv ()
testGetDocumentsSQLSorted = doTimes 1 $ do
  -- setup
  author <- addNewRandomUser
  _doc <- addRandomDocumentWithAuthorAndCondition author (const True)

  let domains = [ DocumentsVisibleToUser (userid author)
                ]
      filters = []
  _docs <- dbQuery $ GetDocuments domains filters
            [ Desc DocumentOrderByTitle
            , Desc DocumentOrderByMTime
            , Desc DocumentOrderByStatusClass
            , Desc DocumentOrderByType
            , Desc DocumentOrderByPartners
            ]
            (0,maxBound)
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
           _ <- dbUpdate $ TemplateFromDocument docid (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  newuser <- addNewRandomUser

  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor newuser doc (systemActor mt))
  _ <- dbUpdate $ DocumentFromTemplate docid'  (systemActor mt)

  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  let [author1] = filter isAuthor $ documentsignatorylinks doc
  let [author2] = filter isAuthor $ documentsignatorylinks ndoc
  let isCustom (SignatoryField { sfType = CustomFT _ _ }) = True
      isCustom _ = False
  if (fmap sfValue $ filter isCustom $ signatoryfields $ author1)
     == (fmap sfValue $ filter isCustom $ signatoryfields $ author2)
    then assertSuccess
    else assertFailure "Replacing signatory details based on user is loosing fields | SKRIVAPADEV-294"


testCreateFromTemplateCompanyField :: TestEnv ()
testCreateFromTemplateCompanyField = doTimes 10 $ do
  user <- addNewRandomUser
  company <- addNewCompany
  _ <- dbUpdate $ SetUserCompany (userid user)  (companyid company)
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (\doc -> isPreparation doc)
  tmpdoc <- dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           _ <- dbUpdate $ TemplateFromDocument docid (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor user' doc (systemActor mt))
  _ <- dbUpdate $ DocumentFromTemplate docid'  (systemActor mt)
  doc' <- dbQuery $ GetDocumentByDocumentID docid'
  let [author] = filter isAuthor $ documentsignatorylinks doc'
  assertEqual "Author signatory link company name is not same as his company" (getCompanyName company) (getCompanyName author)



testAddDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testAddDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  success <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) file (systemActor t)
  --assert
  assert $ not success

testAddDocumentAttachmentOk :: TestEnv ()
testAddDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute

  success <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) file (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  --assert
  assert success
  assertEqual "Author attachment was really attached" [file]
                  (map authorattachmentfile $ documentauthorattachments ndoc)

testRemoveDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testRemoveDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  success <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (unsafeFileID 0) (systemActor t)
  --assert
  assert $ not success

testRemoveDocumentAttachmentOk :: TestEnv ()
testRemoveDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  success <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (unsafeFileID 0) (systemActor t)
  --assert
  assert $ not success

---------------------------------------------------------------------

testUpdateSigAttachmentsAttachmentsOk :: TestEnv ()
testUpdateSigAttachmentsAttachmentsOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file1 <- addNewRandomFile
  file2 <- addNewRandomFile
  --execute
  let att1 = SignatoryAttachment { signatoryattachmentfile = Just file1
                                 , signatoryattachmentname = "att1"
                                 , signatoryattachmentdescription = "att1 description"
                                 }
  let att2 = SignatoryAttachment { signatoryattachmentfile = Nothing
                                 , signatoryattachmentname = "att2"
                                 , signatoryattachmentdescription = "att2 description"
                                 }
  ctx <- mkContext defaultValue
  (time, sl) <- rand 10 arbitrary
  let sa = signatoryActor ctx{ ctxtime = time } sl
  randomUpdate $ SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) [att1, att2] sa

  doc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  randomUpdate $ DeleteSigAttachment doc (signatorylinkid $ (documentsignatorylinks doc) !! 0) file1 sa
  ndoc1 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  randomUpdate $ SaveSigAttachment doc (signatorylinkid $ (documentsignatorylinks doc) !! 0) att1 file2 sa
  ndoc2 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  --assert
  assertEqual "Both attachments were attached" 2 (length (signatoryattachments $ (documentsignatorylinks doc1) !! 0))

  assertBool "All signatory attachments are not connected to files" (all (isNothing . signatoryattachmentfile)
                                                                           (signatoryattachments $ (documentsignatorylinks ndoc1) !! 0))

  assertBool "Attachment connected to signatory"
                 (Just file2 `elem` map signatoryattachmentfile (signatoryattachments $ (documentsignatorylinks ndoc2) !! 0))

------------------------------------------------

testTimeoutDocumentNonSignableLeft :: TestEnv ()
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    dbUpdate $ TimeoutDocument (documentid doc) (systemActor mt)

testTimeoutDocumentSignableNotPendingLeft :: TestEnv ()
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \t->TimeoutDocument (documentid doc) (systemActor t)

testTimeoutDocumentSignablePendingRight :: TestEnv ()
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  --execute
  randomUpdate $ \t->TimeoutDocument (documentid doc) (systemActor t)
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testTimeoutDocumentSignableNotLeft :: TestEnv ()
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  actor <- arbitrarySystemActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ \d-> TimeoutDocument d actor

testSignDocumentNonSignableLeft :: TestEnv ()
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignableNotPendingLeft :: TestEnv ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  let Just sl = getSigLinkFor doc author
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignablePendingRight :: TestEnv ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^
           any ((== StandardAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) . documentsignatorylinks)
  let Just sl = find ((== StandardAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) (documentsignatorylinks doc)
  time <- rand 10 arbitrary
  randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
  randomUpdate $ SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentSignablePendingElegRight :: TestEnv ()
testSignDocumentSignablePendingElegRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^
           any ((== ELegAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) . documentsignatorylinks)
  let Just sl = find ((== ELegAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) (documentsignatorylinks doc)
  time <- rand 10 arbitrary
  randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
  randomUpdate $ \signinfo -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (Just signinfo) SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentNotLeft :: TestEnv ()
testSignDocumentNotLeft = doTimes 10 $ do
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    -- our machinery is broken here, baseline condition has only relations
    -- this should be ignored and properly return info about non existing document
    randomUpdate $ \d sl mh si t -> SignDocument d sl mh si SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testPreparationToPendingNotSignableLeft :: TestEnv ()
testPreparationToPendingNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    randomUpdate $ PreparationToPending (documentid doc) (systemActor time) Nothing

testPreparationToPendingSignableNotPreparationLeft :: TestEnv ()
testPreparationToPendingSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = documentAllStatuses \\ [Preparation]
         }
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ PreparationToPending (documentid doc) (systemActor time) Nothing

testPreparationToPendingNotLeft :: TestEnv ()
testPreparationToPendingNotLeft = doTimes 100 $ do
  (time, did) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ PreparationToPending did (systemActor time) Nothing

testPreparationToPendingSignablePreparationRight :: TestEnv ()
testPreparationToPendingSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Preparation]
         , randomDocumentCondition = (any isSignatory . documentsignatorylinks) &&^
          (isJust . documentfile) &&^
          ((==) 1 . length . filter isAuthor . documentsignatorylinks)
         }
  time <- rand 10 arbitrary
  randomUpdate $ PreparationToPending (documentid doc) (systemActor time) Nothing
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testRejectDocumentNotSignableLeft :: TestEnv ()
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  ctx <- mkContext defaultValue
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing
           (authorActor ctx{ ctxtime = time } author)

testRejectDocumentSignableNotPendingLeft :: TestEnv ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ not . isPending)
  let Just sl = getSigLinkFor doc author
  ctx <- mkContext defaultValue
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing
           (authorActor ctx{ ctxtime = time } author)

testRejectDocumentNotLeft :: TestEnv ()
testRejectDocumentNotLeft = doTimes 10 $ do
  _ <- addRandomDocument . randomDocumentAllowsDefault =<< addNewRandomUser
  ctx <- mkContext defaultValue
  (did, time, sl) <- rand 10 arbitrary
  let sa = signatoryActor ctx{ ctxtime = time } sl
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ RejectDocument did (signatorylinkid sl) Nothing sa

testRejectDocumentSignablePendingRight :: TestEnv ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  slid <- rand 10 $ elements (map signatorylinkid . filter (signatoryispartner) $ documentsignatorylinks doc)
  let Just sl = getSigLinkFor doc slid
  ctx <- mkContext defaultValue
  time <- rand 10 arbitrary
  let sa = signatoryActor ctx{ ctxtime = time } sl
  randomUpdate $ RejectDocument (documentid doc) slid Nothing sa
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testMarkInvitationRead :: TestEnv ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks))

  sl' <- rand 10 $ elements $ documentsignatorylinks doc
  let slid = signatorylinkid sl'
  ctx <- mkContext defaultValue
  time <- getMinutesTime
  success <- dbUpdate $ MarkInvitationRead (documentid doc) slid
          (signatoryActor ctx{ ctxtime = time } sl')
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  let Just sl = getSigLinkFor ndoc slid
  assertEqual "Invitation read time should be set." (Just time) (maybereadinvite sl)

testMarkInvitationReadDocDoesntExist :: TestEnv ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  ctx <- mkContext defaultValue
  (did, sl, time) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist{} -> True) $ do
    _ <- randomUpdate $ MarkInvitationRead did (signatorylinkid sl)
            (signatoryActor ctx{ ctxtime = time } sl)
    return ()
  return ()

testMarkDocumentSeenNotSignableLeft :: TestEnv ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }

  forEachSignatoryLink doc $ \sl ->
    when (isNothing $ maybeseeninfo sl) $ do
      ctx <- mkContext defaultValue
      time <- rand 10 arbitrary
      let sa = signatoryActor ctx{ ctxtime = time } sl
      assertRaisesKontra (\DocumentTypeShouldBe{} -> True) $ do
        randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa

testMarkDocumentSeenClosedOrPreparationLeft :: TestEnv ()
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         }
  forEachSignatoryLink doc $ \sl ->
    when (isNothing $ maybeseeninfo sl) $ do
      ctx <- mkContext defaultValue
      time <- rand 10 arbitrary
      let sa = signatoryActor ctx{ ctxtime = time } sl
      assertRaisesKontra (\DocumentStatusShouldBe{} -> True) $ do
        randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa

testMarkDocumentSeenNotLeft :: TestEnv ()
testMarkDocumentSeenNotLeft = doTimes 10 $ do
  author <- addNewRandomUser
  _doc <- addRandomDocument (randomDocumentAllowsDefault author)
  (d, s, m) <- rand 10 arbitrary
  a <- arbitrarySignatoryActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ MarkDocumentSeen d s m a

forEachSignatoryLink :: Document -> (SignatoryLink -> TestEnv ()) -> TestEnv ()
forEachSignatoryLink doc fn =
  let f [] = return ()
      f (sl:sls) = do
        fn sl
        f sls
  in f (documentsignatorylinks doc)

testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                ctx <- mkContext defaultValue
                time <- rand 10 arbitrary
                let sa = signatoryActor ctx{ ctxtime = time } sl
                randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
                let Just  tsl  = getSigLinkFor ndoc (signatorylinkid sl)
                assertBool "Signatorylink should be marked seen now." (hasSeen tsl))

testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  forEachSignatoryLink doc $ \sl ->
    when (not $ hasSeen sl) $ do
      mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
      ctx <- mkContext defaultValue
      time <- rand 10 arbitrary
      let sa = signatoryActor ctx{ ctxtime = time } sl
      assertRaisesKontra (\SignatoryTokenDoesNotMatch {} -> True) $ do
        randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh sa

testSetInvitationDeliveryStatusNotSignableLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  actor <- arbitrarySystemActor
  let Just sl = getAuthorSigLink doc
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    success <- randomUpdate $ \st-> SetEmailInvitationDeliveryStatus (documentid doc) (signatorylinkid sl) st actor
    assert $ not success


testSetInvitationDeliveryStatusNotLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  actor <- arbitrarySystemActor
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    success <- randomUpdate $ \d s st-> SetEmailInvitationDeliveryStatus d s st actor
    assert $ not success

testSetInvitationDeliveryStatusSignableRight :: TestEnv ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  st <- rand 10 arbitrary
  actor <- arbitrarySystemActor
  success <- randomUpdate $ SetEmailInvitationDeliveryStatus (documentid doc) slid st actor
  assert success

testSetDocumentTagsRight :: TestEnv ()
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  ctx <- mkContext defaultValue
  (tags, time) <- first S.fromList <$> rand 10 arbitrary
  let actor = authorActor ctx{ ctxtime = time } author
  success <- randomUpdate $ SetDocumentTags (documentid doc) tags actor
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  assertEqual "Tags should be equal" tags (documenttags ndoc)

testCloseDocumentSignableButNotEverybodyHasSigned :: TestEnv ()
testCloseDocumentSignableButNotEverybodyHasSigned = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = (\doc -> length (documentsignatorylinks doc) > 1) &&^
                                     (not . all (isSignatory =>>^ hasSigned) . documentsignatorylinks)
         }
  sa <- arbitrarySystemActor
  assertRaisesKontra (\(SignatoryHasNotYetSigned {}) -> True) $ do
    randomUpdate $ CloseDocument (documentid doc) sa

testCloseDocumentNotSignableNothing :: TestEnv ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  sa <- arbitrarySystemActor
  assertRaisesKontra (\(DocumentTypeShouldBe {}) -> True) $ do
    randomUpdate $ CloseDocument (documentid doc) sa

testCloseDocumentNotNothing :: TestEnv ()
testCloseDocumentNotNothing = doTimes 10 $ do
  sa <- arbitrarySystemActor
  did <- rand 10 arbitrary
  assertRaisesKontra (\(DocumentDoesNotExist {}) -> True) $ do
    randomUpdate $ CloseDocument did sa

testCancelDocumentNotSignableNothing :: TestEnv ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  time <- rand 10 arbitrary
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $
               randomUpdate $ CancelDocument (documentid doc)
                              (authorActor ctx{ ctxtime = time } author)

testCancelDocumentNotNothing :: TestEnv ()
testCancelDocumentNotNothing = doTimes 10 $ do
  aa <- arbitraryAuthorActor

  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $
             randomUpdate $ (\did -> CancelDocument did aa)

testSetDocumentTitleNotLeft :: TestEnv ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  (did, title) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- randomUpdate $ SetDocumentTitle did title actor
  assert $ not success

testSetDocumentTitleRight :: TestEnv ()
testSetDocumentTitleRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed)
         }
  let title = "my new cool title"
  actor <- arbitraryAuthorActor
  success <- randomUpdate $ SetDocumentTitle (documentid doc) title actor
  ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  assertEqual "Title is set properly" title (documenttitle ndoc)

testSetDocumentDaysToSignNotLeft :: TestEnv ()
testSetDocumentDaysToSignNotLeft = doTimes 10 $ do
  (did, d) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- randomUpdate $ SetDaysToSign did d actor
  assert $ not success

testSetDocumentDaysToSignRight :: TestEnv ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = not . isClosed
         }
  actor <- arbitraryAuthorActor
  let daystosign = 15
  success1 <- randomUpdate $ SetDaysToSign (documentid doc) daystosign actor
  ndoc1 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success1
  assertEqual "Days to sign is set properly" daystosign (documentdaystosign ndoc1)


assertInvariants :: Document -> TestEnv ()
assertInvariants document = do
  now <- getMinutesTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a

testGetDocumentsByCompanyWithFilteringCompany :: TestEnv ()
testGetDocumentsByCompanyWithFilteringCompany = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  let actor = systemActor time
  _ <- dbUpdate $ SetDocumentTags did (S.singleton $ DocumentTag name value) actor
  docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)

  assertEqual "Should have 1 document returned" (length docs') 1


testGetDocumentsByCompanyWithFilteringFilters :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFilters = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name value]] [] (0,maxBound)
  docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)

  assertEqual "Should have no documents returned" docs []
  assertEqual "Should have 1 document returned" [did] (map documentid docs')

testSetDocumentUnsavedDraft :: TestEnv ()
testSetDocumentUnsavedDraft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  doc <- dbQuery $ GetDocumentByDocumentID did
  docs1 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                     [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
  _ <- dbUpdate $ SetDocumentUnsavedDraft [did] True
  docs2 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                     [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
  _ <- dbUpdate $ SetDocumentUnsavedDraft [did] False
  docs3 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                     [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
  docs4 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                     [DocumentFilterUnsavedDraft True, DocumentFilterByDocumentID did] [] (0,maxBound)
  let isdraft = (isSignable doc && isPreparation doc)

  assertEqual "Should return the document" [did] (map documentid docs1)
  assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs2)
  assertEqual "Should return the document" [did] (map documentid docs3)
  assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs4)


testGetDocumentsByCompanyWithFilteringFinds :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFinds = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  let actor = systemActor time
  _ <- dbUpdate $ SetDocumentTags did (S.singleton $ DocumentTag name value) actor
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name value]] [] (0,maxBound)
  docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)

  assertEqual "Should have one document returned" [did] (map documentid docs)
  assertEqual "Should have one document returned" [did] (map documentid docs')

testGetDocumentsByCompanyWithFilteringFindsMultiple :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFindsMultiple = doTimes 10 $ do
  (name1, value1) <- rand 10 arbitrary
  (name2, value2) <- rand 10 arbitrary
  (name3, value3) <- rand 10 arbitrary
  if (name1 /= name2 && name1 /= name2 && name2 /= name3)
   then do
    company <- addNewCompany
    author <- addNewRandomUser
    time <- getMinutesTime
    let actor = systemActor time
    _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
    Just author' <- dbQuery $ GetUserByID (userid author)
    did <- addRandomDocumentWithAuthor author'

    _ <- dbUpdate $ SetDocumentTags did (S.fromList [DocumentTag name1 value1, DocumentTag name2 value2]) actor
    docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name1 value1]] [] (0,maxBound)
    docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name2 value2]] [] (0,maxBound)
    docs'' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2]] [] (0,maxBound)
    docs''' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)
    docs'''' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2, DocumentTag name3 value3]] [] (0,maxBound)

    assertEqual "Should have one document returned" [did] (map documentid docs)
    assertEqual "Should have one document returned" [did] (map documentid docs')
    assertEqual "Should have one document returned" [did] (map documentid docs'')
    assertEqual "Should have one document returned" [did] (map documentid docs''')
    assertEqual "Should have zero documents returned" [] (map documentid docs'''')
   else return ()

testStatusClassSignedWhenAllSigned :: TestEnv ()
testStatusClassSignedWhenAllSigned = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isClosed &&^ ((<=) 2 . length . (filter isSignatory) . documentsignatorylinks))
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)

  assertEqual "Statusclass for signed documents is signed" SCSigned (documentstatusclass doc')

runScheduler :: MonadIO m => ActionQueueT (AWS.AmazonMonadT m) SchedulerData a -> m a
runScheduler m = do
  let appConf = confDefault { dbConfig = "" }
  templates <- liftIO $ newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
  filecache <- MemCache.new BS.length 52428800
  CronEnv.runScheduler appConf filecache templates m
