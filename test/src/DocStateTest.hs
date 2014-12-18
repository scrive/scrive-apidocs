module DocStateTest{- (docStateTests)-} where

import Control.Arrow (first)
import Control.Concurrent (newMVar)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Functor
import Data.List
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit.Base (Assertion)
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Set as S

import ActionQueue.Monad (ActionQueueT)
import ActionQueue.Scheduler (SchedulerData(..))
import AppConf (AppConf(dbConfig))
import Company.Model
import Context (ctxtime)
import Control.Logic
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.Action (findAndDoPostDocumentClosedActions, findAndExtendDigitalSignatures)
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentMonad (DocumentT, theDocument, theDocumentID, withDocumentM, withDocument, withDocumentID)
import Doc.DocUtils
import Doc.Model
import Doc.SealStatus (SealStatus(..), hasGuardtimeSignature)
import Doc.SignatoryFieldID
import Doc.TestInvariants
import EvidenceLog.Model
import EvidenceLog.View (getSignatoryIdentifierMap, simplyfiedEventText)
import File.FileID
import MinutesTime
import Templates (getTemplatesModTime, readGlobalTemplates)
import TestingUtil
import TestKontra
import User.Model
import Util.Actor
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Default
import qualified Amazon as AWS
import qualified CronEnv
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified MemCache

docStateTests :: TestEnvSt -> Test
docStateTests env = testGroup "DocState" [
  dataStructureProperties,
  testThat "Document with seal status Missing gets sealed" env testSealMissingSignatures,
  testThat "Document with extensible digital signature can be extended" env testExtendDigitalSignatures,
  testThat "RejectDocument adds to the log" env testRejectDocumentEvidenceLog,
  testThat "RestartDocument adds to the log" env testRestartDocumentEvidenceLog,
  testThat "SignDocument adds to the log" env testSignDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" env testTimeoutDocumentEvidenceLog,
  testThat "ProlongDocument can be executed and adds to a log" env testProlongDocument,

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
  testThat "SignWithELegFailure adds to the log" env testSignWithELegFailureEvidenceDocumentEvidenceLog,

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

  testThat "ChangeAuthenticationMethod works and evidence is as expected" env testChangeAuthenticationMethod
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
  runScheduler $ findAndDoPostDocumentClosedActions Nothing
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  unless (hasGuardtimeSignature doc') $ do
    assertFailure $ "Unexpected seal status: " ++ show (documentsealstatus doc')

testExtendDigitalSignatures :: TestEnv ()
testExtendDigitalSignatures = do
  author <- addNewRandomUser
  let filename = "test/pdfs/extensible.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent
  file1 <- addNewFile filename filecontent
  file2 <- addNewFile filename filecontent
  addRandomDocumentWithAuthorAndConditionAndFile author (isSignable &&^ isClosed) file `withDocumentM` do
    now <- currentTime
    let actor = systemActor (2 `monthsBefore` now)
    -- Append a file to tweak the modification time
    dbUpdate $ AppendSealedFile file1 Guardtime{ extended = False, private = False } actor
    dbUpdate $ AppendExtendedSealedFile file2 Guardtime{ extended = False, private = False } actor
    runScheduler findAndExtendDigitalSignatures
    documentsealstatus <$> theDocument >>= \case
      Just (Guardtime{ extended = True }) -> assertSuccess
      s -> assertFailure $ "Unexpected extension status: " ++ show s


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
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks)) `withDocumentM` do
    Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
    randomUpdate $ \m t->RejectDocument (signatorylinkid sl) m (systemActor t)

    lg <- dbQuery . GetEvidenceLog  =<< theDocumentID
    assertJust $ find (\e -> evType e == Current RejectDocumentEvidence) lg

testRestartDocumentEvidenceLog :: TestEnv ()
testRestartDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  withDocument doc $ randomUpdate $ \t->CancelDocument (systemActor t)
  cdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  mdoc <- randomUpdate $ \t->RestartDocument cdoc (systemActor t)
  assertJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromJust mdoc)
  assertJust $ find (\e -> evType e == Current RestartDocumentEvidence) lg
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg
  lg2 <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg2

getScreenshots :: (MonadIO m, MonadDB m, MonadThrow m) => m SignatoryScreenshots.SignatoryScreenshots
getScreenshots = do
  now <- currentTime
  first_ <- liftIO $ BS.readFile "test/screenshots/s1.jpg"
  signing <- liftIO $ BS.readFile "test/screenshots/s2.jpg"
  let mkss i = Just $ Screenshot.Screenshot{ Screenshot.time = now
                                           , Screenshot.image = Binary i
                                           }
  return $ SignatoryScreenshots.emptySignatoryScreenshots{ SignatoryScreenshots.first = mkss first_
                                     , SignatoryScreenshots.signing = mkss signing
                                     }

testSignDocumentEvidenceLog :: TestEnv ()
testSignDocumentEvidenceLog = do
  author <- addNewRandomUser
  screenshots <- getScreenshots
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks) &&^ (all ((==) StandardAuthentication . signatorylinkauthenticationmethod) . documentsignatorylinks)) `withDocumentM` do
    Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
    randomUpdate $ \t->MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
    randomUpdate $ \t->SignDocument  (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing screenshots (systemActor t)

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current SignDocumentEvidence) lg

testTimeoutDocumentEvidenceLog :: TestEnv ()
testTimeoutDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
    success <- randomUpdate $ \t->TimeoutDocument (systemActor t)
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current TimeoutDocumentEvidence) lg



testProlongDocument :: TestEnv ()
testProlongDocument = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
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
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation &&^ ((<=) 2 . length . documentsignatorylinks)) `withDocumentM` do
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ \t->PreparationToPending (systemActor t) tz

    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current PreparationToPendingEvidence) lg

testMarkInvitationReadEvidenceLog :: TestEnv ()
testMarkInvitationReadEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
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
    let expectedFull = "Scrive E-sign’s external email delivery system reported that the invitation to "
                ++ (if signatoryispartner sl then "sign" else "review")
                ++ " (sent to " ++ getEmail sl ++ ") was opened."
    assertEqual "Correct event text full" (expectedFull) (evText e)
    let expectedSimple = "The invitation to "
                ++ (if signatoryispartner sl then "sign" else "review")
                ++ " the document (sent to " ++ getEmail sl ++ ") was opened."
    sim <- getSignatoryIdentifierMap True [e]
    simpletext <- theDocument >>= \d -> simplyfiedEventText EventForVerificationPages (Just "author") d{ documentlang = LANG_EN } sim e
    assertEqual "Correct simplified event text" (expectedSimple) simpletext

testSaveSigAttachmentEvidenceLog :: TestEnv ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ isSignable) `withDocumentM` do
    file <- addNewRandomFile
    let sa = SignatoryAttachment { signatoryattachmentfile        = Nothing
                                 , signatoryattachmentname        = "attachment"
                                 , signatoryattachmentdescription = "gimme!"
                                 }
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
                                                  &&^ isPreparation
                                                  &&^ ((all (isSignatory &&^ not .hasSigned &&^ (==) StandardAuthentication . signatorylinkauthenticationmethod)) . documentsignatorylinks)
                                                  &&^ (((==) 2) . length .documentsignatorylinks)) `withDocumentM` do
    file <- addNewRandomFile
    sl <- (\d -> (documentsignatorylinks d) !! 1) <$> theDocument
    let sa = SignatoryAttachment { signatoryattachmentfile        = Nothing
                                 , signatoryattachmentname        = "attachment"
                                 , signatoryattachmentdescription = "gimme!"
                                 }
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
    let sa = SignatoryAttachment { signatoryattachmentfile        = Just file
                                               , signatoryattachmentname        = "attachment"
                                               , signatoryattachmentdescription = "gimme!"
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
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
    randomUpdate $ \t-> CancelDocument (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current CancelDocumentEvidence) lg

testSignWithELegFailureEvidenceDocumentEvidenceLog :: TestEnv ()
testSignWithELegFailureEvidenceDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
    Just sl <- getAuthorSigLink <$> theDocument
    randomUpdate $ \t-> LogSignWithELegFailureForDocument (signatorylinkid sl) Nothing Nothing "first" "last" "198404011234" (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current SignWithELegFailureEvidence) lg

testChangeSignatoryEmailWhenUndeliveredEvidenceLog :: TestEnv ()
testChangeSignatoryEmailWhenUndeliveredEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks)) `withDocumentM` do
    Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument
    success <- randomUpdate $ \t-> ChangeSignatoryEmailWhenUndelivered (signatorylinkid sl) Nothing "email@email.com" (systemActor t)
    assert success
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current ChangeSignatoryEmailWhenUndeliveredEvidence) lg

testCloseDocumentEvidenceLog :: TestEnv ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (all ((==) StandardAuthentication . signatorylinkauthenticationmethod) . documentsignatorylinks)) `withDocumentM` do
    documentsignatorylinks <$> theDocument >>= \sls -> forM_  sls $ \sl -> when (isSignatory sl) $ do
      randomUpdate $ \t->MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
      randomUpdate $ \t->SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)
    randomUpdate $ \t-> CloseDocument (systemActor t)
    lg <- dbQuery . GetEvidenceLog =<< theDocumentID
    assertJust $ find (\e -> evType e == Current CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> TestEnv (User, UTCTime, Document)
performNewDocumentWithRandomUser mcompany doctype title = do
  user <- maybe addNewRandomUser (\c -> addNewRandomCompanyUser (companyid c) False) mcompany
  ctx <- mkContext defaultValue
  let aa = authorActor ctx user
  doc <- randomUpdate $ NewDocument defaultValue user title doctype defaultTimeZoneName 0 aa
  return (user, ctxtime ctx, doc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> (User, UTCTime, Document) -> TestEnv ()
assertGoodNewDocument mcompany doctype title (user, time, doc) = do
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
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition user (isSignable &&^ isPending) `withDocumentM` do
    doc <- theDocument
    randomUpdate $ CancelDocument (authorActor ctx user)

    canceleddoc <- theDocument
    assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
    assertEqual "Updated modification time" (ctxtime ctx) (documentmtime canceleddoc)
    assertEqual "Siglinks are unchanged"
                    (documentsignatorylinks doc)
                    (documentsignatorylinks canceleddoc)
    assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: TestEnv ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ not . isPending)
  ctx <- mkContext defaultValue
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $
               withDocument doc $ randomUpdate $ CancelDocument (authorActor ctx user)

testSignatories1 :: Assertion
testSignatories1 =
  let s1 = defaultValue { signatoryfields =
              [ SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Eric" True False []
              , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Normand" True False []
              ]}
      s2 = defaultValue { signatoryfields =
              [ SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Normand" True False []
              , SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Eric" True False []
              ]}
  in assertBool "Signatories fields should be equal" (s1 == s2)

propSignatoryDetailsEq :: SignOrder -> SignatoryLink -> Property
propSignatoryDetailsEq o1 sd =
   (o1 == o1) ==> sd{signatorysignorder = o1} == sd{signatorysignorder = o1}

propSignatoryDetailsNEq :: SignOrder -> SignOrder -> SignatoryLink -> Property
propSignatoryDetailsNEq o1 o2 sd =
  (o1 /= o2) ==> sd{signatorysignorder = o1} /= sd{signatorysignorder = o2}


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
testArchiveDocumentPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  _doc0 <- addRandomDocumentWithAuthorAndCondition author isPending
  _doc1 <- addRandomDocumentWithAuthorAndCondition author isPending

  let doc = _doc1

  assertRaisesKontra (\(DocumentStatusShouldBe {}) -> True) $
               withDocument doc $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)


testArchiveDocumentAuthorRight :: TestEnv ()
testArchiveDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testArchiveDocumentCompanyAdminRight :: TestEnv ()
testArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid adminuser) (systemActor t)
    assertOneArchivedSigLink =<< theDocument

testRestoreArchivedDocumentAuthorRight :: TestEnv ()
testRestoreArchivedDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument author (systemActor t)
    assertNoArchivedSigLink =<< theDocument

testRestoreArchiveDocumentCompanyAdminRight :: TestEnv ()
testRestoreArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
    randomUpdate $ \t->RestoreArchivedDocument adminuser (systemActor t)

    assertNoArchivedSigLink =<< theDocument

testChangeAuthenticationMethod :: TestEnv ()
testChangeAuthenticationMethod = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (
      isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks)
      &&^ ( all ((==) StandardAuthentication . signatorylinkauthenticationmethod) . documentsignatorylinks)
    ) `withDocumentM` do
      Just sl <- getSigLinkFor (not . (isAuthor::SignatoryLink->Bool)) <$> theDocument

      randomUpdate $ \t->ChangeAuthenticationMethod (signatorylinkid sl) SMSPinAuthentication Nothing (systemActor t)
      lg1 <- dbQuery . GetEvidenceLog  =<< theDocumentID
      assertJust $ find (\e -> evType e == Current ChangeAuthenticationMethodStandardToSMSEvidence) lg1
      assertNothing $ find (\e -> evType e == Current UpdateFieldMobileEvidence) lg1

      randomUpdate $ \t->ChangeAuthenticationMethod (signatorylinkid sl) SMSPinAuthentication (Just "+486543222112") (systemActor t)
      lg2 <- dbQuery . GetEvidenceLog  =<< theDocumentID
      assertEqual "Too many evidence logs for change authentication method"
        (length $ filter (\e -> evType e == Current ChangeAuthenticationMethodStandardToSMSEvidence) lg2) 1
      assertJust $ find (\e -> evType e == Current UpdateFieldMobileEvidence) lg2

--------------------------------------------------------------------------------
testReallyDeleteDocument :: TestEnv ()
testReallyDeleteDocument = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

  assertRaisesKontra (\DocumentIsNotDeleted {} -> True) $
    withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  withDocument doc $ randomUpdate $ \t->ArchiveDocument (userid author) (systemActor t)
  withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  assertRaisesKontra (\DocumentIsReallyDeleted {} -> True) $
    withDocument doc $ randomUpdate $ \t->ReallyDeleteDocument (userid author) (systemActor t)
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByDocumentID (documentid doc)]
                     [] (0,1)
  assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)


testReallyDeleteDocumentCompanyAdmin :: TestEnv ()
testReallyDeleteDocumentCompanyAdmin = doTimes 10 $ do
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
    docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByDocumentID (documentid doc)]
                       [] (0,1)
    assertEqual "Really deleted documents are not visible to user" [] (map documentid docs)

testReallyDeleteDocumentSomebodyElse :: TestEnv ()
testReallyDeleteDocumentSomebodyElse = doTimes 10 $ do
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
testPurgeDocument = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  now <- currentTime
  archived1 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged zero documents when not deleted" 0 archived1
  withDocument doc $ randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
  archived2 <- dbUpdate $ PurgeDocuments 0 0
  assertEqual "Purged single document" 1 archived2

  allDocs1 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                         [DocumentFilterByDocumentID (documentid doc)] [] (0,-1)
  assertEqual "List documents does not include purged ones" [] (map documentid allDocs1)

testPurgeDocumentUserSaved :: TestEnv ()
testPurgeDocumentUserSaved = doTimes 10 $ do
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
testPurgeDocumentActiveSignLink = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (isClosed &&^ (not . null . filter (isSignatory &&^ (not . isAuthor)). documentsignatorylinks)) `withDocumentM` do
    now <- currentTime
    randomUpdate $ \t -> ArchiveDocument (userid author) ((systemActor t) { actorTime = now })
    updateMTimeAndObjectVersion now
    archived <- dbUpdate $ PurgeDocuments 0 1
    assertEqual "Purged zero documents" 0 archived

testArchiveIdleDocument :: TestEnv ()
testArchiveIdleDocument = doTimes 10 $ do
  company <- addNewCompany
  _ <- dbUpdate $ SetCompanyInfo (companyid company) ((companyinfo company){ companyidledoctimeout = Just 1 })
  author <- addNewRandomCompanyUser(companyid company) False
  author2 <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isClosed &&^ isSignable)
  _ <- addRandomDocumentWithAuthorAndCondition author (isTemplate ||^ isPending)
  _ <- addRandomDocumentWithAuthorAndCondition author2 (isClosed &&^ isSignable)
  archived1 <- dbUpdate $ ArchiveIdleDocuments (documentmtime doc)
  assertEqual "Archived zero idle documents (too early)" 0 archived1
  archived2 <- dbUpdate $ ArchiveIdleDocuments (2 `daysAfter` documentmtime doc)
  assertEqual "Archived idle documents for one signatory" 1 archived2

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: TestEnv ()
testArchiveDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    unrelateduser <- addNewRandomUser
    assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True) $
      randomUpdate $ \t -> ArchiveDocument (userid unrelateduser) (systemActor t)

testArchiveDocumentCompanyStandardLeft :: TestEnv ()
testArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    assertRaisesKontra (\UserShouldBeSelfOrCompanyAdmin {} -> True) $
      randomUpdate $ \t->ArchiveDocument (userid standarduser) (systemActor t)

testRestoreArchivedDocumentUnrelatedUserLeft :: TestEnv ()
testRestoreArchivedDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d) `withDocumentM` do
    unrelateduser <- addNewRandomUser
    randomUpdate $ \t -> ArchiveDocument (userid author) (systemActor t)
    assertRaisesKontra (\UserShouldBeDirectlyOrIndirectlyRelatedToDocument {} -> True)$ do
      randomUpdate $ \t->RestoreArchivedDocument unrelateduser (systemActor t)

testRestoreArchiveDocumentCompanyStandardLeft :: TestEnv ()
testRestoreArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
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
checkQueryDoesntContainArchivedDocs qry = doTimes 10 $ do
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
testSetDocumentLangNotLeft = doTimes 10 $ do
  d <- rand 10 arbitrary
  success <- withDocumentID d $ randomUpdate $ \l t -> SetDocumentLang l (systemActor t)
  assert $ not success

testNewDocumentDependencies :: TestEnv ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  -- execute
  ctx <- mkContext defaultValue
  let aa = authorActor ctx author
  doc <- randomUpdate $ (\title doctype -> NewDocument defaultValue author (fromSNN title) doctype defaultTimeZoneName 0 aa)
  -- assert
  assertInvariants doc

testDocumentCanBeCreatedAndFetchedByID :: TestEnv ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  let aa = authorActor ctx author
  doc <- randomUpdate $ (\title doctype -> NewDocument defaultValue author (fromSNN title) doctype defaultTimeZoneName 0 aa)
  -- execute
  ndoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  -- assert

  assert $ documentid doc == documentid ndoc
  assertInvariants ndoc

testDocumentAttachNotPreparationLeft :: TestEnv ()
testDocumentAttachNotPreparationLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    withDocument doc $ randomUpdate $ \t->AttachFile file (systemActor t)

testDocumentAttachPreparationRight :: TestEnv ()
testDocumentAttachPreparationRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    --execute
    randomUpdate $ \t -> AttachFile file (systemActor t)

    --assert
    assertInvariants =<< theDocument


testNoDocumentAttachAlwaysLeft :: TestEnv ()
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
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
testDocumentAttachHasAttachment = doTimes 10 $ do
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
testNoDocumentAppendSealedAlwaysLeft = doTimes 10 $ do
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

testDocumentAppendSealedPendingRight :: TestEnv ()
testDocumentAppendSealedPendingRight = doTimes 10 $ do
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
    nfile <- documentsealedfile <$> theDocument
    assertBool "Should have new file attached, but it's not" $ Just file == nfile


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
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    mt <- rand 10 arbitrary
    sf <- signatoryFieldsFromUser author
    --execute
    success <- dbUpdate $ ResetSignatoryDetails [defaultValue { signatoryfields = sf}] (systemActor mt)
    --assert
    assert $ not success

testPreparationResetSignatoryDetailsAlwaysRight :: TestEnv ()
testPreparationResetSignatoryDetailsAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    mt <- rand 10 arbitrary
    --execute
    success <- dbUpdate $ ResetSignatoryDetails [defaultValue { signatoryisauthor = True , maybesignatory = Just $ userid author}] (systemActor mt)
    --assert
    assert success
    assertInvariants =<< theDocument

testPreparationResetSignatoryDetails2Works :: TestEnv ()
testPreparationResetSignatoryDetails2Works = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    mt <- rand 10 arbitrary
    --execute
    let newData1 = defaultValue {   signatoryisauthor = True , maybesignatory = Just $ userid author}
    success1 <- dbUpdate $ ResetSignatoryDetails [newData1] (systemActor mt)
    assert success1
    assertInvariants =<< theDocument
    sls <- documentsignatorylinks <$> theDocument
    assertEqual "Proper delivery method set" [EmailDelivery] (map signatorylinkdeliverymethod sls)
    assertEqual "Proper authentication method set" [StandardAuthentication] (map signatorylinkauthenticationmethod sls)

    let newData2 =  defaultValue { signatoryisauthor = True, maybesignatory = Just $ userid author , signatorylinkdeliverymethod = PadDelivery, signatorylinkauthenticationmethod = ELegAuthentication }
    success2 <- dbUpdate $ ResetSignatoryDetails [newData2] (systemActor mt)
    assert success2
    sls2 <- documentsignatorylinks <$> theDocument
    assertEqual "Proper delivery method set" [PadDelivery] (map signatorylinkdeliverymethod sls2)
    assertEqual "Proper authentication method set" [ELegAuthentication] (map signatorylinkauthenticationmethod sls2)
    assertInvariants =<< theDocument

testNoDocumentResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNoDocumentResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  --author <- addNewRandomUser
  mt <- rand 10 arbitrary
  --execute
  -- non-existent docid

  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID a $ dbUpdate $ ResetSignatoryDetails [defaultValue { signatoryisauthor = True } ] (systemActor mt)



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

  success <- withDocument doc1 $ dbUpdate $ SetDocumentTitle title (actor)
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
           _ <- withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  newuser <- addNewRandomUser

  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor defaultValue newuser doc (systemActor mt))
  _ <- withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (systemActor mt)

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
           _ <- withDocumentID docid $ dbUpdate $ TemplateFromDocument (systemActor mt)
           dbQuery $ GetDocumentByDocumentID docid
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  docid' <- fromJust <$> (dbUpdate $ CloneDocumentWithUpdatedAuthor defaultValue user' doc (systemActor mt))
  _ <- withDocumentID docid' $ dbUpdate $ DocumentFromTemplate (systemActor mt)
  doc' <- dbQuery $ GetDocumentByDocumentID docid'
  let [author] = filter isAuthor $ documentsignatorylinks doc'
  assertEqual "Author signatory link company name is not same as his company" (getCompanyName company) (getCompanyName author)



testAddDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testAddDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    file <- addNewRandomFile
    --execute
    success <- randomUpdate $ \t->AddDocumentAttachment file (systemActor t)
    --assert
    assert $ not success

testAddDocumentAttachmentOk :: TestEnv ()
testAddDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    file <- addNewRandomFile
    --execute

    success <- randomUpdate $ \t->AddDocumentAttachment file (systemActor t)

    --assert
    assert success
    assertEqual "Author attachment was really attached" [file]
                    . map authorattachmentfile . documentauthorattachments =<< theDocument

testRemoveDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testRemoveDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isPreparation) `withDocumentM` do
    --execute
    success <- randomUpdate $ \t -> RemoveDocumentAttachment (unsafeFileID 0) (systemActor t)
    --assert
    assert $ not success

testRemoveDocumentAttachmentOk :: TestEnv ()
testRemoveDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
    --execute
    success <- randomUpdate $ \t -> RemoveDocumentAttachment (unsafeFileID 0) (systemActor t)
    --assert
    assert $ not success

---------------------------------------------------------------------

testUpdateSigAttachmentsAttachmentsOk :: TestEnv ()
testUpdateSigAttachmentsAttachmentsOk = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author isPreparation `withDocumentM` do
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
    (time, sl) <- rand 10 arbitrary
    let sa = signatoryActor ctx{ ctxtime = time } sl
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
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    withDocument doc $ dbUpdate $ TimeoutDocument (systemActor mt)

testTimeoutDocumentSignableNotPendingLeft :: TestEnv ()
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    withDocument doc $ randomUpdate $ \t->TimeoutDocument (systemActor t)

testTimeoutDocumentSignablePendingRight :: TestEnv ()
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
    --execute
    randomUpdate $ \t->TimeoutDocument (systemActor t)
    assertInvariants =<< theDocument

testTimeoutDocumentSignableNotLeft :: TestEnv ()
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  actor <- arbitrarySystemActor
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    d <- rand 10 arbitrary
    withDocument d $ randomUpdate $ TimeoutDocument actor

testSignDocumentNonSignableLeft :: TestEnv ()
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) (signatorymagichash sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignableNotPendingLeft :: TestEnv ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending)) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ \si t -> SignDocument (signatorylinkid sl) (signatorymagichash sl) si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testSignDocumentSignablePendingRight :: TestEnv ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^
           any ((== StandardAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) . documentsignatorylinks) `withDocumentM` do
    Just sl <- find ((== StandardAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) .documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary
    randomUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
    randomUpdate $ SignDocument (signatorylinkid sl) (signatorymagichash sl) Nothing Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentSignablePendingElegRight :: TestEnv ()
testSignDocumentSignablePendingElegRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^
           any ((== ELegAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) . documentsignatorylinks) `withDocumentM` do
    Just sl <- find ((== ELegAuthentication) . signatorylinkauthenticationmethod &&^ isSignatory &&^ (not . hasSigned)) . documentsignatorylinks <$> theDocument
    time <- rand 10 arbitrary
    randomUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
    randomUpdate $ \signinfo -> SignDocument (signatorylinkid sl) (signatorymagichash sl) (Just signinfo) Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor time)

testSignDocumentNotLeft :: TestEnv ()
testSignDocumentNotLeft = doTimes 10 $ do
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    -- our machinery is broken here, baseline condition has only relations
    -- this should be ignored and properly return info about non existing document
    d <- rand 10 arbitrary
    withDocument d $ randomUpdate $ \sl mh si t -> SignDocument sl mh si Nothing SignatoryScreenshots.emptySignatoryScreenshots (systemActor t)

testPreparationToPendingNotSignableLeft :: TestEnv ()
testPreparationToPendingNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         } `withDocumentM` do
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      tz <- mkTimeZoneName "Europe/Stockholm"
      randomUpdate $ PreparationToPending (systemActor time) tz

testPreparationToPendingSignableNotPreparationLeft :: TestEnv ()
testPreparationToPendingSignableNotPreparationLeft = doTimes 10 $ do
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
testPreparationToPendingNotLeft = doTimes 100 $ do
  (time, did) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    tz <- mkTimeZoneName "Europe/Stockholm"
    withDocumentID did $ randomUpdate $ PreparationToPending (systemActor time) tz

testPreparationToPendingSignablePreparationRight :: TestEnv ()
testPreparationToPendingSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Preparation]
         , randomDocumentCondition = (any isSignatory . documentsignatorylinks) &&^
          (isJust . documentfile) &&^
          ((==) 1 . length . filter isAuthor . documentsignatorylinks)
         } `withDocumentM` do
    time <- rand 10 arbitrary
    tz <- mkTimeZoneName "Europe/Stockholm"
    randomUpdate $ PreparationToPending (systemActor time) tz
    assertInvariants =<< theDocument

testRejectDocumentNotSignableLeft :: TestEnv ()
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) Nothing
             (authorActor ctx{ ctxtime = time } author)

testRejectDocumentSignableNotPendingLeft :: TestEnv ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ not . isPending) `withDocumentM` do
    Just sl <- getSigLinkFor author <$> theDocument
    time <- rand 10 arbitrary
    assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
      randomUpdate $ RejectDocument (signatorylinkid sl) Nothing
             (authorActor ctx{ ctxtime = time } author)

testRejectDocumentNotLeft :: TestEnv ()
testRejectDocumentNotLeft = doTimes 10 $ do
  _ <- addRandomDocument . randomDocumentAllowsDefault =<< addNewRandomUser
  ctx <- mkContext defaultValue
  (did, time, sl) <- rand 10 arbitrary
  let sa = signatoryActor ctx{ ctxtime = time } sl
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    withDocumentID did $ randomUpdate . RejectDocument (signatorylinkid sl) Nothing =<< sa

testRejectDocumentSignablePendingRight :: TestEnv ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending) `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . filter (signatoryispartner) . documentsignatorylinks =<< theDocument
    Just sl <- getSigLinkFor slid <$> theDocument
    time <- rand 10 arbitrary
    let sa = signatoryActor ctx{ ctxtime = time } sl
    randomUpdate . RejectDocument slid Nothing =<< sa

    assertInvariants =<< theDocument

testMarkInvitationRead :: TestEnv ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks)) `withDocumentM` do

    sl' <- rand 10 . elements . documentsignatorylinks =<< theDocument
    let slid = signatorylinkid sl'
    time <- currentTime
    success <- dbUpdate . MarkInvitationRead slid =<< signatoryActor ctx{ ctxtime = time } sl'

    assert success
    Just sl <- getSigLinkFor slid <$> theDocument
    assertEqual "Invitation read time should be set." (Just time) (maybereadinvite sl)

testMarkInvitationReadDocDoesntExist :: TestEnv ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  ctx <- mkContext defaultValue
  (did, sl, time) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist{} -> True) $ do
    _ <- withDocumentID did $ randomUpdate . MarkInvitationRead (signatorylinkid sl)
            =<< signatoryActor ctx{ ctxtime = time } sl
    return ()
  return ()

testMarkDocumentSeenNotSignableLeft :: TestEnv ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         } `withDocumentM` do

    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor ctx{ ctxtime = time } sl
        assertRaisesKontra (\DocumentTypeShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa

testMarkDocumentSeenClosedOrPreparationLeft :: TestEnv ()
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         } `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (isNothing $ maybeseeninfo sl) $ do
        time <- rand 10 arbitrary
        let sa = signatoryActor ctx{ ctxtime = time } sl
        assertRaisesKontra (\DocumentStatusShouldBe{} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa

testMarkDocumentSeenNotLeft :: TestEnv ()
testMarkDocumentSeenNotLeft = doTimes 10 $ do
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
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation))) `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
                when (not $ hasSeen sl) $ do
                  time <- rand 10 arbitrary
                  let sa = signatoryActor ctx{ ctxtime = time } sl
                  randomUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< sa
                  Just tsl <- getSigLinkFor (signatorylinkid sl) <$> theDocument
                  assertBool "Signatorylink should be marked seen now." (hasSeen tsl)

testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation))) `withDocumentM` do
    (theDocument >>=) $ forEachSignatoryLink $ \sl ->
      when (not $ hasSeen sl) $ do
        mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
        time <- rand 10 arbitrary
        let sa = signatoryActor ctx{ ctxtime = time } sl
        assertRaisesKontra (\SignatoryTokenDoesNotMatch {} -> True) $ do
          randomUpdate . MarkDocumentSeen (signatorylinkid sl) mh =<< sa

testSetInvitationDeliveryStatusNotSignableLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author (not . isSignable) `withDocumentM` do
    actor <- arbitrarySystemActor
    Just sl <- getAuthorSigLink <$> theDocument
    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
      success <- randomUpdate $ \st-> SetEmailInvitationDeliveryStatus (signatorylinkid sl) st actor
      assert $ not success


testSetInvitationDeliveryStatusNotLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  actor <- arbitrarySystemActor
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    d <- rand 10 arbitrary
    success <- withDocumentID d $ randomUpdate $ \s st-> SetEmailInvitationDeliveryStatus s st actor
    assert $ not success

testSetInvitationDeliveryStatusSignableRight :: TestEnv ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocumentWithAuthorAndCondition author isSignable `withDocumentM` do
    slid <- rand 10 . elements . map signatorylinkid . documentsignatorylinks =<< theDocument
    st <- rand 10 arbitrary
    actor <- arbitrarySystemActor
    success <- randomUpdate $ SetEmailInvitationDeliveryStatus slid st actor
    assert success

testSetDocumentTagsRight :: TestEnv ()
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  addRandomDocumentWithAuthor' author `withDocumentM` do
    (tags, time) <- first S.fromList <$> rand 10 arbitrary
    let actor = authorActor ctx{ ctxtime = time } author
    success <- randomUpdate $ SetDocumentTags tags actor

    assert success
    assertEqual "Tags should be equal" tags . documenttags =<< theDocument

testCloseDocumentSignableButNotEverybodyHasSigned :: TestEnv ()
testCloseDocumentSignableButNotEverybodyHasSigned = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = (\doc -> length (documentsignatorylinks doc) > 1) &&^
                                     (not . all (isSignatory =>>^ hasSigned) . documentsignatorylinks)
         } `withDocumentM` do
    sa <- arbitrarySystemActor
    assertRaisesKontra (\(SignatoryHasNotYetSigned {}) -> True) $ do
      randomUpdate $ CloseDocument sa

testCloseDocumentNotSignableNothing :: TestEnv ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         } `withDocumentM` do
    sa <- arbitrarySystemActor
    assertRaisesKontra (\(DocumentTypeShouldBe {}) -> True) $ do
      randomUpdate $ CloseDocument sa

testCloseDocumentNotNothing :: TestEnv ()
testCloseDocumentNotNothing = doTimes 10 $ do
  sa <- arbitrarySystemActor
  did <- rand 10 arbitrary
  assertRaisesKontra (\(DocumentDoesNotExist {}) -> True) $ do
    withDocumentID did $ randomUpdate $ CloseDocument sa

testCancelDocumentNotSignableNothing :: TestEnv ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  ctx <- mkContext defaultValue
  time <- rand 10 arbitrary
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         } `withDocumentM` do

    assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $
                 randomUpdate $ CancelDocument
                                (authorActor ctx{ ctxtime = time } author)

testCancelDocumentNotNothing :: TestEnv ()
testCancelDocumentNotNothing = doTimes 10 $ do
  aa <- arbitraryAuthorActor
  did <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $
             withDocumentID did $ randomUpdate $ CancelDocument aa

testSetDocumentTitleNotLeft :: TestEnv ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  (did, StringNoNUL title) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- withDocumentID did $ randomUpdate $ SetDocumentTitle title actor
  assert $ not success

testSetDocumentTitleRight :: TestEnv ()
testSetDocumentTitleRight = doTimes 10 $ do
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
testSetDocumentDaysToSignNotLeft = doTimes 10 $ do
  (did, d) <- rand 10 arbitrary
  actor <- arbitraryAuthorActor
  success <- withDocumentID did $ randomUpdate $ SetDaysToSign d actor
  assert $ not success

testSetDocumentDaysToSignRight :: TestEnv ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomUser
  actor <- arbitraryAuthorActor
  addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = not . isClosed
         } `withDocumentM` do
    let daystosign = 15
    success1 <- randomUpdate $ SetDaysToSign daystosign actor

    assert success1
    assertEqual "Days to sign is set properly" daystosign . documentdaystosign =<< theDocument

assertInvariants :: (MonadIO m, MonadDB m, MonadThrow m) => Document -> m ()
assertInvariants document = do
  now <- currentTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a

testGetDocumentsByCompanyWithFilteringCompany :: TestEnv ()
testGetDocumentsByCompanyWithFilteringCompany = doTimes 10 $ do
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
    docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)

    assertEqual "Should have 1 document returned" 1 (length docs')


testGetDocumentsByCompanyWithFilteringFilters :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFilters = doTimes 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
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
  withDocumentID did $ do
    isdraft <- (isSignable &&^ isPreparation) <$> theDocument

    docs1 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
    _ <- dbUpdate $ SetDocumentUnsavedDraft True
    docs2 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
    _ <- dbUpdate $ SetDocumentUnsavedDraft False
    docs3 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                       [DocumentFilterUnsavedDraft False, DocumentFilterByDocumentID did] [] (0,maxBound)
    docs4 <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)]
                       [DocumentFilterUnsavedDraft True, DocumentFilterByDocumentID did] [] (0,maxBound)

    assertEqual "Should return the document" [did] (map documentid docs1)
    assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs2)
    assertEqual "Should return the document" [did] (map documentid docs3)
    assertEqual "Should return no documents" ([] <| isdraft |>[did])    (map documentid docs4)


testGetDocumentsByCompanyWithFilteringFinds :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFinds = doTimes 10 $ do
  (StringNoNUL name, StringNoNUL value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (companyid company)
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- currentTime
  let actor = systemActor time
  _ <- withDocumentID did $ dbUpdate $ SetDocumentTags (S.singleton $ DocumentTag name value) actor
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [DocumentFilterByTags [DocumentTag name value]] [] (0,maxBound)
  docs' <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid author)] [] [] (0,maxBound)

  assertEqual "Should have one document returned" [did] (map documentid docs)
  assertEqual "Should have one document returned" [did] (map documentid docs')

testGetDocumentsByCompanyWithFilteringFindsMultiple :: TestEnv ()
testGetDocumentsByCompanyWithFilteringFindsMultiple = doTimes 10 $ do
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
  let appConf = defaultValue { dbConfig = "" }
  templates <- liftIO $ newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
  filecache <- MemCache.new BS.length 52428800
  CronEnv.runScheduler appConf filecache templates m
