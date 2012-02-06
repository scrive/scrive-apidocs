{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import DB.Classes
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MinutesTime
import Test.HUnit.Base (Assertion)
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Data.ByteString.UTF8 as BS

import Data.Functor
import Data.Maybe
import Data.Convertible(convert)
import Database.HDBC(SqlValue)
import DB.Nexus
import Control.Monad
import Control.Monad.Trans
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import File.FileID

import qualified Log

import EvidenceLog.Model

docStateTests :: Nexus -> Test
docStateTests conn = testGroup "DocState" [
  dataStructureProperties,
  
  testThat "ReallyDeleteDocument adds to the log" conn testReallyDeleteDocumentEvidenceLog,
  testThat "RejectDocument adds to the log" conn testRejectDocumentEvidenceLog,
  testThat "RemoveDaysToSign adds to the log" conn testRemoveDaysToSignEvidenceLog,
  testThat "RemoveDocumentAttachment adds to the log" conn testRemoveDocumentAttachmentEvidenceLog,
  testThat "ResetSignatoryDetails adds to the log" conn testResetSignatoryDetailsEvidenceLog,
  testThat "RestartDocument adds to the log" conn testRestartDocumentEvidenceLog,
  testThat "RestoreArchivedDocument adds to the log" conn testRestoreArchivedDocumentEvidenceLog,
  testThat "SaveDocumentForUser adds to the log" conn testSaveDocumentForUserEvidenceLog,
  testThat "SetDaysToSign adds to the log" conn testSetDaysToSignEvidenceLog,
  testThat "SetDocumentAdvancedFunctionality adds to the log" conn testSetDocumentAdvancedFunctionalityEvidenceLog,
  testThat "SetDocumentInviteTime adds to the log" conn testSetDocumentInviteTimeEvidenceLog,
  testThat "SetDocumentLocale adds to the log" conn testSetDocumentLocaleEvidenceLog,
  testThat "SetDocumentTags adds to the log" conn testSetDocumentTagsEvidenceLog,
  testThat "SetDocumentTimeoutTime adds to the log" conn testSetDocumentTimeoutTimeEvidenceLog,
  testThat "SetDocumentTitle adds to the log" conn testSetDocumentTitleEvidenceLog,
  testThat "SetDocumentUI adds to the log" conn testSetDocumentUIEvidenceLog,
  testThat "SetElegitimationIdentification adds to the log" conn testSetElegitimationIdentificationEvidenceLog,
  testThat "SetEmailIdentification adds to the log" conn testSetEmailIdentificationEvidenceLog,
  testThat "SetInvitationDeliveryStatus adds to the log" conn testSetInvitationDeliveryStatusEvidenceLog,
  testThat "SetInviteText adds to the log" conn testSetInviteTextEvidenceLog,
  testThat "SignDocument adds to the log" conn testSignDocumentEvidenceLog,
  testThat "SignableFromDocumentIDWithUpdatedAuthor adds to the log" conn testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog,
  testThat "TemplateFromDocument adds to the log" conn testTemplateFromDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" conn testTimeoutDocumentEvidenceLog,
  testThat "UpdateFields adds to the log" conn testUpdateFieldsEvidenceLog,
  
  testThat "PreparationToPending adds to the log" conn testPreparationToPendingEvidenceLog,
  testThat "PendingToAwaitingAuthor adds to the log" conn testPendingToAwaitingAuthorEvidenceLog,
  testThat "MarkInvitationRead adds to the log" conn testMarkInvitationReadEvidenceLog,
  testThat "MarkDocumentSeen adds to the log" conn testMarkDocumentSeenEvidenceLog,
  testThat "ErrorDocument adds to the log" conn testErrorDocumentEvidenceLog,
  testThat "DocumentFromSignatoryData adds to the log" conn testDocumentFromSignatoryDataEvidenceLog,
  testThat "SaveSigAttachment adds to the log" conn testSaveSigAttachmentEvidenceLog,
  testThat "UpdateSigAttachment adds to the log" conn testUpdateSigAttachmentsEvidenceLog,
  testThat "DeleteSigAttachment adds to the log" conn testDeleteSigAttachmentEvidenceLog,
  testThat "CloseDocument adds to the log" conn testCloseDocumentEvidenceLog,
  testThat "ChangeSignatoryEmailWhenUndelivered adds to the log" conn testChangeSignatoryEmailWhenUndeliveredEvidenceLog,
  testThat "ChangeMailfile adds to the log" conn testChangeMainfileEvidenceLog,
  testThat "CancelDocument adds to the log" conn testCancelDocumentEvidenceLog,
  testThat "AttachSealedFile adds to the log" conn testAttachSealedFileEvidenceLog,
  testThat "AttachFile adds to the log" conn testAttachFileEvidenceLog,
  testThat "AttachCSVUpload adds to the log" conn testAttachCSVUploadEvidenceLog,
  testThat "ArchiveDocumentEvidence adds to the log" conn testArchiveDocumentEvidenceLog,
  testThat "AddInvitationEvidence adds to the log" conn testAddInvitationEvidenceLog,
  testThat "NewDocument adds to the log" conn testNewDocumentEvidenceLog,
  testThat "AddDocumentAttachment adds to the log" conn testAddDocumentAttachmentEvidenceLog,
  testThat "GetDocumentsByCompanyAndTags filters" conn testGetDocumentsByCompanyAndTagsFilters,
  testThat "GetDocumentsByCompanyAndTags finds" conn testGetDocumentsByCompanyAndTagsFinds,
  testThat "GetDocumentsByCompanyAndTags finds with multiple" conn testGetDocumentsByCompanyAndTagsFindsMultiple,  
  testThat "GetDocumentsByCompanyAndTags finds with company filter" conn testGetDocumentsByCompanyAndTagsCompany,
  testThat "NewDocument inserts a new contract for a single user successfully" conn testNewDocumentForNonCompanyUserInsertsANewContract,
  testThat "NewDocument inserts a new contract for a company user successfully" conn testNewDocumentForACompanyUserInsertsANewContract,
  testThat "NewDocument inserts a new offer for a single user successfully" conn testNewDocumentForNonCompanyUserInsertsANewOffer,
  testThat "NewDocument inserts a new offer for a company user successfully" conn testNewDocumentForACompanyUserInsertsANewOffer,
  testThat "NewDocument inserts a new order for a single user successfully" conn testNewDocumentForNonCompanyUserInsertsANewOrder,
  testThat "NewDocument inserts a new order for a company user successfully" conn testNewDocumentForACompanyUserInsertsANewOrder,
  testThat "NewDocument with mismatching user & company results in left" conn testNewDocumentForMismatchingUserAndCompanyFails,

  testThat "CancelDocument cancels a document" conn testCancelDocumentCancelsDocument,
  testThat "CancelDocument fails if doc not pending or awaiting author" conn testCancelDocumentReturnsLeftIfDocInWrongState,

  testThat "SetDocumentLocale fails when doc doesn't exist" conn testSetDocumentLocaleNotLeft,

  testThat "SetDocumentTitle fails when doc doesn't exist" conn testSetDocumentTitleNotLeft,
  testThat "SetDocumentTitle succeeds when doc exists and has proper status" conn testSetDocumentTitleRight,

  testThat "SetDaysToSign fails when doc doesn't exist" conn testSetDocumentDaysToSignNotLeft,
  testThat "RemoveDaysToSign fails when doc doesn't exist" conn testRemoveDocumentDaysToSignNotLeft,
  testThat "SetDaysToSign and RemoveDaysToSign succeed when doc exist and has proper status" conn testSetDocumentDaysToSignRight,

  testThat "CloseDocument fails when doc is not signable" conn testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" conn testCloseDocumentNotNothing,
  testThat "CloseDocument succeeds when doc is signable and awaiting author" conn testCloseDocumentSignableAwaitingAuthorJust,
  testThat "CloseDocument fails when doc is signable and awaiting author" conn testCloseDocumentSignableNotAwaitingAuthorNothing,

  testThat "CancelDocument fails when doc is not signable" conn testCancelDocumentNotSignableNothing,
  testThat "CancelDocument fails when doc doesn't exist" conn testCancelDocumentNotNothing,
  testThat "CancelDocument succeeds when doc is signable and awaiting author" conn testCancelDocumentSignableAwaitingAuthorJust,
  testThat "CancelDocument fails when doc is signable and awaiting author" conn testCancelDocumentSignableNotAwaitingAuthorNothing,

  testThat "PendingToAwaitingAuthor fails when doc is not signable" conn testPendingToAwaitingAuthorDocumentNotSignableNothing,
  testThat "PendingToAwaitingAuthor fails when doc doesn't exist" conn testPendingToAwaitingAuthorDocumentNotNothing,
  testThat "PendingToAwaitingAuthor succeeds when doc is signable and awaiting author" conn testPendingToAwaitingAuthorDocumentSignableAwaitingAuthorJust,
  testThat "PendingToAwaitingAuthor fails when doc is signable and awaiting author" conn testPendingToAwaitingAuthorDocumentSignableNotAwaitingAuthorNothing,


  testThat "SetDocumentTags fails when does not exist" conn testSetDocumentTagsNotLeft,
  testThat "SetDocumentTags succeeds" conn testSetDocumentTagsRight,

  testThat "SetDocumentUI fails when does not exist" conn testSetDocumentUINotLeft,
  testThat "SetDocumentUI succeeds" conn testSetDocumentUIRight,

  testThat "SetDocumentTimeoutTime fails when does not exist" conn testSetDocumentTimeoutTimeNotLeft,
  testThat "SetDocumentTimeoutTime fails when not signable" conn testSetDocumentTimeoutTimeNotSignableLeft,
  testThat "SetDocumentTimeoutTime succeeds when signable" conn testSetDocumentTimeoutTimeSignableRight,

  testThat "GetTimeoutedButPendingDocuments works as expected" conn testGetTimedOutButPendingDocuments,

  testThat "SetInvitationDeliveryStatus fails when not signable" conn testSetInvitationDeliveryStatusNotSignableLeft,
  testThat "SetInvitationDeliveryStatus fails when doc does not exist" conn testSetInvitationDeliveryStatusNotLeft,
  testThat "SetInvitationDeliveryStatus succeeds if signable" conn testSetInvitationDeliveryStatusSignableRight,

  testThat "MarkDocumentSeen fails when not signable" conn testMarkDocumentSeenNotSignableLeft,
  testThat "MarkDocumentSeen fails when closed or preparation" conn testMarkDocumentSeenClosedOrPreparationLeft,
  testThat "MarkDocumentSeen fails when doc does not exist" conn testMarkDocumentSeenNotLeft,
  testThat "MarkDocumentSeen succeeds when siglink and magichash match" conn testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight,
  testThat "MarkDocumentSeen fails when the siglink matches but magichash does not" conn testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft,

  testThat "MarkInvitationRead when has not read" conn testMarkInvitationRead,
  testThat "MarkInvitationRead never fails when doc doesn't exist" conn testMarkInvitationReadDocDoesntExist,

  testThat "RejectDocument succeeds when signable and pending" conn testRejectDocumentSignablePendingRight,
  testThat "RejectDocument fails when document doesn't exist" conn testRejectDocumentNotLeft,
  testThat "RejectDocument fails when signable but not pending" conn testRejectDocumentSignableNotPendingLeft,
  testThat "RejectDocument fails when not signable" conn testRejectDocumentNotSignableLeft,

--  testThat "AuthorSignDocument succeeds when signable and preparation" conn testAuthorSignDocumentSignablePreparationRight,
--  testThat "AuthorSignDocument fails when document doesn't exist" conn testAuthorSignDocumentNotLeft,
--  testThat "AuthorSignDocument fails when signable but not preparation" conn testAuthorSignDocumentSignableNotPreparationLeft,
--  testThat "AuthorSignDocument fails when not signable" conn testAuthorSignDocumentNotSignableLeft,

  testThat "PreparationToPending succeeds when signable and preparation" conn testPreparationToPendingSignablePreparationRight,
  testThat "PreparationToPending fails when document doesn't exist" conn testPreparationToPendingNotLeft,
  testThat "PreparationToPending fails when signable but not preparation" conn testPreparationToPendingSignableNotPreparationLeft,
  testThat "PreparationToPending fails when not signable" conn testPreparationToPendingNotSignableLeft,

  testThat "SignDocument fails when doc doesn't exist" conn testSignDocumentNotLeft,
  testThat "SignDocument succeeds when doc is Signable and Pending" conn testSignDocumentSignablePendingRight,
  testThat "SignDocument fails when the document is Signable but not in Pending" conn testSignDocumentSignableNotPendingLeft,
  testThat "SignDocument fails when document is not signable" conn testSignDocumentNonSignableLeft,

  testThat "TimeoutDocument fails when doc doesn't exist" conn testTimeoutDocumentSignableNotLeft,
  testThat "TimeoutDocument succeeds when doc is Signable and Pending" conn testTimeoutDocumentSignablePendingRight,
  testThat "TimeoutDocument fails when the document is Signable but not in Pending" conn testTimeoutDocumentSignableNotPendingLeft,
  testThat "create document and check invariants" conn testNewDocumentDependencies,
  testThat "can create new document and read it back with the returned id" conn testDocumentCanBeCreatedAndFetchedByID,
  testThat "can create new document and read it back with GetDocuments" conn testDocumentCanBeCreatedAndFetchedByAllDocs,

{-
  testThat "when I call update document, it doesn't change the document id" conn testDocumentUpdateDoesNotChangeID,
  testThat "when I call update document, i can change the title" conn testDocumentUpdateCanChangeTitle,
  -}
  testThat "when I attach a file to a real document in preparation, it returns Right" conn testDocumentAttachPreparationRight,
  testThat "when I attach a file to a real document not in preparation, it returns Right" conn testDocumentAttachNotPreparationLeft,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" conn testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" conn testDocumentAttachHasAttachment,

  testThat "when I attach a sealed file to a bad docid, it always returns left" conn testNoDocumentAttachSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" conn testDocumentAttachSealedPendingRight,


  testThat "when I ChangeMainFile of a real document returns Right" conn testDocumentChangeMainFileRight,
  testThat "when I ChangeMainFile of a bad docid, it ALWAYS returns Left" conn testNoDocumentChangeMainFileAlwaysLeft,

  {-
  testThat "when I call updateDocument, it fails when the doc doesn't exist" conn testNoDocumentUpdateDocumentAlwaysLeft,
  testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" conn testNotPreparationUpdateDocumentAlwaysLeft,
  testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" conn testPreparationUpdateDocumentAlwaysRight,
-}
  testThat "when I create document from shared template author custom fields are stored" conn testCreateFromSharedTemplate,

  testThat "when I call ResetSignatoryDetails, it fails when the doc doesn't exist" conn testNoDocumentResetSignatoryDetailsAlwaysLeft,
  testThat "When I call ResetSignatoryDetails with a doc that is not in Preparation, always returns left" conn testNotPreparationResetSignatoryDetailsAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" conn testPreparationResetSignatoryDetailsAlwaysRight,
  testThat "when I call attachcsvupload with a doc that does not exist, always returns left" conn testNoDocumentAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload with a doc that is not in preparation, always returns left" conn testNotPreparationAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload and the csvindex is the author, return left" conn testPreparationAttachCSVUploadAuthorIndexLeft,
  testThat "when I call attachcsvupload and not existing signatory link, return left" conn testPreparationAttachCSVUploadNonExistingSignatoryLink,

  testThat "addDocumentAttachment fails if not in preparation" conn testAddDocumentAttachmentFailsIfNotPreparation,
  testThat "addDocumentAttachment doesn't fail if there's no attachments" conn testAddDocumentAttachmentOk,

  testThat "removeDocumentAttachment fails if not in preparation" conn testRemoveDocumentAttachmentFailsIfNotPreparation,
  testThat "removeDocumentAttachment doesn't fail if there's no attachments" conn testRemoveDocumentAttachmentOk,

  testThat "UpdateSigAttachments works as advertised" conn testUpdateSigAttachmentsAttachmentsOk,

  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "documentFromSignatoryData fails when document doesn't exist" conn testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "documentFromSignatoryData succeeds when document exists" conn testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" conn testTimeoutDocumentNonSignableLeft,
  testProperty "bitfieldDeriveConvertibleId" propbitfieldDeriveConvertibleId,

  -- archive & doc deletion tests
  testThat "ArchiveDocument fails if the document is pending or awaiting author" conn testArchiveDocumentPendingLeft,
  testThat "ArchiveDocument succeeds if the archiving user is the author" conn testArchiveDocumentAuthorRight,
  testThat "ArchiveDocument succeeds if the archiving user is a company admin" conn testArchiveDocumentCompanyAdminRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the author" conn testRestoreArchivedDocumentAuthorRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the company admin" conn testRestoreArchiveDocumentCompanyAdminRight,
  testThat "ReallyDeleteDocument succeeds if deleted by the author who is a private user" conn testReallyDeleteDocumentPrivateAuthorRight,
  testThat "ReallyDeleteDocument succeeds if deleted by a company admin user" conn testReallyDeleteDocumentCompanyAdminRight,
-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged

  testThat "ArchiveDocument fails if the archiving user is an unrelated user" conn testArchiveDocumentUnrelatedUserLeft,
  testThat "ArchiveDocument fails if the archiving user is just another standard company user" conn testArchiveDocumentCompanyStandardLeft,
  testThat "RestoreArchivedDocument fails if the storing user is an unrlated user" conn testRestoreArchivedDocumentUnrelatedUserLeft,
  testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user" conn testRestoreArchiveDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if deleted by the author who is a standard company user" conn testReallyDeleteDocumentCompanyAuthorLeft,
  testThat "ReallyDeleteDocument fails if the deleting user is just another standard company user" conn testReallyDeleteDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if the document hasn't been archived" conn testReallyDeleteNotArchivedLeft,

  testThat "GetDocumentsByAuthor doesn't return archived docs" conn testGetDocumentsByAuthorNoArchivedDocs,
  testThat "GetDocumentsByCompanyAndTags doesn't return archived docs" conn testGetDocumentsByCompanyAndTagsNoArchivedDocs,
  testThat "GetDocumentsBySignatory doesn't return archived docs" conn testGetDocumentsBySignatoryNoArchivedDocs,
  testThat "GetDeletedDocumentsByUser returns archived docs" conn testGetDeletedDocumentsByUserArchivedDocs

  ]

dataStructureProperties :: Test
dataStructureProperties = testGroup "data structure properties" [
  --testProperty "signatories are equal with same fields" propSignatoryDetailsEq,
  testProperty "signatories are different with different fields" propSignatoryDetailsNEq,
  testCase "given example" testSignatories1
  ]

testNewDocumentForNonCompanyUserInsertsANewContract :: DB ()
testNewDocumentForNonCompanyUserInsertsANewContract = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc title"
  assertGoodNewDocument Nothing (Signable Contract) "doc title" True result

testNewDocumentForACompanyUserInsertsANewContract :: DB ()
testNewDocumentForACompanyUserInsertsANewContract = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Contract) "doc title"
  assertGoodNewDocument (Just company) (Signable Contract) "doc title" True result

testNewDocumentForNonCompanyUserInsertsANewOffer :: DB ()
testNewDocumentForNonCompanyUserInsertsANewOffer = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Offer) "doc title"
  assertGoodNewDocument Nothing (Signable Offer) "doc title" False result

testNewDocumentForACompanyUserInsertsANewOffer :: DB ()
testNewDocumentForACompanyUserInsertsANewOffer = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Offer) "doc title"
  assertGoodNewDocument (Just company) (Signable Offer) "doc title" False result

testNewDocumentForNonCompanyUserInsertsANewOrder :: DB ()
testNewDocumentForNonCompanyUserInsertsANewOrder = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Order) "doc title"
  assertGoodNewDocument Nothing (Signable Order) "doc title" False result

testNewDocumentForACompanyUserInsertsANewOrder :: DB ()
testNewDocumentForACompanyUserInsertsANewOrder = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Offer) "doc title"
  assertGoodNewDocument (Just company) (Signable Offer) "doc title" False result

testNewDocumentForMismatchingUserAndCompanyFails :: DB ()
testNewDocumentForMismatchingUserAndCompanyFails = doTimes 10 $ do
  company <- addNewCompany
  singleuser <- addNewRandomUser
  companyuser <- addNewRandomCompanyUser (companyid company) False
  time <- getMinutesTime
  let aa = AuthorActor time (IPAddress 0) (userid singleuser) (BS.toString $ getEmail singleuser)
  edoc1 <- randomUpdate $ NewDocument singleuser (Just company) (BS.fromString "doc title") (Signable Contract) aa
  let ca = AuthorActor time (IPAddress 0) (userid companyuser) (BS.toString $ getEmail companyuser)
  edoc2 <- randomUpdate $ NewDocument companyuser Nothing (BS.fromString "doc title") (Signable Contract) ca
  validTest $ do
    assertLeft edoc1
    assertLeft edoc2
    
testReallyDeleteDocumentEvidenceLog :: DB ()
testReallyDeleteDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument author (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ReallyDeleteDocumentEvidence) lg
  
testRejectDocumentEvidenceLog :: DB ()
testRejectDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))  
  etdoc <- randomUpdate $ \m t->RejectDocument (documentid doc) (signatorylinkid sl) m (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RejectDocumentEvidence) lg
  
testRemoveDaysToSignEvidenceLog :: DB ()
testRemoveDaysToSignEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->RemoveDaysToSign (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RemoveDaysToSignEvidence) lg
  
testRemoveDocumentAttachmentEvidenceLog :: DB ()
testRemoveDocumentAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _ <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (SystemActor t)
  etdoc <- randomUpdate $ \t->RemoveDocumentAttachment (documentid doc) (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RemoveDocumentAttachmentEvidence) lg
  
testResetSignatoryDetailsEvidenceLog :: DB ()
testResetSignatoryDetailsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \sd t->ResetSignatoryDetails (documentid doc) [(sd, [SignatoryAuthor])] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ResetSignatoryDetailsEvidence) lg
  
testRestartDocumentEvidenceLog :: DB ()
testRestartDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  cdoc <- randomUpdate $ \t->CancelDocument (documentid doc) ManualCancel (SystemActor t)
  assertRight cdoc
  etdoc <- randomUpdate $ \t->RestartDocument (fromRight cdoc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromRight etdoc)
  assertJust $ find (\e -> evType e == RestartDocumentEvidence) lg
  assertJust $ find (\e -> evType e == CancelDocumentEvidence) lg
  lg2 <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RestartDocumentEvidence) lg2
  
testRestoreArchivedDocumentEvidenceLog :: DB ()
testRestoreArchivedDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RestoreArchivedDocumentEvidence) lg
  
testSaveDocumentForUserEvidenceLog :: DB ()
testSaveDocumentForUserEvidenceLog = do
  author <- addNewRandomUser
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \t->SaveDocumentForUser (documentid doc) user (signatorylinkid sl) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SaveDocumentForUserEvidence) lg
  
testSetDaysToSignEvidenceLog :: DB ()
testSetDaysToSignEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->SetDaysToSign (documentid doc) 30 (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDaysToSignEvidence) lg
  
testSetDocumentAdvancedFunctionalityEvidenceLog :: DB ()
testSetDocumentAdvancedFunctionalityEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (((==) BasicFunctionality . documentfunctionality) &&^ isPreparation)
  etdoc <- randomUpdate $ \t->SetDocumentAdvancedFunctionality (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentAdvancedFunctionalityEvidence) lg
  
testSetDocumentInviteTimeEvidenceLog :: DB ()
testSetDocumentInviteTimeEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->SetDocumentInviteTime (documentid doc) t (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentInviteTimeEvidence) lg
  
testSetDocumentLocaleEvidenceLog :: DB ()
testSetDocumentLocaleEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \l t->SetDocumentLocale (documentid doc) l (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentLocaleEvidence) lg
  
testSetDocumentTagsEvidenceLog :: DB ()
testSetDocumentTagsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \ts t->SetDocumentTags (documentid doc) ts (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTagsEvidence) lg
  
testSetDocumentTimeoutTimeEvidenceLog :: DB ()
testSetDocumentTimeoutTimeEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  etdoc <- randomUpdate $ \o t->SetDocumentTimeoutTime (documentid doc) o (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTimeoutTimeEvidence) lg
  
testSetDocumentTitleEvidenceLog :: DB ()
testSetDocumentTitleEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \n t->SetDocumentTitle (documentid doc) n (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTitleEvidence) lg
  
testSetDocumentUIEvidenceLog :: DB ()
testSetDocumentUIEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \u t->SetDocumentUI (documentid doc) u (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentUIEvidence) lg
  
testSetElegitimationIdentificationEvidenceLog :: DB ()
testSetElegitimationIdentificationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->SetElegitimationIdentification (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetElegitimationIdentificationEvidence) lg
  
testSetEmailIdentificationEvidenceLog :: DB ()
testSetEmailIdentificationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->SetEmailIdentification (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetEmailIdentificationEvidence) lg
  
testSetInvitationDeliveryStatusEvidenceLog :: DB ()
testSetInvitationDeliveryStatusEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \s t->SetInvitationDeliveryStatus (documentid doc) (signatorylinkid sl) s (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationDeliveryStatusEvidence) lg
  
testSetInviteTextEvidenceLog :: DB ()
testSetInviteTextEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \i t->SetInviteText (documentid doc) i (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationTextEvidence) lg
  
testSignDocumentEvidenceLog :: DB ()
testSignDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  etdoc <- randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SignDocumentEvidence) lg
    
testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog :: DB ()
testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isTemplate &&^ isPreparation)
  _<- randomUpdate $ \i t->SetInviteText (documentid doc) i (SystemActor t)
  etdoc <- randomUpdate $ \t->SignableFromDocumentIDWithUpdatedAuthor author Nothing (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromRight etdoc)
  assertJust $ find (\e -> evType e == SignableFromDocumentIDWithUpdatedAuthorEvidence) lg
  assertJust $ find (\e -> evType e == SetInvitationTextEvidence) lg
  
testTemplateFromDocumentEvidenceLog :: DB ()
testTemplateFromDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- randomUpdate $ \t->TemplateFromDocument (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == TemplateFromDocumentEvidence) lg
  
testTimeoutDocumentEvidenceLog :: DB ()
testTimeoutDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  etdoc <- randomUpdate $ \t->TimeoutDocument (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == TimeoutDocumentEvidence) lg
  
testUpdateFieldsEvidenceLog :: DB ()
testUpdateFieldsEvidenceLog = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ isSignable &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \f t->UpdateFields (documentid doc) (signatorylinkid sl) [f] (SystemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  validTest $ do
    assertRight etdoc
    assertJust $ find (\e -> evType e == UpdateFieldsEvidence) lg
  
testPreparationToPendingEvidenceLog :: DB ()
testPreparationToPendingEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  etdoc <- randomUpdate $ \t->PreparationToPending (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == PreparationToPendingEvidence) lg
  
testPendingToAwaitingAuthorEvidenceLog :: DB ()
testPendingToAwaitingAuthorEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  forM_ (filter (not . isAuthor) $ documentsignatorylinks doc) $ \sl-> do
    m <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
    assertRight m
    s <- randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing (SystemActor t)
    assertRight s
  etdoc <- randomUpdate $ \t->PendingToAwaitingAuthor (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == PendingToAwaitingAuthorEvidence) lg

testMarkInvitationReadEvidenceLog :: DB ()    
testMarkInvitationReadEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = getAuthorSigLink doc
  etdoc <- randomUpdate $ \t->MarkInvitationRead (documentid doc) (signatorylinkid sl) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == MarkInvitationReadEvidence) lg
    
testMarkDocumentSeenEvidenceLog :: DB ()    
testMarkDocumentSeenEvidenceLog  = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = getAuthorSigLink doc
  etdoc <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  assertRight etdoc
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  _ <- randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing (SystemActor t)  
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertBool "Should have 3 seen events." $ 3 == length (filter (\e -> evType e == MarkDocumentSeenEvidence) lg)


testErrorDocumentEvidenceLog :: DB ()    
testErrorDocumentEvidenceLog  = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (const True)
  etdoc <- randomUpdate $ \t->ErrorDocument (documentid doc) "Some error" (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ErrorDocumentEvidence) lg
    
testDocumentFromSignatoryDataEvidenceLog :: DB ()
testDocumentFromSignatoryDataEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  _<-randomUpdate $ \t->UpdateSigAttachments (documentid doc) 
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentemail       = BS.fromString "hello@goodbye.com"
                                             , signatoryattachmentname        = BS.fromString "attachment"
                                             , signatoryattachmentdescription = BS.fromString "gimme!"
                                             }] (SystemActor t)
  etdoc <- randomUpdate $ \t a b c d e f -> DocumentFromSignatoryData (documentid doc) 1 a b c d e f [] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromRight etdoc)
  assertJust $ find (\e -> evType e == AddSigAttachmentEvidence) lg
  assertJust $ find (\e -> evType e == AuthorUsesCSVEvidence)    lg

testSaveSigAttachmentEvidenceLog :: DB ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _<-randomUpdate $ \t->UpdateSigAttachments (documentid doc) 
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentemail       = BS.fromString "hello@goodbye.com"
                                             , signatoryattachmentname        = BS.fromString "attachment"
                                             , signatoryattachmentdescription = BS.fromString "gimme!"
                                             }] (SystemActor t)
  _ <- randomUpdate $ \t->PreparationToPending (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->SaveSigAttachment (documentid doc) (BS.fromString "attachment") (BS.fromString "hello@goodbye.com") (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SaveSigAttachmentEvidence) lg
  
testUpdateSigAttachmentsEvidenceLog :: DB ()
testUpdateSigAttachmentsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  etdoc<-randomUpdate $ \t->UpdateSigAttachments (documentid doc) 
                        [SignatoryAttachment { signatoryattachmentfile        = Just $ (fileid file)
                                             , signatoryattachmentemail       = BS.fromString "hello@goodbye.com"
                                             , signatoryattachmentname        = BS.fromString "attachment"
                                             , signatoryattachmentdescription = BS.fromString "gimme!"
                                             }] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AddSigAttachmentEvidence) lg
  
testDeleteSigAttachmentEvidenceLog :: DB ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _<-randomUpdate $ \t->UpdateSigAttachments (documentid doc) 
                        [SignatoryAttachment { signatoryattachmentfile        = Just $ (fileid file)
                                             , signatoryattachmentemail       = BS.fromString "hello@goodbye.com"
                                             , signatoryattachmentname        = BS.fromString "attachment"
                                             , signatoryattachmentdescription = BS.fromString "gimme!"
                                             }] (SystemActor t)
  etdoc <- randomUpdate $ \t->DeleteSigAttachment (documentid doc) (BS.fromString "hello@goodbye.com") (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == DeleteSigAttachmentEvidence) lg

testNewDocumentEvidenceLog :: DB ()
testNewDocumentEvidenceLog = do
  (_, _, ed) <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc"
  assertRight ed
  let Right d = ed
      did = documentid d
  lg <- dbQuery $ GetEvidenceLog did
  assertJust $ find (\e -> evType e == NewDocumentEvidence) lg
  
testAddDocumentAttachmentEvidenceLog :: DB ()
testAddDocumentAttachmentEvidenceLog = do
  (_, _, ed) <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc"
  assertRight ed
  let Right d = ed
      did = documentid d
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t->AddDocumentAttachment did (fileid file) (SystemActor t)
  assertRight edoc
  lg <- dbQuery $ GetEvidenceLog did
  assertJust $ find (\e -> evType e == AddDocumentAttachmentEvidence) lg
  
testAddInvitationEvidenceLog :: DB ()
testAddInvitationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \t->AddInvitationEvidence (documentid doc) (signatorylinkid sl) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == InvitationEvidence) lg

testArchiveDocumentEvidenceLog :: DB ()
testArchiveDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  etdoc <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ArchiveDocumentEvidence) lg

testAttachCSVUploadEvidenceLog :: DB ()
testAttachCSVUploadEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \c t->AttachCSVUpload (documentid doc) (signatorylinkid sl) c (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachCSVUploadEvidence) lg

testAttachFileEvidenceLog :: DB ()
testAttachFileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  etdoc <- randomUpdate $ \t->AttachFile (documentid doc) (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachFileEvidence) lg

testAttachSealedFileEvidenceLog :: DB ()
testAttachSealedFileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  file <- addNewRandomFile  
  etdoc <- randomUpdate $ \t->AttachSealedFile (documentid doc) (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachSealedFileEvidence) lg

testCancelDocumentEvidenceLog :: DB ()
testCancelDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (isPending ||^ isAwaitingAuthor))
  etdoc <- randomUpdate $ \t-> CancelDocument (documentid doc) ManualCancel (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == CancelDocumentEvidence) lg

testChangeMainfileEvidenceLog :: DB ()
testChangeMainfileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (const True)
  file <- addNewRandomFile  
  etdoc <- randomUpdate $ \t-> ChangeMainfile (documentid doc) (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ChangeMainfileEvidence) lg

testChangeSignatoryEmailWhenUndeliveredEvidenceLog :: DB ()
testChangeSignatoryEmailWhenUndeliveredEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- randomUpdate $ \t-> ChangeSignatoryEmailWhenUndelivered (documentid doc) (signatorylinkid sl) Nothing (BS.fromString "email@email.com") (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ChangeSignatoryEmailWhenUndeliveredEvidence) lg

testCloseDocumentEvidenceLog :: DB ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  forM_ (documentsignatorylinks doc) $ \sl -> do
    ignore $ randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
    ignore $ randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing (SystemActor t)
  etdoc <- randomUpdate $ \t-> CloseDocument (documentid doc) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> DB (User, MinutesTime, Either String Document)
performNewDocumentWithRandomUser Nothing doctype title = do
  user <- addNewRandomUser
  time <- getMinutesTime
  let aa = AuthorActor time (IPAddress 0) (userid user) (BS.toString $ getEmail user)  
  edoc <- randomUpdate $ NewDocument user Nothing (BS.fromString title) doctype aa
  return (user, time, edoc)
performNewDocumentWithRandomUser (Just company) doctype title = do
  user <- addNewRandomCompanyUser (companyid company) False
  time <- getMinutesTime
  let aa = AuthorActor time (IPAddress 0) (userid user) (BS.toString $ getEmail user)  
  edoc <- randomUpdate $ NewDocument user (Just company) (BS.fromString title) doctype aa
  return (user, time, edoc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> Bool -> (User, MinutesTime, Either String Document) -> DB (Maybe (DB ()))
assertGoodNewDocument mcompany doctype title authorsigns (user, time, edoc) = do
  let (Right doc) = edoc
  validTest $ do
    assertRight edoc
    assertEqual "Correct title" (BS.fromString title) (documenttitle doc)
    assertEqual "Correct type" doctype (documenttype doc)
    assertEqual "Doc has user's region" (getRegion user) (getRegion doc)
    assertEqual "Doc creation time" time (documentctime doc)
    assertEqual "Doc modification time" time (documentmtime doc)
    assertEqual "Doc has user's service" (userservice user) (documentservice doc)
    assertEqual "No author attachments" [] (documentauthorattachments doc)
    assertEqual "No sig attachments" [] (documentsignatoryattachments doc)
    assertEqual "Uses email identification only" [EmailIdentification] (documentallowedidtypes doc)
    assertEqual "Doc has user's footer" (customfooter $ usersettings user) (fmap BS.toString <$> documentmailfooter $ documentui doc)
    assertEqual "In preparation" Preparation (documentstatus doc)
    assertEqual "1 signatory" 1 (length $ documentsignatorylinks doc)
    let siglink = head $ documentsignatorylinks doc
    if authorsigns
      then assertEqual "link is author and signer" [SignatoryPartner,SignatoryAuthor] (signatoryroles siglink)
      else assertEqual "link is just author" [SignatoryAuthor] (signatoryroles siglink)
    assertEqual "link first name matches author's" (getFirstName user) (getFirstName siglink)
    assertEqual "link last name matches author's" (getLastName user) (getLastName siglink)
    assertEqual "link email matches author's" (getEmail user) (getEmail siglink)
    assertEqual "link personal number matches author's" (getPersonalNumber user) (getPersonalNumber siglink)
    assertEqual "link company name matches company's" (getCompanyName mcompany) (getCompanyName siglink)
    assertEqual "link company number matches company's" (getCompanyNumber mcompany) (getCompanyNumber siglink)
    assertEqual "link signatory matches author id" (Just $ userid user) (maybesignatory siglink)
    assertEqual "link signatory matches author company" (companyid <$> mcompany) (maybecompany siglink)

testCancelDocumentCancelsDocument :: DB ()
testCancelDocumentCancelsDocument = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> isSignable d && documentstatus d `elem` [AwaitingAuthor, Pending])
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time (IPAddress 0) (userid user) (BS.toString $ getEmail user))
  when (isLeft edoc) $ Log.debug (fromLeft edoc)
  assertRight edoc
  let (Right canceleddoc) = edoc
  validTest $ do
    assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
    assertEqual "Updated modification time" time (documentmtime canceleddoc)
    assertEqual "Matching cancellation reason" (Just ManualCancel) (documentcancelationreason canceleddoc)
    assertEqual "Siglinks are unchanged" (documentsignatorylinks doc) (documentsignatorylinks canceleddoc)
    assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: DB ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> isSignable d && not (documentstatus d `elem` [AwaitingAuthor, Pending]))
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time (IPAddress 0) (userid user) (BS.toString $ getEmail user)) 
  validTest $ assertLeft edoc

testSignatories1 :: Assertion
testSignatories1 =
  let s1 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField FirstNameFT (BS.fromString "Eric") []
                                               ,SignatoryField LastNameFT (BS.fromString "Normand") []
                                                ]
                            }
      s2 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField LastNameFT (BS.fromString "Normand") []
                                               ,SignatoryField FirstNameFT (BS.fromString "Eric") []
                                                ]
                            }
  in assertBool "Signatories should be equal" (s1 == s2)

propSignatoryDetailsEq :: SignOrder -> SignatoryDetails -> Property
propSignatoryDetailsEq o1 sd =
   (o1 == o1) ==> sd{signatorysignorder = o1} == sd{signatorysignorder = o1}

propSignatoryDetailsNEq :: SignOrder -> SignOrder -> SignatoryDetails -> Property
propSignatoryDetailsNEq o1 o2 sd =
  (o1 /= o2) ==> sd{signatorysignorder = o1} /= sd{signatorysignorder = o2}

assertOneArchivedSigLink :: MonadIO m => Either a Document -> m ()
assertOneArchivedSigLink etdoc =
  assertEqual "Expected one archived sig link"
              1
              (length . filter signatorylinkdeleted . documentsignatorylinks $ fromRight etdoc)

assertOneReallyDeletedSigLink :: MonadIO m => Either a Document -> m ()
assertOneReallyDeletedSigLink etdoc =
  assertEqual "Expected one really deleted sig link"
              1
              (length . filter signatorylinkreallydeleted . documentsignatorylinks $ fromRight etdoc)

assertNoArchivedSigLink :: MonadIO m => Either a Document -> m ()
assertNoArchivedSigLink etdoc =
  assertEqual "Expected no archived sig link"
              0
              (length . filter signatorylinkdeleted . documentsignatorylinks $ fromRight etdoc)

testArchiveDocumentPendingLeft :: DB ()
testArchiveDocumentPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> (isPending d || isAwaitingAuthor d))
  etdoc <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testArchiveDocumentAuthorRight :: DB ()
testArchiveDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertOneArchivedSigLink etdoc

testArchiveDocumentCompanyAdminRight :: DB ()
testArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ \t->ArchiveDocument adminuser (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertOneArchivedSigLink etdoc

testRestoreArchivedDocumentAuthorRight :: DB ()
testRestoreArchivedDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (SystemActor t)
  validTest $ do 
    assertRight etdoc
    assertNoArchivedSigLink etdoc

testRestoreArchiveDocumentCompanyAdminRight :: DB ()
testRestoreArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->RestoreArchivedDocument adminuser (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertNoArchivedSigLink etdoc

testReallyDeleteDocumentPrivateAuthorRight :: DB ()
testReallyDeleteDocumentPrivateAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc' <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  assertRight etdoc'
  assertOneArchivedSigLink etdoc'
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument author (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertOneReallyDeletedSigLink etdoc

testReallyDeleteDocumentCompanyAdminRight :: DB ()
testReallyDeleteDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument adminuser (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertOneReallyDeletedSigLink etdoc

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: DB ()
testArchiveDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomAdvancedUser
  etdoc <- randomUpdate $ \t->ArchiveDocument unrelateduser (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testArchiveDocumentCompanyStandardLeft :: DB ()
testArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ \t->ArchiveDocument standarduser (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testRestoreArchivedDocumentUnrelatedUserLeft :: DB ()
testRestoreArchivedDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomAdvancedUser
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->RestoreArchivedDocument unrelateduser (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testRestoreArchiveDocumentCompanyStandardLeft :: DB ()
testRestoreArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->RestoreArchivedDocument standarduser (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testReallyDeleteDocumentCompanyAuthorLeft :: DB ()
testReallyDeleteDocumentCompanyAuthorLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument author (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testReallyDeleteDocumentCompanyStandardLeft :: DB ()
testReallyDeleteDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument standarduser (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testReallyDeleteNotArchivedLeft :: DB ()
testReallyDeleteNotArchivedLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ \t->ReallyDeleteDocument author (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testGetDocumentsByAuthorNoArchivedDocs :: DB ()
testGetDocumentsByAuthorNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsByAuthor . userid)

testGetDocumentsByCompanyAndTagsNoArchivedDocs :: DB ()
testGetDocumentsByCompanyAndTagsNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (\u -> GetDocumentsByCompanyAndTags Nothing (fromJust $ usercompany u) [])

testGetDocumentsBySignatoryNoArchivedDocs :: DB ()
testGetDocumentsBySignatoryNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsBySignatory . userid)

checkQueryDoesntContainArchivedDocs :: DBQuery q [Document] => (User -> q) -> DB ()
checkQueryDoesntContainArchivedDocs qry = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  docsbeforearchive <- dbQuery (qry author)
  assertEqual "Expecting one doc before archive" [documentid doc] (map documentid docsbeforearchive)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  docsafterarchive <- dbQuery (qry author)
  assertEqual "Expecting no docs after archive" [] (map documentid docsafterarchive)
  _ <- randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (SystemActor t)
  docsafterestore <- dbQuery (qry author)
  validTest $ assertEqual "Expecting one doc after restoring" [documentid doc] (map documentid docsafterestore)

testGetDeletedDocumentsByUserArchivedDocs :: DB ()
testGetDeletedDocumentsByUserArchivedDocs =
  checkQueryContainsArchivedDocs (GetDeletedDocumentsByUser . userid)

checkQueryContainsArchivedDocs :: DBQuery q [Document] => (User -> q) -> DB ()
checkQueryContainsArchivedDocs qry = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  docsbeforearchive <- dbQuery (qry author)
  assertEqual "Expecting no docs before archive" [] (map documentid docsbeforearchive)
  _ <- randomUpdate $ \t->ArchiveDocument author (documentid doc) (SystemActor t)
  docsafterarchive <- dbQuery (qry author)
  assertEqual "Expecting 1 doc after archive" [documentid doc] (map documentid docsafterarchive)
  _ <- randomUpdate $ \t -> ReallyDeleteDocument author (documentid doc) (SystemActor t)
  docsafterdelete <- dbQuery (qry author)
  validTest $ assertEqual "Expecting no docs after really deleting" [] (map documentid docsafterdelete)

testSetDocumentLocaleNotLeft :: DB ()
testSetDocumentLocaleNotLeft = doTimes 10 $ do
  edoc <- randomUpdate $ \d l t -> SetDocumentLocale d l (SystemActor t)
  validTest $ do
    assertLeft edoc

testNewDocumentDependencies :: DB ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  -- execute
  now <- liftIO $ getMinutesTime
  let aa = AuthorActor now (IPAddress 0) (userid author) (BS.toString $ getEmail author)
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype aa)
  -- assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testDocumentCanBeCreatedAndFetchedByID :: DB ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  now <- liftIO $ getMinutesTime
  let aa = AuthorActor now (IPAddress 0) (userid author) (BS.toString $ getEmail author)
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype aa)
  let doc = case edoc of
          Left msg -> error $ show msg
          Right d -> d
  -- execute
  mdoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  -- assert
  validTest $ do
    assertJust mdoc
    assert $ sameDocID doc (fromJust mdoc)
    assertInvariants (fromJust mdoc)

testDocumentCanBeCreatedAndFetchedByAllDocs :: DB ()
testDocumentCanBeCreatedAndFetchedByAllDocs = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  -- execute
  now <- liftIO $ getMinutesTime
  let aa = AuthorActor now (IPAddress 0) (userid author) (BS.toString $ getEmail author)
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype aa)

  let doc = case edoc of
          Left msg -> error $ show msg
          Right d -> d
  docs <- dbQuery $ GetDocuments Nothing
  -- assert
  validTest $ do
    assertJust $ find (sameDocID doc) docs
    assertInvariants $ fromJust $ find (sameDocID doc) docs

{-
testDocumentUpdateDoesNotChangeID :: DB ()
testDocumentUpdateDoesNotChangeID = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  (mt, a, b, c, d, e, f) <- rand 10 arbitrary
  doc <-  addRandomDocumentWithAuthorAndCondition author isPreparation
  r <- getRandomAuthorRoles doc

  let sd = signatoryDetailsFromUser author Nothing
  -- execute
  enewdoc <- (runDB . dbUpdate) $ Reset mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality
  --assert
  validTest $ do
    assertRight enewdoc
    assert $ sameDocID doc $ fromRight enewdoc
    assertInvariants $ fromRight enewdoc

testDocumentUpdateCanChangeTitle :: DB ()
testDocumentUpdateCanChangeTitle = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  (mt, a, b, c, d, e, f) <- rand 10 arbitrary
  r <- getRandomAuthorRoles doc

  --execute
  let sd = signatoryDetailsFromUser author Nothing
  enewdoc <- (runDB . dbUpdate) $ UpdateDocument mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality
  --assert
  validTest $ do
    assertRight enewdoc
    assert $ (documenttitle $ fromRight enewdoc) == a
    assertInvariants $ fromRight enewdoc
-}
testDocumentAttachNotPreparationLeft :: DB ()
testDocumentAttachNotPreparationLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t->AttachFile (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ do
    assertLeft edoc

testDocumentAttachPreparationRight :: DB ()
testDocumentAttachPreparationRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t -> AttachFile (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc


testNoDocumentAttachAlwaysLeft :: DB ()
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
  -- setup
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ (\docid t -> AttachFile docid (fileid file) (SystemActor t))
  --assert
  validTest $ do
    assertLeft edoc

testDocumentAttachHasAttachment :: DB ()
testDocumentAttachHasAttachment = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t -> AttachFile (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ do
    assertRight edoc
    -- assertJust $ find ((== a) . filename) (documentfiles $ fromRight edoc)
    assertInvariants $ fromRight edoc

testNoDocumentAttachSealedAlwaysLeft :: DB ()
testNoDocumentAttachSealedAlwaysLeft = doTimes 10 $ do
  -- setup
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  time <- rand 10 arbitrary
  edoc <- randomUpdate $ (\docid -> AttachSealedFile docid (fileid file) (SystemActor time))
  --assert
  validTest $ assertLeft edoc

testDocumentAttachSealedPendingRight :: DB ()
testDocumentAttachSealedPendingRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument ((randomDocumentAllowsDefault author) { randomDocumentAllowedTypes = [ Signable Offer
                                                                                                , Signable Contract
                                                                                                , Signable Offer
                                                                                                ]
                                                                 , randomDocumentAllowedStatuses = [Closed]
                                                                 })
  file <- addNewRandomFile
  time <- rand 10 arbitrary
  --execute
  edoc <- randomUpdate $ AttachSealedFile (documentid doc) (fileid file) (SystemActor time)
  --assert
  validTest $ do
    assertRight edoc
    assertBool "Should have new file attached, but it's not" $ (fileid file) `elem` documentsealedfiles (fromRight edoc)


testDocumentChangeMainFileRight :: DB ()
testDocumentChangeMainFileRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (const True)
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t->ChangeMainfile (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ do
    assertRight edoc
    let doc1 = fromRight edoc
    assertBool "New file is attached" (fileid file `elem` (documentfiles doc1 ++ documentsealedfiles doc1))
    assertInvariants $ fromRight edoc


testNoDocumentChangeMainFileAlwaysLeft :: DB ()
testNoDocumentChangeMainFileAlwaysLeft = doTimes 10 $ do
  -- setup
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ (\docid t -> ChangeMainfile docid (fileid file) (SystemActor t))
  --assert
  validTest $ do
    assertLeft edoc

testGetTimedOutButPendingDocuments :: DB ()
testGetTimedOutButPendingDocuments = doTimes 1 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ (isJust . documenttimeouttime))
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (not . (isPending ||^ isAwaitingAuthor))

  let t = unTimeoutTime $ fromJust $ documenttimeouttime doc
  --execute
  docsA <- dbQuery $ GetTimeoutedButPendingDocuments ((-10) `minutesAfter` t)
  docsB <- dbQuery $ GetTimeoutedButPendingDocuments (10 `minutesAfter` t)

  --assert
  validTest $ do
    assertEqual "Documents do not timeout before time" [] (map documentstatus docsA)
    assertEqual "Documents timeout after time" [Pending] (map documentstatus docsB)


{-
testNotPreparationUpdateDocumentAlwaysLeft :: DB ()
testNotPreparationUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  (mt, a, b, c, d, e, f) <- rand 10 arbitrary

  let sd = signatoryDetailsFromUser author Nothing
  -- execute
  enewdoc <- (runDB . dbUpdate) $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f AdvancedFunctionality

  --assert
  validTest $ assertLeft enewdoc

testPreparationUpdateDocumentAlwaysRight :: DB ()
testPreparationUpdateDocumentAlwaysRight = doTimes 10 $ do
  -- setup
  (mt, a, b, c, d, e, f) <- rand 10 arbitrary

  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  r <- getRandomAuthorRoles doc
  let sd = signatoryDetailsFromUser author Nothing

  --execute
  enewdoc <- (runDB . dbUpdate) $ UpdateDocument mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality

  --assert
  validTest $ do
    assertRight enewdoc
    assertInvariants $ fromRight enewdoc

testNoDocumentUpdateDocumentAlwaysLeft :: DB ()
testNoDocumentUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ UpdateDocument
  --assert
  validTest $ assertLeft edoc
-}
testNotPreparationResetSignatoryDetailsAlwaysLeft :: DB ()
testNotPreparationResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  mt <- rand 10 arbitrary
  let sd = signatoryDetailsFromUser author Nothing
  --execute
  edoc <- dbUpdate $ ResetSignatoryDetails (documentid doc) [(sd, [SignatoryAuthor])] (SystemActor mt)
  --assert
  validTest $ assertLeft edoc

testPreparationResetSignatoryDetailsAlwaysRight :: DB ()
testPreparationResetSignatoryDetailsAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  mt <- rand 10 arbitrary
  --execute
  edoc <- dbUpdate $ ResetSignatoryDetails (documentid doc) [(emptySignatoryDetails, [SignatoryAuthor])] (SystemActor mt)
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testNoDocumentResetSignatoryDetailsAlwaysLeft :: DB ()
testNoDocumentResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  --author <- addNewRandomAdvancedUser
  mt <- rand 10 arbitrary
  --execute
  -- non-existent docid
  edoc <- dbUpdate $ ResetSignatoryDetails a [(emptySignatoryDetails, [SignatoryAuthor])] (SystemActor mt)
  --assert
  validTest $ assertLeft edoc

testNoDocumentAttachCSVUploadAlwaysLeft :: DB ()
testNoDocumentAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  edoc <- randomUpdate $ \did slid csv t->AttachCSVUpload did slid csv (SystemActor t)
  --assert
  validTest $ assertLeft edoc

testNotPreparationAttachCSVUploadAlwaysLeft :: DB ()
testNotPreparationAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  slid <- rand 10 $ elements [signatorylinkid sl | sl <- documentsignatorylinks doc, isSignatory sl]
  --execute
  edoc <- randomUpdate $ \csv t -> AttachCSVUpload (documentid doc) slid csv (SystemActor t)
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadAuthorIndexLeft :: DB ()
testPreparationAttachCSVUploadAuthorIndexLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  (csvupload, t) <- rand 10 arbitrary
  let Just ai = authorIndex (documentsignatorylinks doc)
  --execute
  edoc <- dbUpdate $ AttachCSVUpload (documentid doc)
          (signatorylinkid ((documentsignatorylinks doc) !! ai))
          (csvupload { csvsignatoryindex = ai })
          (SystemActor t)
  --assert
  validTest $ assertLeft edoc

authorIndex :: [SignatoryLink] -> Maybe Int
authorIndex sls = case catMaybes $ zipWith (\sl i -> if isAuthor sl then Just i else Nothing) sls [0..] of
  [] -> Nothing
  x:_ -> Just x

testPreparationAttachCSVUploadNonExistingSignatoryLink :: DB ()
testPreparationAttachCSVUploadNonExistingSignatoryLink = doTimes 3 $ do
  -- setup
  (csvupload, time) <- rand 10 arbitrary
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  let i = 1 + maximum [unSignatoryLinkID $ signatorylinkid sl | sl <- documentsignatorylinks doc]
  --execute
  edoc <- dbUpdate $ AttachCSVUpload (documentid doc) 
          (SignatoryLinkID i) csvupload (SystemActor time)
  --assert
  validTest $ assertLeft edoc


testCreateFromSharedTemplate :: DB ()
testCreateFromSharedTemplate = do
  user <- addNewRandomAdvancedUser
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (not . isAttachment)
  tmpdoc <- fmap fromJust $ dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary  
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else fmap fromRight $ dbUpdate $ TemplateFromDocument docid (SystemActor mt)
  newuser <- addNewRandomAdvancedUser

  doc' <- fmap fromRight $ dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor newuser Nothing (documentid doc) (SystemActor mt)
  let [author1] = filter isAuthor $ documentsignatorylinks doc
  let [author2] = filter isAuthor $ documentsignatorylinks doc'
  let isCustom (SignatoryField { sfType = CustomFT _ _ }) = True
      isCustom _ = False
  if (fmap sfValue $ filter isCustom $ signatoryfields $ signatorydetails author1)
     == (fmap sfValue $ filter isCustom $ signatoryfields $ signatorydetails author2)
    then assertSuccess
    else assertFailure "Replacing signatory details based on user is loosing fields | SKRIVAPADEV-294"

testAddDocumentAttachmentFailsIfNotPreparation :: DB ()
testAddDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ assertLeft edoc

testAddDocumentAttachmentOk :: DB ()
testAddDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (SystemActor t)
  --assert
  validTest $ do
    assertRight edoc
    let doc1 = fromRight edoc
    assertEqual "Author attachment was really attached" [fileid file]
                  (map authorattachmentfile $ documentauthorattachments doc1)

testRemoveDocumentAttachmentFailsIfNotPreparation :: DB ()
testRemoveDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  edoc <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (FileID 0) (SystemActor t)
  --assert
  validTest $ assertLeft edoc

testRemoveDocumentAttachmentOk :: DB ()
testRemoveDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (FileID 0) (SystemActor t)
  --assert
  validTest $ assertRight edoc

---------------------------------------------------------------------

-- DeleteSigAttachment, SaveSigAttachment, UpdateSigAttachments

{-
testUpdateSigAttachmentsFailsIfNotPreparation :: DB ()
testAddDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ AddDocumentAttachment (documentid doc) (fileid file)
  --assert
  validTest $ assertLeft edoc
-}

testUpdateSigAttachmentsAttachmentsOk :: DB ()
testUpdateSigAttachmentsAttachmentsOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file1 <- addNewRandomFile
  file2 <- addNewRandomFile
  --execute
  let email1 = BS.fromString "g1@g.com"
      name1 = BS.fromString "att1"
  let att1 = SignatoryAttachment { signatoryattachmentfile = Just (fileid file1)
                                 , signatoryattachmentemail = email1
                                 , signatoryattachmentname = name1
                                 , signatoryattachmentdescription = BS.fromString "att1 description"
                                 }
  let att2 = SignatoryAttachment { signatoryattachmentfile = Nothing
                                 , signatoryattachmentemail = BS.fromString "g2@g.com"
                                 , signatoryattachmentname = BS.fromString "att2"
                                 , signatoryattachmentdescription = BS.fromString "att2 description"
                                 }
  (time, sl) <- rand 10 arbitrary
  let sa = SignatoryActor time (IPAddress 0) Nothing (BS.toString email1) sl
  edoc1 <- randomUpdate $ UpdateSigAttachments (documentid doc) [att1, att2] sa

  edoc2 <- randomUpdate $ DeleteSigAttachment (documentid doc) email1 (fileid file1) sa

  edoc3 <- randomUpdate $ SaveSigAttachment (documentid doc) name1 email1 (fileid file2) sa

  --assert
  validTest $ do
    assertRight edoc1
    let doc1 = fromRight edoc1
    assertEqual "Both attachments were attached" 2 (length (documentsignatoryattachments doc1))
    assertRight edoc2
    let doc2 = fromRight edoc2
    assertBool "All signatory attachments are not connected to files" (all (isNothing . signatoryattachmentfile) (documentsignatoryattachments doc2))

    assertRight edoc3
    let doc3 = fromRight edoc3
    assertBool "Attachment connected to signatory"
                 (Just (fileid file2) `elem` map signatoryattachmentfile (documentsignatoryattachments doc3))


{-
testRemoveDocumentAttachmentFailsIfNotPreparation :: DB ()
testRemoveDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  edoc <- randomUpdate $ RemoveDocumentAttachment (documentid doc) (FileID 0)
  --assert
  validTest $ assertLeft edoc

testRemoveDocumentAttachmentOk :: DB ()
testRemoveDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- randomUpdate $ RemoveDocumentAttachment (documentid doc) (FileID 0)
  --assert
  validTest $ assertRight edoc
-}

------------------------------------------------

testDocumentFromSignatoryDataFailsDoesntExist :: DB ()
testDocumentFromSignatoryDataFailsDoesntExist = doTimes 10 $ do
  (did, ix, a, b, c, d, e, f, g, aa :: AuthorActor) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData did ix a b c d e f g aa
  validTest $ assertLeft mdoc

testDocumentFromSignatoryDataSucceedsExists :: DB ()
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  (time, ix, a, b, c, d, e, f, g) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc) ix a b c d e f g
          (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))
  validTest $ assertRight mdoc

testTimeoutDocumentNonSignableLeft :: DB ()
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  etdoc <- dbUpdate $ TimeoutDocument (documentid doc) (SystemActor mt)
  validTest $ assertLeft etdoc

testTimeoutDocumentSignableNotPendingLeft :: DB ()
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ \t->TimeoutDocument (documentid doc) (SystemActor t)
  validTest $ assertLeft etdoc

testTimeoutDocumentSignablePendingRight :: DB ()
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  --execute
  etdoc <- randomUpdate $ \t->TimeoutDocument (documentid doc) (SystemActor t)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testTimeoutDocumentSignableNotLeft :: DB ()
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  actor::SystemActor <- rand 10 arbitrary
  etdoc <- randomUpdate $ \d->TimeoutDocument d actor
  validTest $ assertLeft etdoc

testSignDocumentNonSignableLeft :: DB ()
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  etdoc <- randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si (SystemActor t)
  validTest $ assertLeft etdoc

testSignDocumentSignableNotPendingLeft :: DB ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  let Just sl = getSigLinkFor doc author
  etdoc <- randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si (SystemActor t)
  validTest $ assertLeft etdoc

testSignDocumentSignablePendingRight :: DB ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = find (isSignatory &&^ (not . hasSigned)) (documentsignatorylinks doc)
  time <- rand 10 arbitrary
  _ <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor time)
  etdoc <- randomUpdate $ \si -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si (SystemActor time)
  validTest $ do
    assertRight etdoc


testSignDocumentNotLeft :: DB ()
testSignDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ \d sl mh si t -> SignDocument d sl mh si (SystemActor t)
  validTest $ assertLeft etdoc

testPreparationToPendingNotSignableLeft :: DB ()
testPreparationToPendingNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ PreparationToPending (documentid doc) (SystemActor time)
  validTest $ assertLeft etdoc

testPreparationToPendingSignableNotPreparationLeft :: DB ()
testPreparationToPendingSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = documentAllStatuses \\ [Preparation]
         }
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ PreparationToPending (documentid doc) (SystemActor time)
  validTest $ assertLeft etdoc

testPreparationToPendingNotLeft :: DB ()
testPreparationToPendingNotLeft = doTimes 100 $ do
  (time, did) <- rand 10 arbitrary
  etdoc <- randomUpdate $ PreparationToPending did (SystemActor time)
  validTest $ assertLeft etdoc

testPreparationToPendingSignablePreparationRight :: DB ()
testPreparationToPendingSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Preparation]
         , randomDocumentCondition = (any isSignatory . documentsignatorylinks) &&^
          ((==) 1 . length . documentfiles) &&^
          ((==) 1 . length . filter isAuthor . documentsignatorylinks)
         }
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ PreparationToPending (documentid doc) (SystemActor time)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

{-
testAuthorSignDocumentNotSignableLeft :: DB ()
testAuthorSignDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSignDocumentSignableNotPreparationLeft :: DB ()
testAuthorSignDocumentSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPreparation))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSignDocumentNotLeft :: DB ()
testAuthorSignDocumentNotLeft = doTimes 10 $ do
  edoc <- randomUpdate AuthorSignDocument
  validTest $ assertLeft edoc

testAuthorSignDocumentSignablePreparationRight :: DB ()
testAuthorSignDocumentSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isSignable &&^ isPreparation &&^ (not . hasSigned . getAuthorSigLink) &&^ (isSignatory . getAuthorSigLink))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc
-}

testRejectDocumentNotSignableLeft :: DB ()
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing 
           (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))
  validTest $ assertLeft etdoc

testRejectDocumentSignableNotPendingLeft :: DB ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isPending ||^ isAwaitingAuthor)))
  let Just sl = getSigLinkFor doc author
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing 
           (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))
  validTest $ assertLeft etdoc

testRejectDocumentNotLeft :: DB ()
testRejectDocumentNotLeft = doTimes 10 $ do
  (did, time, slid) <- rand 10 arbitrary
  let sa = SignatoryActor time (IPAddress 0) Nothing "hello@hello.com" slid
  etdoc <- randomUpdate $ RejectDocument did slid Nothing sa
  validTest $ assertLeft etdoc

testRejectDocumentSignablePendingRight :: DB ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  let Just sl = getSigLinkFor doc slid
  time <- rand 10 arbitrary
  let sa = SignatoryActor time (IPAddress 0) Nothing (BS.toString $ getEmail sl) slid
  edoc <- randomUpdate $ RejectDocument (documentid doc) slid Nothing sa
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: DB ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks))
  forM_ (documentsignatorylinks doc) $ \sl -> Log.debug $ "maybereadinvite: " ++ show (maybereadinvite sl)
  sl' <- rand 10 $ elements $ documentsignatorylinks doc
  let slid = signatorylinkid sl'
  time <- getMinutesTime
  edoc <- dbUpdate $ MarkInvitationRead (documentid doc) slid 
          (SignatoryActor time (IPAddress 0) (maybesignatory sl') (BS.toString $ getEmail sl') slid)
  validTest $ do
    assertRight edoc
    let Just sl = getSigLinkFor (fromRight edoc) slid
    assertEqual "Invitation read time should be set." (Just time) (maybereadinvite sl)

testMarkInvitationReadDocDoesntExist :: DB ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  (did, slid, time, ip, eml) <- rand 10 arbitrary
  _ <- randomUpdate $ MarkInvitationRead did slid 
       (SignatoryActor time ip Nothing eml slid)
  validTest $ assertSuccess

testMarkDocumentSeenNotSignableLeft :: DB ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }

  validTest (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                (time, ip) <- rand 10 arbitrary
                let sa = SignatoryActor time ip (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl) 
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                assertLeft etdoc)

testMarkDocumentSeenClosedOrPreparationLeft :: DB ()
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         }
  validTest (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                (time, ip) <- rand 10 arbitrary
                let sa = SignatoryActor time ip (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl) 
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                assertLeft etdoc)

testMarkDocumentSeenNotLeft :: DB ()
testMarkDocumentSeenNotLeft = doTimes 10 $ do
  (d, s, m, a :: SignatoryActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ MarkDocumentSeen d s m a
  validTest $ assertLeft etdoc

forEachSignatoryLink :: Document -> (SignatoryLink -> DB ()) -> DB ()
forEachSignatoryLink doc fn =
  let f [] = return ()
      f (sl:sls) = do
        fn sl
        f sls
  in f (documentsignatorylinks doc)

testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight :: DB ()
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                (time, ip) <- rand 10 arbitrary
                let sa = SignatoryActor time ip (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                assertRight etdoc
                let Right tdoc = etdoc
                    Just  tsl  = getSigLinkFor tdoc (signatorylinkid sl)
                assertBool "Signatorylink should be marked seen now." (hasSeen tsl))

testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: DB ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
                (time, ip) <- rand 10 arbitrary
                let sa = SignatoryActor time ip (maybesignatory sl) (BS.toString $ getEmail sl) (signatorylinkid sl)
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh sa
                assertLeft etdoc)

testSetInvitationDeliveryStatusNotSignableLeft :: DB ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  (actor::SystemActor) <- rand 10 arbitrary  
  edoc <- randomUpdate $ \sl st-> SetInvitationDeliveryStatus (documentid doc) sl st actor
  validTest $ assertLeft edoc


testSetInvitationDeliveryStatusNotLeft :: DB ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  (actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ \d s st-> SetInvitationDeliveryStatus d s st actor
  validTest $ assertLeft etdoc

testSetInvitationDeliveryStatusSignableRight :: DB ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  (st, actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc) slid st actor
  validTest $ assertRight etdoc

testSetDocumentTimeoutTimeNotSignableLeft :: DB ()
testSetDocumentTimeoutTimeNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  (time, actor::SystemActor) <- rand 10 arbitrary  
  edoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc) time actor
  validTest $ assertLeft edoc

testSetDocumentTimeoutTimeSignableRight :: DB ()
testSetDocumentTimeoutTimeSignableRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  (time, actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc) time actor
  validTest $ assertRight etdoc

testSetDocumentTimeoutTimeNotLeft :: DB ()
testSetDocumentTimeoutTimeNotLeft = doTimes 10 $ do
  (time, actor::AuthorActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ \did -> SetDocumentTimeoutTime did time actor
  validTest $ assertLeft etdoc

testSetDocumentTagsNotLeft :: DB ()
testSetDocumentTagsNotLeft = doTimes 10 $ do
  (tags, actor::AuthorActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ (\did -> SetDocumentTags did tags actor)
  validTest $ assertLeft etdoc

testSetDocumentTagsRight :: DB ()
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  (tags, time) <- rand 10 arbitrary
  let actor = AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author)
  edoc <- randomUpdate $ SetDocumentTags (documentid doc) tags actor
  validTest $ do
    assertRight edoc
    assertEqual "Tags should be equal" tags $ documenttags (fromRight edoc)

testSetDocumentUINotLeft :: DB ()
testSetDocumentUINotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ (\did ui time -> SetDocumentUI did ui (SystemActor time))
  validTest $ assertLeft etdoc

testSetDocumentUIRight :: DB ()
testSetDocumentUIRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  (ac:: SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ (\ui -> SetDocumentUI (documentid doc) ui ac)
  validTest $ assertRight etdoc

testCloseDocumentSignableAwaitingAuthorJust :: DB ()
testCloseDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor]
         , randomDocumentCondition = const True
         }
  sa :: SystemActor <- rand 10 arbitrary
  let Just sl = getAuthorSigLink doc
  _ <- randomUpdate (MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)  sa)      
  _ <- randomUpdate (\si -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si sa)
  etdoc <- randomUpdate (CloseDocument (documentid doc) sa)
  validTest $ assertRight etdoc

testCloseDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCloseDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = const True
         }
  sa :: SystemActor <- rand 10 arbitrary
  let Just sl = getAuthorSigLink doc
  _ <- randomUpdate (\si -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si sa)
  etdoc <- randomUpdate $ CloseDocument (documentid doc) sa
  validTest $ assertLeft etdoc

testCloseDocumentNotSignableNothing :: DB ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  sa :: SystemActor <- rand 10 arbitrary
  etdoc <- randomUpdate $ CloseDocument (documentid doc) sa
  validTest $ assertLeft etdoc

testCloseDocumentNotNothing :: DB ()
testCloseDocumentNotNothing = doTimes 10 $ do
  sa :: SystemActor <- rand 10 arbitrary
  did :: DocumentID <- rand 10 arbitrary
  etdoc <- randomUpdate $ CloseDocument did sa
  validTest $ assertLeft etdoc


testCancelDocumentSignableAwaitingAuthorJust :: DB ()
testCancelDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor]
         , randomDocumentCondition =  const True
         }
  time <- rand 10 arbitrary
  --let Just sl = getAuthorSigLink doc
  let actor = AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author)
  etdoc <- randomUpdate (CancelDocument (documentid doc) ManualCancel actor)
  validTest $ assertRight etdoc

testCancelDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCancelDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor, Pending]
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel
           (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))

  validTest $ assertRight etdoc

testCancelDocumentNotSignableNothing :: DB ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  time <- rand 10 arbitrary
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  etdoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time (IPAddress 0) (userid author) (BS.toString $ getEmail author))
  validTest $ assertLeft etdoc

testCancelDocumentNotNothing :: DB ()
testCancelDocumentNotNothing = doTimes 10 $ do
  aa :: AuthorActor <- rand 10 arbitrary
  etdoc <- randomUpdate $ (\did -> CancelDocument did ManualCancel aa)
  validTest $ assertLeft etdoc


testPendingToAwaitingAuthorDocumentSignableAwaitingAuthorJust :: DB ()
testPendingToAwaitingAuthorDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = const True
         }

  (actor::SystemActor) <- rand 10 arbitrary
  forM_ (filter (not . isAuthor) $ documentsignatorylinks doc) $ \s -> do
    _ <- randomUpdate (MarkDocumentSeen (documentid doc) (signatorylinkid s) (signatorymagichash s) actor)
    _ <- randomUpdate $ \si -> SignDocument (documentid doc) (signatorylinkid s) (signatorymagichash s) si actor
    return ()
  etdoc <- randomUpdate (PendingToAwaitingAuthor (documentid doc) actor) 
           
  validTest $ assertRight etdoc

testPendingToAwaitingAuthorDocumentSignableNotAwaitingAuthorNothing :: DB ()
testPendingToAwaitingAuthorDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = ((<=) 2 . length . documentsignatorylinks)
         }

  (actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ PendingToAwaitingAuthor (documentid doc) actor

  validTest $ assertLeft etdoc

testPendingToAwaitingAuthorDocumentNotSignableNothing :: DB ()
testPendingToAwaitingAuthorDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  (actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ PendingToAwaitingAuthor (documentid doc) actor
  validTest $ assertLeft etdoc

testPendingToAwaitingAuthorDocumentNotNothing :: DB ()
testPendingToAwaitingAuthorDocumentNotNothing = doTimes 10 $ do
  (actor::SystemActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ (\did -> PendingToAwaitingAuthor did actor)
  validTest $ assertLeft etdoc


testSetDocumentTitleNotLeft :: DB ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  (did, title, actor::AuthorActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ SetDocumentTitle did title actor
  validTest $ assertLeft etdoc

testSetDocumentTitleRight :: DB ()
testSetDocumentTitleRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed)
         }
  let title = BS.fromString "my new cool title"
  actor::AuthorActor <- rand 10 arbitrary
  etdoc <- randomUpdate $ SetDocumentTitle (documentid doc) title actor
  validTest $ do
    assertRight etdoc
    let Right doc' = etdoc
    assertEqual "Title is set properly" title (documenttitle doc')

testSetDocumentDaysToSignNotLeft :: DB ()
testSetDocumentDaysToSignNotLeft = doTimes 10 $ do
  (did, d, actor::AuthorActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ SetDaysToSign did d actor
  validTest $ assertLeft etdoc

testRemoveDocumentDaysToSignNotLeft :: DB ()
testRemoveDocumentDaysToSignNotLeft = doTimes 10 $ do
  (did, actor::AuthorActor) <- rand 10 arbitrary
  etdoc <- randomUpdate $ RemoveDaysToSign did actor
  validTest $ assertLeft etdoc

testSetDocumentDaysToSignRight :: DB ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed) &&^ (isNothing . documentdaystosign)
         }
  actor :: AuthorActor <- rand 10 arbitrary
  let daystosign = 15
  etdoc1 <- randomUpdate $ SetDaysToSign (documentid doc) daystosign actor
  etdoc2 <- randomUpdate $ RemoveDaysToSign (documentid doc) actor
  validTest $ do
    assertRight etdoc1
    assertRight etdoc2
    let Right doc1' = etdoc1
    let Right doc2' = etdoc2
    assertEqual "Days to sign is set properly" (Just daystosign) (documentdaystosign doc1')
    assertEqual "Days to sign removed properly" (Nothing) (documentdaystosign doc2')

assertInvariants :: Document -> DB ()
assertInvariants document = do
  now <- getMinutesTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a

propbitfieldDeriveConvertibleId :: [SignatoryRole] -> Bool
propbitfieldDeriveConvertibleId ss =
  let ss' = nub (sort ss)
  in ss' == convert (convert ss' :: SqlValue)
     
     
testGetDocumentsByCompanyAndTagsCompany :: DB ()
testGetDocumentsByCompanyAndTagsCompany = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  company2 <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  let actor = SystemActor time
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name value] actor
  docs <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company2) []
  docs' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) []
  validTest $ do
    assertEqual "Should have no documents returned" docs []
    assertEqual "Should have 1 document returned" (length docs') 1    
     

testGetDocumentsByCompanyAndTagsFilters :: DB ()
testGetDocumentsByCompanyAndTagsFilters = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  _ <- addRandomDocumentWithAuthor author'
  docs <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name value]
  docs' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) []
  validTest $ do
    assertEqual "Should have no documents returned" docs []
    assertEqual "Should have 1 document returned" (length docs') 1    
    

testGetDocumentsByCompanyAndTagsFinds :: DB ()
testGetDocumentsByCompanyAndTagsFinds = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  let actor = SystemActor time
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name value] actor
  docs <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name value]
  docs' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) []
  validTest $ do
    assertEqual "Should have one document returned" (length docs) 1
    assertEqual "Should have one document returned" (length docs') 1
  
testGetDocumentsByCompanyAndTagsFindsMultiple :: DB ()
testGetDocumentsByCompanyAndTagsFindsMultiple = doTimes 10 $ do
  (name1, value1) <- rand 10 arbitrary
  (name2, value2) <- rand 10 arbitrary
  (name3, value3) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  time <- getMinutesTime
  let actor = SystemActor time  
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'

  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name1 value1, DocumentTag name2 value2] actor
  docs <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name1 value1]  
  docs' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name2 value2]
  docs'' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name1 value1, DocumentTag name2 value2]  
  docs''' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) []
  docs'''' <- dbQuery $ GetDocumentsByCompanyAndTags Nothing (companyid company) [DocumentTag name1 value1, DocumentTag name2 value2, DocumentTag name3 value3]  
  validTest $ do
    assertEqual "Should have one document returned" (length docs) 1  
    assertEqual "Should have one document returned" (length docs') 1
    assertEqual "Should have one document returned" (length docs'') 1
    assertEqual "Should have one document returned" (length docs''') 1
    assertEqual "Should have zero documents returned" (length docs'''') 0    
