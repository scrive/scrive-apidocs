module DocStateTest where

import Control.Logic
import DB.Classes
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import Misc
import IPAddress
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MagicHash
import MinutesTime
import Test.HUnit.Base (Assertion)
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo

import Data.Functor
import Data.Maybe
import Data.Convertible(convert)
import Database.HDBC(SqlValue)
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

docStateTests :: DBEnv -> Test
docStateTests env = testGroup "DocState" [
  dataStructureProperties,
  testThat "ReallyDeleteDocument adds to the log" env testReallyDeleteDocumentEvidenceLog,
  testThat "RejectDocument adds to the log" env testRejectDocumentEvidenceLog,
  testThat "RemoveDaysToSign adds to the log" env testRemoveDaysToSignEvidenceLog,
  testThat "RemoveDocumentAttachment adds to the log" env testRemoveDocumentAttachmentEvidenceLog,
  testThat "ResetSignatoryDetails adds to the log" env testResetSignatoryDetailsEvidenceLog,
  testThat "RestartDocument adds to the log" env testRestartDocumentEvidenceLog,
  testThat "RestoreArchivedDocument adds to the log" env testRestoreArchivedDocumentEvidenceLog,
  testThat "SaveDocumentForUser adds to the log" env testSaveDocumentForUserEvidenceLog,
  testThat "SetDaysToSign adds to the log" env testSetDaysToSignEvidenceLog,
  testThat "SetDocumentAdvancedFunctionality adds to the log" env testSetDocumentAdvancedFunctionalityEvidenceLog,
  testThat "SetDocumentInviteTime adds to the log" env testSetDocumentInviteTimeEvidenceLog,
  testThat "SetDocumentLocale adds to the log" env testSetDocumentLocaleEvidenceLog,
  testThat "SetDocumentTags adds to the log" env testSetDocumentTagsEvidenceLog,
  testThat "SetDocumentTimeoutTime adds to the log" env testSetDocumentTimeoutTimeEvidenceLog,
  testThat "SetDocumentTitle adds to the log" env testSetDocumentTitleEvidenceLog,
  testThat "SetDocumentUI adds to the log" env testSetDocumentUIEvidenceLog,
  testThat "Set ElegitimationIdentification adds to the log" env testSetElegitimationIdentificationEvidenceLog,
  testThat "Set EmailIdentification adds to the log" env testSetEmailIdentificationEvidenceLog,
  testThat "SetInvitationDeliveryStatus adds to the log" env testSetInvitationDeliveryStatusEvidenceLog,
  testThat "SetInviteText adds to the log" env testSetInviteTextEvidenceLog,
  testThat "SignDocument adds to the log" env testSignDocumentEvidenceLog,
  testThat "SignableFromDocumentIDWithUpdatedAuthor adds to the log" env testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog,
  testThat "TemplateFromDocument adds to the log" env testTemplateFromDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" env testTimeoutDocumentEvidenceLog,
  testThat "UpdateFields adds to the log" env testUpdateFieldsEvidenceLog,
  testThat "Documents are shared in company properly" env testGetDocumentsSharedInCompany,

  testThat "Documents sorting SQL syntax is correct" env testGetDocumentsSQLSorted,
 
  testThat "PreparationToPending adds to the log" env testPreparationToPendingEvidenceLog,
  testThat "MarkInvitationRead adds to the log" env testMarkInvitationReadEvidenceLog,
  testThat "MarkDocumentSeen adds to the log" env testMarkDocumentSeenEvidenceLog,
  testThat "ErrorDocument adds to the log" env testErrorDocumentEvidenceLog,
  testThat "DocumentFromSignatoryData adds to the log" env testDocumentFromSignatoryDataEvidenceLog,
  testThat "SaveSigAttachment adds to the log" env testSaveSigAttachmentEvidenceLog,
  testThat "DeleteSigAttachment adds to the log" env testDeleteSigAttachmentEvidenceLog,
  testThat "CloseDocument adds to the log" env testCloseDocumentEvidenceLog,
  testThat "ChangeSignatoryEmailWhenUndelivered adds to the log" env testChangeSignatoryEmailWhenUndeliveredEvidenceLog,
  testThat "ChangeMailfile adds to the log" env testChangeMainfileEvidenceLog,
  testThat "CancelDocument adds to the log" env testCancelDocumentEvidenceLog,
  testThat "AttachSealedFile adds to the log" env testAttachSealedFileEvidenceLog,
  testThat "AttachFile adds to the log" env testAttachFileEvidenceLog,
  testThat "AttachCSVUpload adds to the log" env testAttachCSVUploadEvidenceLog,
  testThat "ArchiveDocumentEvidence adds to the log" env testArchiveDocumentEvidenceLog,
  testThat "AddInvitationEvidence adds to the log" env testAddInvitationEvidenceLog,
  testThat "NewDocument adds to the log" env testNewDocumentEvidenceLog,
  testThat "AddDocumentAttachment adds to the log" env testAddDocumentAttachmentEvidenceLog,
  testThat "GetDocumentsByCompanyWithFiltering filters" env testGetDocumentsByCompanyWithFilteringFilters,
  testThat "GetDocumentsByCompanyWithFiltering finds" env testGetDocumentsByCompanyWithFilteringFinds,
  testThat "GetDocumentsByCompanyWithFiltering finds with multiple" env testGetDocumentsByCompanyWithFilteringFindsMultiple,  
  testThat "GetDocumentsByCompanyWithFiltering finds with company filter" env testGetDocumentsByCompanyWithFilteringCompany,
  testThat "NewDocument inserts a new contract for a single user successfully" env testNewDocumentForNonCompanyUserInsertsANewContract,
  testThat "NewDocument inserts a new contract for a company user successfully" env testNewDocumentForACompanyUserInsertsANewContract,
  testThat "NewDocument inserts a new offer for a single user successfully" env testNewDocumentForNonCompanyUserInsertsANewOffer,
  testThat "NewDocument inserts a new offer for a company user successfully" env testNewDocumentForACompanyUserInsertsANewOffer,
  testThat "NewDocument inserts a new order for a single user successfully" env testNewDocumentForNonCompanyUserInsertsANewOrder,
  testThat "NewDocument inserts a new order for a company user successfully" env testNewDocumentForACompanyUserInsertsANewOrder,
  testThat "NewDocument with mismatching user & company results in left" env testNewDocumentForMismatchingUserAndCompanyFails,

  testThat "CancelDocument cancels a document" env testCancelDocumentCancelsDocument,
  testThat "CancelDocument fails if doc not pending or awaiting author" env testCancelDocumentReturnsLeftIfDocInWrongState,

  testThat "SetDocumentLocale fails when doc doesn't exist" env testSetDocumentLocaleNotLeft,

  testThat "SetDocumentTitle fails when doc doesn't exist" env testSetDocumentTitleNotLeft,
  testThat "SetDocumentTitle succeeds when doc exists and has proper status" env testSetDocumentTitleRight,

  testThat "SetDaysToSign fails when doc doesn't exist" env testSetDocumentDaysToSignNotLeft,
  testThat "SetDaysToSign and RemoveDaysToSign succeed when doc exist and has proper status" env testSetDocumentDaysToSignRight,

  testThat "CloseDocument fails when doc is not signable" env testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" env testCloseDocumentNotNothing,
  testThat "CloseDocument fails when doc is signable and awaiting author" env testCloseDocumentSignableNotAwaitingAuthorNothing,

  testThat "CancelDocument fails when doc is not signable" env testCancelDocumentNotSignableNothing,
  testThat "CancelDocument fails when doc doesn't exist" env testCancelDocumentNotNothing,

  testThat "SetDocumentTags fails when does not exist" env testSetDocumentTagsNotLeft,
  testThat "SetDocumentTags succeeds" env testSetDocumentTagsRight,

  testThat "SetDocumentUI fails when does not exist" env testSetDocumentUINotLeft,
  testThat "SetDocumentUI succeeds" env testSetDocumentUIRight,

  testThat "SetDocumentTimeoutTime fails when does not exist" env testSetDocumentTimeoutTimeNotLeft,
  testThat "SetDocumentTimeoutTime fails when not signable" env testSetDocumentTimeoutTimeNotSignableLeft,
  testThat "SetDocumentTimeoutTime succeeds when signable" env testSetDocumentTimeoutTimeSignableRight,

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
  testThat "SignDocument succeeds when doc is Signable and Pending" env testSignDocumentSignablePendingRight,
  testThat "SignDocument fails when the document is Signable but not in Pending" env testSignDocumentSignableNotPendingLeft,
  testThat "SignDocument fails when document is not signable" env testSignDocumentNonSignableLeft,

  testThat "TimeoutDocument fails when doc doesn't exist" env testTimeoutDocumentSignableNotLeft,
  testThat "TimeoutDocument succeeds when doc is Signable and Pending" env testTimeoutDocumentSignablePendingRight,
  testThat "TimeoutDocument fails when the document is Signable but not in Pending" env testTimeoutDocumentSignableNotPendingLeft,
  testThat "create document and check invariants" env testNewDocumentDependencies,
  testThat "can create new document and read it back with the returned id" env testDocumentCanBeCreatedAndFetchedByID,
  testThat "can create new document and read it back with GetDocumentsByService" env testDocumentCanBeCreatedAndFetchedByAllDocs,

{-
  testThat "when I call update document, it doesn't change the document id" env testDocumentUpdateDoesNotChangeID,
  testThat "when I call update document, i can change the title" env testDocumentUpdateCanChangeTitle,
  -}
  testThat "when I attach a file to a real document in preparation, it returns Right" env testDocumentAttachPreparationRight,
  testThat "when I attach a file to a real document not in preparation, it returns Right" env testDocumentAttachNotPreparationLeft,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" env testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" env testDocumentAttachHasAttachment,

  testThat "when I attach a sealed file to a bad docid, it always returns left" env testNoDocumentAttachSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" env testDocumentAttachSealedPendingRight,


  testThat "when I ChangeMainFile of a real document returns Right" env testDocumentChangeMainFileRight,
  testThat "when I ChangeMainFile of a bad docid, it ALWAYS returns Left" env testNoDocumentChangeMainFileAlwaysLeft,

  {-
  testThat "when I call updateDocument, it fails when the doc doesn't exist" env testNoDocumentUpdateDocumentAlwaysLeft,
  testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" env testNotPreparationUpdateDocumentAlwaysLeft,
  testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" env testPreparationUpdateDocumentAlwaysRight,
-}
  testThat "when I create document from shared template author custom fields are stored" env testCreateFromSharedTemplate,

  testThat "when I call ResetSignatoryDetails, it fails when the doc doesn't exist" env testNoDocumentResetSignatoryDetailsAlwaysLeft,
  testThat "When I call ResetSignatoryDetails with a doc that is not in Preparation, always returns left" env testNotPreparationResetSignatoryDetailsAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" env testPreparationResetSignatoryDetailsAlwaysRight,
  testThat "when I call attachcsvupload with a doc that does not exist, always returns left" env testNoDocumentAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload with a doc that is not in preparation, always returns left" env testNotPreparationAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload and the csvindex is the author, return left" env testPreparationAttachCSVUploadAuthorIndexLeft,
  testThat "when I call attachcsvupload and not existing signatory link, return left" env testPreparationAttachCSVUploadNonExistingSignatoryLink,

  testThat "addDocumentAttachment fails if not in preparation" env testAddDocumentAttachmentFailsIfNotPreparation,
  testThat "addDocumentAttachment doesn't fail if there's no attachments" env testAddDocumentAttachmentOk,

  testThat "removeDocumentAttachment fails if not in preparation" env testRemoveDocumentAttachmentFailsIfNotPreparation,
  testThat "removeDocumentAttachment doesn't fail if there's no attachments" env testRemoveDocumentAttachmentOk,

  testThat "UpdateSigAttachments works as advertised" env testUpdateSigAttachmentsAttachmentsOk,

  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "documentFromSignatoryData fails when document doesn't exist" env testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "documentFromSignatoryData succeeds when document exists" env testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" env testTimeoutDocumentNonSignableLeft,
  testProperty "bitfieldDeriveConvertibleId" propbitfieldDeriveConvertibleId,

  -- archive & doc deletion tests
  testThat "ArchiveDocument fails if the document is pending or awaiting author" env testArchiveDocumentPendingLeft,
  testThat "ArchiveDocument succeeds if the archiving user is the author" env testArchiveDocumentAuthorRight,
  testThat "ArchiveDocument succeeds if the archiving user is a company admin" env testArchiveDocumentCompanyAdminRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the author" env testRestoreArchivedDocumentAuthorRight,
  testThat "RestoreArchivedDocument succeeds if the restoring user is the company admin" env testRestoreArchiveDocumentCompanyAdminRight,
  testThat "ReallyDeleteDocument succeeds if deleted by the author who is a private user" env testReallyDeleteDocumentPrivateAuthorRight,
  testThat "ReallyDeleteDocument succeeds if deleted by a company admin user" env testReallyDeleteDocumentCompanyAdminRight,
-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged

  testThat "ArchiveDocument fails if the archiving user is an unrelated user" env testArchiveDocumentUnrelatedUserLeft,
  testThat "ArchiveDocument fails if the archiving user is just another standard company user" env testArchiveDocumentCompanyStandardLeft,
  testThat "RestoreArchivedDocument fails if the storing user is an unrlated user" env testRestoreArchivedDocumentUnrelatedUserLeft,
  testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user" env testRestoreArchiveDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if deleted by the author who is a standard company user" env testReallyDeleteDocumentCompanyAuthorLeft,
  testThat "ReallyDeleteDocument fails if the deleting user is just another standard company user" env testReallyDeleteDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if the document hasn't been archived" env testReallyDeleteNotArchivedLeft,

  testThat "GetDocumentsByAuthor doesn't return archived docs" env testGetDocumentsByAuthorNoArchivedDocs,
  testThat "GetDocumentsByCompanyWithFiltering doesn't return archived docs" env testGetDocumentsByCompanyWithFilteringNoArchivedDocs,
  testThat "GetDocumentsBySignatory doesn't return archived docs" env testGetDocumentsBySignatoryNoArchivedDocs,
  testThat "GetDeletedDocumentsByUser returns archived docs" env testGetDeletedDocumentsByUserArchivedDocs

  ]

dataStructureProperties :: Test
dataStructureProperties = testGroup "data structure properties" [
  testProperty "signatories are equal with same fields" propSignatoryDetailsEq,
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
  let aa = AuthorActor time noIP (userid singleuser) (getEmail singleuser)
  edoc1 <- randomUpdate $ NewDocument singleuser (Just company) "doc title" (Signable Contract) 0 aa
  let ca = AuthorActor time noIP (userid companyuser) (getEmail companyuser)
  edoc2 <- randomUpdate $ NewDocument companyuser Nothing "doc title" (Signable Contract) 0 ca
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
  _ <- randomUpdate $ \t->SetDaysToSign (documentid doc) (Just 3) (SystemActor t)
  etdoc <- randomUpdate $ \t->SetDaysToSign (documentid doc) Nothing (SystemActor t)
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
  etdoc <- randomUpdate $ \t->SetDaysToSign (documentid doc) (Just 30) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDaysToSignEvidence) lg
  
testSetDocumentAdvancedFunctionalityEvidenceLog :: DB ()
testSetDocumentAdvancedFunctionalityEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (((==) BasicFunctionality . documentfunctionality) &&^ isPreparation)
  etdoc <- randomUpdate $ \t->SetDocumentFunctionality (documentid doc) AdvancedFunctionality (SystemActor t)
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
  _ <- randomUpdate $ \t->SetDocumentLocale (documentid doc) (mkLocaleFromRegion REGION_SE) (SystemActor t)
  etdoc <- randomUpdate $ \t->SetDocumentLocale (documentid doc) (mkLocaleFromRegion REGION_GB) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentLocaleEvidence) lg
  
testSetDocumentTagsEvidenceLog :: DB ()
testSetDocumentTagsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- loop doc
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTagsEvidence) lg
    where loop doc = do
                  ts <- rand 10 arbitrary
                  if documenttags doc == ts
                    then loop doc
                    else randomUpdate $ \t->SetDocumentTags (documentid doc) ts (SystemActor t)

  
testSetDocumentTimeoutTimeEvidenceLog :: DB ()
testSetDocumentTimeoutTimeEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  _ <- randomUpdate $ \t->SetDocumentTimeoutTime (documentid doc) (fromMinutes 0) (SystemActor t)
  etdoc <- randomUpdate $ \t->SetDocumentTimeoutTime (documentid doc) (fromMinutes 10000) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTimeoutTimeEvidence) lg
  
testSetDocumentTitleEvidenceLog :: DB ()
testSetDocumentTitleEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- loop doc
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTitleEvidence) lg
    where loop doc = do
                  title <- rand 10 arbitrary
                  if documenttitle doc == title
                    then loop doc
                    else randomUpdate $ \t->SetDocumentTitle (documentid doc) title (SystemActor t)
  
testSetDocumentUIEvidenceLog :: DB ()
testSetDocumentUIEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- loop doc
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentUIEvidence) lg
    where loop doc = do
                  u <- rand 10 arbitrary
                  if documentui doc == u 
                    then loop doc
                    else randomUpdate $ \t->SetDocumentUI (documentid doc) u (SystemActor t)

  
testSetElegitimationIdentificationEvidenceLog :: DB ()
testSetElegitimationIdentificationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  _ <- randomUpdate $ \t->SetDocumentIdentification (documentid doc) [EmailIdentification] (SystemActor t)
  etdoc <- randomUpdate $ \t->SetDocumentIdentification (documentid doc) [ELegitimationIdentification] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetElegitimationIdentificationEvidence) lg
  
testSetEmailIdentificationEvidenceLog :: DB ()
testSetEmailIdentificationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  _ <- randomUpdate $ \t->SetDocumentIdentification (documentid doc) [ELegitimationIdentification] (SystemActor t)
  etdoc <- randomUpdate $ \t->SetDocumentIdentification (documentid doc) [EmailIdentification] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetEmailIdentificationEvidence) lg
  
testSetInvitationDeliveryStatusEvidenceLog :: DB ()
testSetInvitationDeliveryStatusEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  etdoc <- loop doc sl
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationDeliveryStatusEvidence) lg
    where loop doc sl = do
                  s <- rand 10 arbitrary
                  if invitationdeliverystatus sl == s
                    then loop doc sl
                    else randomUpdate $ \t->SetInvitationDeliveryStatus (documentid doc) (signatorylinkid sl) s (SystemActor t)
  
testSetInviteTextEvidenceLog :: DB ()
testSetInviteTextEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  etdoc <- loop doc
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationTextEvidence) lg
    where loop doc = do
                  i <- rand 10 arbitrary
                  if documentinvitetext doc == i
                    then loop doc
                    else randomUpdate $ \t -> SetInviteText (documentid doc) i (SystemActor t)
  
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
  _<- randomUpdate $ \t->SetInviteText (documentid doc) "" (SystemActor t)
  _<- randomUpdate $ \t->SetInviteText (documentid doc) "new invite text" (SystemActor t)
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
      k = unMagicHash $ signatorymagichash sl
      mh = unsafeMagicHash (k + 1)
  etdoc <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  assertRight etdoc
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  _ <- randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing (SystemActor t)  
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (SystemActor t)
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh (SystemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  let n = length (filter (\e -> evType e == MarkDocumentSeenEvidence) lg)
  assertBool ("Should have 3 seen events, but found " ++ show n) $ 3 == n

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
  randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (SystemActor t)
  etdoc <- randomUpdate $ \t a b c d e f -> DocumentFromSignatoryData (documentid doc) a b c d e f [] (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromRight etdoc)
  assertJust $ find (\e -> evType e == AuthorUsesCSVEvidence)    lg

testSaveSigAttachmentEvidenceLog :: DB ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (SystemActor t)
  _ <- randomUpdate $ \t->PreparationToPending (documentid doc) (SystemActor t)
  etdoc <- randomUpdate $ \t->SaveSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) "attachment" (fileid file) (SystemActor t)
  assertRight etdoc
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SaveSigAttachmentEvidence) lg
  
testDeleteSigAttachmentEvidenceLog :: DB ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _<-randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Just $ (fileid file)
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (SystemActor t)
  etdoc <- randomUpdate $ \t->DeleteSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) (fileid file) (SystemActor t)
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
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
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
  etdoc <- randomUpdate $ \t-> ChangeSignatoryEmailWhenUndelivered (documentid doc) (signatorylinkid sl) Nothing "email@email.com" (SystemActor t)
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
  let aa = AuthorActor time noIP (userid user) (getEmail user)
  edoc <- randomUpdate $ NewDocument user Nothing title doctype 0 aa
  return (user, time, edoc)
performNewDocumentWithRandomUser (Just company) doctype title = do
  user <- addNewRandomCompanyUser (companyid company) False
  time <- getMinutesTime
  let aa = AuthorActor time noIP (userid user) (getEmail user)
  edoc <- randomUpdate $ NewDocument user (Just company) title doctype 0 aa
  return (user, time, edoc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> Bool -> (User, MinutesTime, Either String Document) -> DB (Maybe (DB ()))
assertGoodNewDocument mcompany doctype title authorsigns (user, time, edoc) = do
  let (Right doc) = edoc
  validTest $ do
    assertRight edoc
    assertEqual "Correct title" title (documenttitle doc)
    assertEqual "Correct type" doctype (documenttype doc)
    assertEqual "Doc has user's region" (getRegion user) (getRegion doc)
    assertEqual "Doc creation time" time (documentctime doc)
    assertEqual "Doc modification time" time (documentmtime doc)
    assertEqual "Doc has user's service" (userservice user) (documentservice doc)
    assertEqual "No author attachments" [] (documentauthorattachments doc)
    assertEqual "No sig attachments" [] (concatMap signatoryattachments $ documentsignatorylinks doc)
    assertEqual "Uses email identification only" [EmailIdentification] (documentallowedidtypes doc)
    assertEqual "Doc has user's footer" (customfooter $ usersettings user) (documentmailfooter $ documentui doc)
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
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ isPending)
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time noIP (userid user) (getEmail user))
  when (isLeft edoc) $ Log.debug (fromLeft edoc)
  assertRight edoc
  let (Right canceleddoc) = edoc
  let doNotCompareStatusClass x = x { signatorylinkstatusclass = SCDraft }
  validTest $ do
    assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
    assertEqual "Updated modification time" time (documentmtime canceleddoc)
    assertEqual "Matching cancellation reason" (Just ManualCancel) (documentcancelationreason canceleddoc)
    assertEqual "Siglinks are unchanged" 
                  (map doNotCompareStatusClass (documentsignatorylinks doc))
                  (map doNotCompareStatusClass (documentsignatorylinks canceleddoc))
    assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: DB ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ not . isPending)
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time noIP (userid user) (getEmail user))
  validTest $ assertLeft edoc

testSignatories1 :: Assertion
testSignatories1 =
  let s1 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField FirstNameFT "Eric" []
                                               ,SignatoryField LastNameFT "Normand" []
                                                ]
                            }
      s2 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField LastNameFT "Normand" []
                                               ,SignatoryField FirstNameFT "Eric" []
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
  doc <- addRandomDocumentWithAuthorAndCondition author isPending
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
  validTest $ assertBool "Not admin can only delete drafts" (isLeft etdoc || Preparation == documentstatus doc)

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

testGetDocumentsByCompanyWithFilteringNoArchivedDocs :: DB ()
testGetDocumentsByCompanyWithFilteringNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (\u -> GetDocumentsByCompanyWithFiltering (fromJust $ usercompany u) [])

testGetDocumentsBySignatoryNoArchivedDocs :: DB ()
testGetDocumentsBySignatoryNoArchivedDocs =
  checkQueryDoesntContainArchivedDocs (GetDocumentsBySignatory [Contract, Offer, Order]. userid)

checkQueryDoesntContainArchivedDocs :: DBQuery q [Document] => (User -> q) -> DB ()
checkQueryDoesntContainArchivedDocs qry = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> (isPreparation d || isClosed d) && (isSignable d))
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
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (isPreparation ||^ isClosed))
  docsbeforearchive <- dbQuery (qry author)
  assertEqual "Expecting no docs before archive" [] (map documentid docsbeforearchive)
  _ <- randomUpdate $ \t -> ArchiveDocument author (documentid doc) (SystemActor t)
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
  let aa = AuthorActor now noIP (userid author) (getEmail author)
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany (fromSNN title) doctype 0 aa)
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
  let aa = AuthorActor now noIP (userid author) (getEmail author)
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany (fromSNN title) doctype 0 aa)
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
  let aa = AuthorActor now noIP (userid author) (getEmail author)
  edoc <- randomUpdate $ (\title processtype -> NewDocument author mcompany (fromSNN title) (Signable processtype) 0 aa)

  let doc = case edoc of
          Left msg -> error $ show msg
          Right d -> d
  docs <- dbQuery $ GetDocumentsByService Nothing
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
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

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
  slid <- unsafeSignatoryLinkID <$> rand 10 arbitrary
  --execute
  edoc <- dbUpdate $ AttachCSVUpload (documentid doc) 
          slid csvupload (SystemActor time)
  --assert
  validTest $ assertLeft edoc

testGetDocumentsSharedInCompany :: DB ()
testGetDocumentsSharedInCompany = doTimes 10 $ do
  -- two companies, two users per company, two users outside of company
  -- each having a document here
  company1 <- addNewCompany
  company2 <- addNewCompany
  user1' <- addNewRandomAdvancedUser
  user2' <- addNewRandomAdvancedUser
  _ <- dbUpdate $ SetUserCompany (userid user1') (Just (companyid company1))
  Just user1 <- dbQuery $ GetUserByID (userid user1')
  _ <- dbUpdate $ SetUserCompany (userid user2') (Just (companyid company1))
  Just user2 <- dbQuery $ GetUserByID (userid user2')
  user3' <- addNewRandomAdvancedUser
  user4' <- addNewRandomAdvancedUser
  _ <- dbUpdate $ SetUserCompany (userid user3') (Just (companyid company2))
  Just user3 <- dbQuery $ GetUserByID (userid user3')
  _ <- dbUpdate $ SetUserCompany (userid user4') (Just (companyid company2))
  Just user4 <- dbQuery $ GetUserByID (userid user4')
  user5 <- addNewRandomAdvancedUser
  user6 <- addNewRandomAdvancedUser

  docid1 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user1 (isTemplate)
  docid2 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user2 (isTemplate)
  docid3 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user3 (isTemplate)
  docid4 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user4 (isTemplate)
  docid5 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user5 (isTemplate)
  docid6 <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user6 (isTemplate)

  -- user1: owns doc1, sees doc2
  -- user2: owns doc2, sees doc1
  -- user3: owns doc3,
  -- user4: owns doc4, sees doc3
  -- user5: owns doc5
  -- user6: owns doc6

  _ <- dbUpdate $ SetDocumentSharing [docid4] False
  _ <- dbUpdate $ SetDocumentSharing [docid1, docid2, docid3, docid5, docid6] True

  dlist1 <- dbQuery $ GetAvailableTemplates (userid user1) [Offer, Order, Contract]
  dlist2 <- dbQuery $ GetAvailableTemplates (userid user2) [Offer, Order, Contract]
  dlist3 <- dbQuery $ GetAvailableTemplates (userid user3) [Offer, Order, Contract]
  dlist4 <- dbQuery $ GetAvailableTemplates (userid user4) [Offer, Order, Contract]
  dlist5 <- dbQuery $ GetAvailableTemplates (userid user5) [Offer, Order, Contract]
  dlist6 <- dbQuery $ GetAvailableTemplates (userid user6) [Offer, Order, Contract]

  validTest $ do
    assertEqual "Documents not shared in user without company (X) by user 5" 1 (length dlist5)
    assertEqual "Documents not shared in user without company (Y) by user 6" 1 (length dlist6)
    assertEqual "Documents properly shared in company (2) by user 3" 1 (length dlist3)
    assertEqual "Documents properly shared in company (2) by user 4" 2 (length dlist4)
    assertEqual "Documents properly shared in company (1) by user 1" 2 (length dlist1)
    assertEqual "Documents properly shared in company (1) by user 2" 2 (length dlist2)


testGetDocumentsSQLSorted :: DB ()
testGetDocumentsSQLSorted = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  _doc <- addRandomDocumentWithAuthorAndCondition author (const True)

  let domains = [ DocumentsOfAuthor (userid author)
                , DocumentsOfAuthorDeleted (userid author)
                , DocumentsOfAuthorDeleteValue (userid author) True
                , DocumentsForSignatory (userid author)
                , DocumentsForSignatoryDeleted (userid author)
                , DocumentsForSignatoryDeleteValue (userid author) True
                , TemplatesOfAuthor (userid author)
                , TemplatesOfAuthorDeleted (userid author)
                , TemplatesOfAuthorDeleteValue (userid author) True
                , TemplatesSharedInUsersCompany (userid author)
                  -- , DocumentsOfService (Maybe ServiceID)
                  -- , DocumentsOfCompany CompanyID
                , AttachmentsOfAuthorDeleteValue (userid author) True
                ]
      filters = []
  count <- dbQuery $ GetDocumentsCount domains filters
  docs <- dbQuery $ GetDocuments domains filters
            [ Desc DocumentOrderByTitle
            , Desc DocumentOrderByMTime
            , Desc DocumentOrderByStatusClass
            , Desc DocumentOrderByType
            , Desc DocumentOrderByProcess
            ]
            (DocumentPagination 0 maxBound)
  validTest $ do
    assertEqual "GetDocuments and GetDocumentsCount are compatible" (length docs) count

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
  edoc <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (unsafeFileID 0) (SystemActor t)
  --assert
  validTest $ assertLeft edoc

testRemoveDocumentAttachmentOk :: DB ()
testRemoveDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- randomUpdate $ \t -> RemoveDocumentAttachment (documentid doc) (unsafeFileID 0) (SystemActor t)
  --assert
  validTest $ assertRight edoc

---------------------------------------------------------------------

testUpdateSigAttachmentsAttachmentsOk :: DB ()
testUpdateSigAttachmentsAttachmentsOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file1 <- addNewRandomFile
  file2 <- addNewRandomFile
  --execute
  let email1 = "g1@g.com"
      name1 = "att1"
  let att1 = SignatoryAttachment { signatoryattachmentfile = Just (fileid file1)
                                 , signatoryattachmentname = name1
                                 , signatoryattachmentdescription = "att1 description"
                                 }
  let att2 = SignatoryAttachment { signatoryattachmentfile = Nothing
                                 , signatoryattachmentname = "att2"
                                 , signatoryattachmentdescription = "att2 description"
                                 }
  (time, sl) <- rand 10 arbitrary
  let sa = SignatoryActor time noIP Nothing email1 sl
  randomUpdate $ SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) [att1, att2] sa
  edoc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  edoc2 <- randomUpdate $ DeleteSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) (fileid file1) sa

  edoc3 <- randomUpdate $ SaveSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) name1 (fileid file2) sa

  --assert
  validTest $ do
    assertJust edoc1
    let doc1 = fromJust edoc1
    assertEqual "Both attachments were attached" 2 (length (signatoryattachments $ (documentsignatorylinks doc1) !! 0))
    assertRight edoc2
    let doc2 = fromRight edoc2
    assertBool "All signatory attachments are not connected to files" (all (isNothing . signatoryattachmentfile) 
                                                                           (signatoryattachments $ (documentsignatorylinks doc2) !! 0))

    assertRight edoc3
    let doc3 = fromRight edoc3
    assertBool "Attachment connected to signatory"
                 (Just (fileid file2) `elem` map signatoryattachmentfile (signatoryattachments $ (documentsignatorylinks doc3) !! 0))


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
  (did, a, b, c, d, e, f, g, aa :: AuthorActor) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData did a b c d e f g aa
  validTest $ assertLeft mdoc

testDocumentFromSignatoryDataSucceedsExists :: DB ()
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  (time, a, b, c, d, e, f, g) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc) a b c d e f g
          (AuthorActor time noIP (userid author) (getEmail author))
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
           (AuthorActor time noIP (userid author) (getEmail author))
  validTest $ assertLeft etdoc

testRejectDocumentSignableNotPendingLeft :: DB ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ not . isPending)
  let Just sl = getSigLinkFor doc author
  time <- rand 10 arbitrary
  etdoc <- randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing 
           (AuthorActor time noIP (userid author) (getEmail author))
  validTest $ assertLeft etdoc

testRejectDocumentNotLeft :: DB ()
testRejectDocumentNotLeft = doTimes 10 $ do
  (did, time, slid) <- rand 10 arbitrary
  let sa = SignatoryActor time noIP Nothing "hello@hello.com" slid
  etdoc <- randomUpdate $ RejectDocument did slid Nothing sa
  validTest $ assertLeft etdoc

testRejectDocumentSignablePendingRight :: DB ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  let Just sl = getSigLinkFor doc slid
  time <- rand 10 arbitrary
  let sa = SignatoryActor time noIP Nothing (getEmail sl) slid
  edoc <- randomUpdate $ RejectDocument (documentid doc) slid Nothing sa
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: DB ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks))

  sl' <- rand 10 $ elements $ documentsignatorylinks doc
  let slid = signatorylinkid sl'
  time <- getMinutesTime
  edoc <- dbUpdate $ MarkInvitationRead (documentid doc) slid 
          (SignatoryActor time noIP (maybesignatory sl') (getEmail sl') slid)
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
                let sa = SignatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
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
                let sa = SignatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
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
                let sa = SignatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
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
                let sa = SignatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
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
  let actor = AuthorActor time noIP (userid author) (getEmail author)
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

testCancelDocumentNotSignableNothing :: DB ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  time <- rand 10 arbitrary
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  etdoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel (AuthorActor time noIP (userid author) (getEmail author))
  validTest $ assertLeft etdoc

testCancelDocumentNotNothing :: DB ()
testCancelDocumentNotNothing = doTimes 10 $ do
  aa :: AuthorActor <- rand 10 arbitrary
  etdoc <- randomUpdate $ (\did -> CancelDocument did ManualCancel aa)
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
  let title = "my new cool title"
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

testSetDocumentDaysToSignRight :: DB ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed) &&^ (isNothing . documentdaystosign)
         }
  actor :: AuthorActor <- rand 10 arbitrary
  let daystosign = 15
  etdoc1 <- randomUpdate $ SetDaysToSign (documentid doc) (Just daystosign) actor
  etdoc2 <- randomUpdate $ SetDaysToSign (documentid doc) Nothing actor
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
     
     
testGetDocumentsByCompanyWithFilteringCompany :: DB ()
testGetDocumentsByCompanyWithFilteringCompany = doTimes 10 $ do
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
  docs <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company2) []
  docs' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) []
  validTest $ do
    assertEqual "Should have no documents returned" docs []
    assertEqual "Should have 1 document returned" (length docs') 1    
     

testGetDocumentsByCompanyWithFilteringFilters :: DB ()
testGetDocumentsByCompanyWithFilteringFilters = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  _ <- addRandomDocumentWithAuthor author'
  docs <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name value]]
  docs' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) []
  validTest $ do
    assertEqual "Should have no documents returned" docs []
    assertEqual "Should have 1 document returned" (length docs') 1    
    

testGetDocumentsByCompanyWithFilteringFinds :: DB ()
testGetDocumentsByCompanyWithFilteringFinds = doTimes 10 $ do
  (name, value) <- rand 10 arbitrary
  company <- addNewCompany
  author <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  time <- getMinutesTime
  let actor = SystemActor time
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name value] actor
  docs <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name value]]
  docs' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) []
  validTest $ do
    assertEqual "Should have one document returned" 1 (length docs)
    assertEqual "Should have one document returned" 1 (length docs')
  
testGetDocumentsByCompanyWithFilteringFindsMultiple :: DB ()
testGetDocumentsByCompanyWithFilteringFindsMultiple = doTimes 10 $ do
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
  docs <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name1 value1]]
  docs' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name2 value2]]
  docs'' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2]]
  docs''' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) []
  docs'''' <- dbQuery $ GetDocumentsByCompanyWithFiltering (companyid company) [DocumentFilterByTags [DocumentTag name1 value1, DocumentTag name2 value2, DocumentTag name3 value3]]
  validTest $ do
    assertEqual "Should have one document returned" 1 (length docs)
    assertEqual "Should have one document returned" 1 (length docs')
    assertEqual "Should have one document returned" 1 (length docs'')
    assertEqual "Should have one document returned" 1 (length docs''')
    assertEqual "Should have zero documents returned" 0 (length docs'''')
