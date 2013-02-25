module DocStateTest where

import Control.Arrow (first)
import Control.Logic
import qualified Data.ByteString as BS
import DB
import User.Model
import Doc.Model
import Doc.DocUtils
import Doc.DocStateData
import qualified Doc.Screenshot as Screenshot
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import IPAddress
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import TestKontra
import DB.SQL2
import Doc.SignatoryLinkID
import Company.Model
import Doc.Invariants
import MagicHash
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
  testThat "RejectDocument adds to the log" env testRejectDocumentEvidenceLog,
  testThat "RemoveDocumentAttachment adds to the log" env testRemoveDocumentAttachmentEvidenceLog,
  testThat "ResetSignatoryDetails adds to the log" env testResetSignatoryDetailsEvidenceLog,
  testThat "RestartDocument adds to the log" env testRestartDocumentEvidenceLog,
  testThat "RestoreArchivedDocument adds to the log" env testRestoreArchivedDocumentEvidenceLog,
  testThat "SetDaysToSign adds to the log" env testSetDaysToSignEvidenceLog,
  testThat "SetDocumentInviteTime adds to the log" env testSetDocumentInviteTimeEvidenceLog,
  testThat "SetDocumentLang adds to the log" env testSetDocumentLangEvidenceLog,
  testThat "SetDocumentTags adds to the log" env testSetDocumentTagsEvidenceLog,
  testThat "SetDocumentTitle adds to the log" env testSetDocumentTitleEvidenceLog,
  testThat "Set ELegAuthentication adds to the log" env testSetElegitimationAuthenticationEvidenceLog,
  testThat "Set StandardAuthentication adds to the log" env testSetStandardAuthenticationEvidenceLog,
  testThat "Set PadDelivery adds to the log" env testSetPadDeliveryEvidenceLog,
  testThat "Set EmailDelivery adds to the log" env testSetEmailDeliveryEvidenceLog,
  testThat "SetInvitationDeliveryStatus adds to the log" env testSetInvitationDeliveryStatusEvidenceLog,
  testThat "SetInviteText adds to the log" env testSetInviteTextEvidenceLog,
  testThat "SignDocument adds to the log" env testSignDocumentEvidenceLog,
  testThat "SignableFromDocumentIDWithUpdatedAuthor adds to the log" env testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog,
  testThat "TemplateFromDocument adds to the log" env testTemplateFromDocumentEvidenceLog,
  testThat "TimeoutDocument adds to the log" env testTimeoutDocumentEvidenceLog,
  testThat "UpdateFields adds to the log" env testUpdateFieldsEvidenceLog,
  testThat "Documents are shared in company properly" env testGetDocumentsSharedInCompany,
  testThat "SetDocumentUnsavedDraft and filtering based on unsaved_draft works" env testSetDocumentUnsavedDraft,
  testThat "Documents sorting SQL syntax is correct" env testGetDocumentsSQLSorted,
  testThat "Documents searching by text works" env testGetDocumentsSQLTextFiltered,

  testThat "PreparationToPending adds to the log" env testPreparationToPendingEvidenceLog,
  testThat "MarkInvitationRead adds to the log" env testMarkInvitationReadEvidenceLog,
  testThat "MarkDocumentSeen adds to the log" env testMarkDocumentSeenEvidenceLog,
  testThat "ErrorDocument adds to the log" env testErrorDocumentEvidenceLog,
  testThat "DocumentFromSignatoryData adds to the log" env testDocumentFromSignatoryDataEvidenceLog,
  testThat "SaveSigAttachment adds to the log" env testSaveSigAttachmentEvidenceLog,
  testThat "DeleteSigAttachment will not work after signing" env testDeleteSigAttachmentAlreadySigned,
  testThat "DeleteSigAttachment adds to the log" env testDeleteSigAttachmentEvidenceLog,
  testThat "CloseDocument adds to the log" env testCloseDocumentEvidenceLog,
  testThat "ChangeSignatoryEmailWhenUndelivered adds to the log" env testChangeSignatoryEmailWhenUndeliveredEvidenceLog,
  testThat "CancelDocument adds to the log" env testCancelDocumentEvidenceLog,
  testThat "AttachSealedFile adds to the log" env testAttachSealedFileEvidenceLog,
  testThat "AttachFile adds to the log" env testAttachFileEvidenceLog,
  testThat "AttachCSVUpload adds to the log" env testAttachCSVUploadEvidenceLog,
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

  testThat "SetDocumentUI fails when does not exist" env testSetDocumentUINotLeft,
  testThat "SetDocumentUI succeeds" env testSetDocumentUIRight,

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

  --testThat "when I call update document, it doesn't change the document id" env testDocumentUpdateDoesNotChangeID,
  --testThat "when I call update document, i can change the title" env testDocumentUpdateCanChangeTitle,

  testThat "when I attach a file to a real document in preparation, it returns Right" env testDocumentAttachPreparationRight,
  testThat "when I attach a file to a real document not in preparation, it returns Right" env testDocumentAttachNotPreparationLeft,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" env testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" env testDocumentAttachHasAttachment,

  testThat "when I attach a sealed file to a bad docid, it always returns left" env testNoDocumentAttachSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" env testDocumentAttachSealedPendingRight,

  --testThat "when I call updateDocument, it fails when the doc doesn't exist" env testNoDocumentUpdateDocumentAlwaysLeft,
  --testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" env testNotPreparationUpdateDocumentAlwaysLeft,
  --testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" env testPreparationUpdateDocumentAlwaysRight,

  testThat "when I create document from shared template author custom fields are stored" env testCreateFromSharedTemplate,
  testThat "when I create document from template company name is taken from company" env testCreateFromTemplateCompanyField,
  
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
  testThat "removeDocumentAttachment return False if there's no attachments" env testRemoveDocumentAttachmentOk,

  testThat "UpdateSigAttachments works as advertised" env testUpdateSigAttachmentsAttachmentsOk,

  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "DocumentFromSignatoryData fails when document doesn't exist" env testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "DocumentFromSignatoryData succeeds when document exists" env testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" env testTimeoutDocumentNonSignableLeft,

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
  testThat "RestoreArchivedDocument fails if the storing user is an unrelated user" env testRestoreArchivedDocumentUnrelatedUserLeft,
  testThat "RestoreArchivedDocument fails if the restoring user is just another standard company user" env testRestoreArchiveDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if deleted by the author who is a standard company user" env testReallyDeleteDocumentCompanyAuthorLeft,
  testThat "ReallyDeleteDocument fails if the deleting user is just another standard company user" env testReallyDeleteDocumentCompanyStandardLeft,
  testThat "ReallyDeleteDocument fails if the document hasn't been archived" env testReallyDeleteNotArchivedLeft,

  testThat "GetDocumentsByAuthor doesn't return archived docs" env testGetDocumentsByAuthorNoArchivedDocs,
  testThat "When document is signed it's status class is signed" env testStatusClassSignedWhenAllSigned,
  testThat "When document is pending and some invitation is undelivered it's status is undelivered" env testStatusClassSignedWhenAllSigned
  ]

dataStructureProperties :: Test
dataStructureProperties = testGroup "data structure properties" [
  testProperty "signatories are equal with same fields" propSignatoryDetailsEq,
  testProperty "signatories are different with different fields" propSignatoryDetailsNEq,
  testCase "given example" testSignatories1
  ]

testNewDocumentForNonCompanyUserInsertsANewContract :: TestEnv ()
testNewDocumentForNonCompanyUserInsertsANewContract = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc title"
  assertGoodNewDocument Nothing (Signable Contract) "doc title" result

testNewDocumentForACompanyUserInsertsANewContract :: TestEnv ()
testNewDocumentForACompanyUserInsertsANewContract = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Contract) "doc title"
  assertGoodNewDocument (Just company) (Signable Contract) "doc title" result

testNewDocumentForNonCompanyUserInsertsANewOffer :: TestEnv ()
testNewDocumentForNonCompanyUserInsertsANewOffer = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Offer) "doc title"
  assertGoodNewDocument Nothing (Signable Offer) "doc title" result

testNewDocumentForACompanyUserInsertsANewOffer :: TestEnv ()
testNewDocumentForACompanyUserInsertsANewOffer = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Offer) "doc title"
  assertGoodNewDocument (Just company) (Signable Offer) "doc title" result

testNewDocumentForNonCompanyUserInsertsANewOrder :: TestEnv ()
testNewDocumentForNonCompanyUserInsertsANewOrder = doTimes 10 $ do
  result <- performNewDocumentWithRandomUser Nothing (Signable Order) "doc title"
  assertGoodNewDocument Nothing (Signable Order) "doc title" result

testNewDocumentForACompanyUserInsertsANewOrder :: TestEnv ()
testNewDocumentForACompanyUserInsertsANewOrder = doTimes 10 $ do
  company <- addNewCompany
  result <- performNewDocumentWithRandomUser (Just company) (Signable Offer) "doc title"
  assertGoodNewDocument (Just company) (Signable Offer) "doc title" result

testRejectDocumentEvidenceLog :: TestEnv ()
testRejectDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  randomUpdate $ \m t->RejectDocument (documentid doc) (signatorylinkid sl) m (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RejectDocumentEvidence) lg

testRemoveDocumentAttachmentEvidenceLog :: TestEnv ()
testRemoveDocumentAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _ <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (systemActor t)
  success <- randomUpdate $ \t->RemoveDocumentAttachment (documentid doc) (fileid file) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RemoveDocumentAttachmentEvidence) lg

testResetSignatoryDetailsEvidenceLog :: TestEnv ()
testResetSignatoryDetailsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- randomUpdate $ \sd t->ResetSignatoryDetails (documentid doc) [sd { signatoryisauthor = True }] (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  let pos = [AddSignatoryEvidence, RemoveSignatoryEvidence, AddFieldEvidence, RemoveFieldEvidence, ChangeFieldEvidence]
  assertJust $ find (\e -> evType e `elem` pos) lg

testRestartDocumentEvidenceLog :: TestEnv ()
testRestartDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  randomUpdate $ \t->CancelDocument (documentid doc) ManualCancel (systemActor t)
  Just cdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  mdoc <- randomUpdate $ \t->RestartDocument cdoc (systemActor t)
  assertJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid $ fromJust mdoc)
  assertJust $ find (\e -> evType e == RestartDocumentEvidence) lg
  assertJust $ find (\e -> evType e == CancelDocumentEvidence) lg
  lg2 <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == CancelDocumentEvidence) lg2

testRestoreArchivedDocumentEvidenceLog :: TestEnv ()
testRestoreArchivedDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  _ <- randomUpdate $ \t -> ArchiveDocument (userid author) (documentid doc) (systemActor t)
  randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == RestoreArchivedDocumentEvidence) lg

testSetDaysToSignEvidenceLog :: TestEnv ()
testSetDaysToSignEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- randomUpdate $ \t->SetDaysToSign (documentid doc) 30 (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDaysToSignEvidence) lg

testSetDocumentInviteTimeEvidenceLog :: TestEnv ()
testSetDocumentInviteTimeEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  now <- getMinutesTime
  let t = 1 `minutesAfter` fromMaybe now (signtime <$> documentinvitetime doc)
  success <- dbUpdate $ SetDocumentInviteTime (documentid doc) t (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentInviteTimeEvidence) lg

testSetDocumentLangEvidenceLog :: TestEnv ()
testSetDocumentLangEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success1 <- randomUpdate $ \t->SetDocumentLang (documentid doc) LANG_SV (systemActor t)
  success2 <- randomUpdate $ \t->SetDocumentLang (documentid doc) LANG_EN (systemActor t)
  assert success1
  assert success2
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentLangEvidence) lg

testSetDocumentTagsEvidenceLog :: TestEnv ()
testSetDocumentTagsEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- loop doc
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTagsEvidence) lg
    where loop doc = do
                  ts <- S.fromList <$> rand 10 arbitrary
                  if documenttags doc == ts
                    then loop doc
                    else randomUpdate $ \t->SetDocumentTags (documentid doc) ts (systemActor t)


testSetDocumentTitleEvidenceLog :: TestEnv ()
testSetDocumentTitleEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- loop doc
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetDocumentTitleEvidence) lg
    where loop doc = do
                  title <- rand 10 arbitrary
                  if documenttitle doc == title
                    then loop doc
                    else randomUpdate $ \t->SetDocumentTitle (documentid doc) title (systemActor t)

testSetElegitimationAuthenticationEvidenceLog :: TestEnv ()
testSetElegitimationAuthenticationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success1 <- randomUpdate $ \t->SetDocumentAuthenticationMethod (documentid doc) StandardAuthentication (systemActor t)
  success2 <- randomUpdate $ \t->SetDocumentAuthenticationMethod (documentid doc) ELegAuthentication (systemActor t)
  assert success1
  assert success2
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetELegAuthenticationMethodEvidence) lg

testSetStandardAuthenticationEvidenceLog :: TestEnv ()
testSetStandardAuthenticationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success1 <- randomUpdate $ \t->SetDocumentAuthenticationMethod (documentid doc) ELegAuthentication (systemActor t)
  success2 <- randomUpdate $ \t->SetDocumentAuthenticationMethod (documentid doc) StandardAuthentication (systemActor t)
  assert success1
  assert success2
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetStandardAuthenticationMethodEvidence) lg

testSetPadDeliveryEvidenceLog :: TestEnv ()
testSetPadDeliveryEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success1 <- randomUpdate $ \t->SetDocumentDeliveryMethod (documentid doc) EmailDelivery (systemActor t)
  success2 <- randomUpdate $ \t->SetDocumentDeliveryMethod (documentid doc) PadDelivery (systemActor t)
  assert success1
  assert success2
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetPadDeliveryMethodEvidence) lg

testSetEmailDeliveryEvidenceLog :: TestEnv ()
testSetEmailDeliveryEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success1 <- randomUpdate $ \t->SetDocumentDeliveryMethod (documentid doc) PadDelivery (systemActor t)
  success2 <- randomUpdate $ \t->SetDocumentDeliveryMethod (documentid doc) EmailDelivery (systemActor t)
  assert success1
  assert success2
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetEmailDeliveryMethodEvidence) lg

testSetInvitationDeliveryStatusEvidenceLog :: TestEnv ()
testSetInvitationDeliveryStatusEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  success <- loop doc sl
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationDeliveryStatusEvidence) lg
    where loop doc sl = do
                  s <- rand 10 arbitrary
                  if invitationdeliverystatus sl == s
                    then loop doc sl
                    else randomUpdate $ \t->SetInvitationDeliveryStatus (documentid doc) (signatorylinkid sl) s (systemActor t)

testSetInviteTextEvidenceLog :: TestEnv ()
testSetInviteTextEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- loop doc
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SetInvitationTextEvidence) lg
    where loop doc = do
                  i <- rand 10 arbitrary
                  if documentinvitetext doc == i
                    then loop doc
                    else randomUpdate $ \t -> SetInviteText (documentid doc) i (systemActor t)

getScreenshots :: MonadIO m => m SignatoryScreenshots.T
getScreenshots = do
  now <- getMinutesTime
  first_ <- liftIO $ BS.readFile "test/screenshots/s1.jpg"
  signing <- liftIO $ BS.readFile "test/screenshots/s2.jpg"
  let mkss i = Just (now, Screenshot.T{ Screenshot.mimetype = "image/jpeg"
                                      , Screenshot.image = Binary i
                                      })
  return $ SignatoryScreenshots.empty{ SignatoryScreenshots.first = mkss first_
                                     , SignatoryScreenshots.signing = mkss signing
                                     }

testSignDocumentEvidenceLog :: TestEnv ()
testSignDocumentEvidenceLog = do
  author <- addNewRandomUser
  screenshots <- getScreenshots
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing screenshots (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SignDocumentEvidence) lg

testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog :: TestEnv ()
testSignableFromDocumentIDWithUpdatedAuthorEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isTemplate &&^ isPreparation)
  _<- randomUpdate $ \t->SetInviteText (documentid doc) "" (systemActor t)
  _<- randomUpdate $ \t->SetInviteText (documentid doc) "new invite text" (systemActor t)
  mdoc <- randomUpdate $ \t->SignableFromDocumentIDWithUpdatedAuthor author (documentid doc) (systemActor t)
  assertJust mdoc
  let ndoc = fromJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid ndoc)
  assertJust $ find (\e -> evType e == SignableFromDocumentIDWithUpdatedAuthorEvidence) lg
  assertJust $ find (\e -> evType e == SetInvitationTextEvidence) lg

testTemplateFromDocumentEvidenceLog :: TestEnv ()
testTemplateFromDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  success <- randomUpdate $ \t->TemplateFromDocument (documentid doc) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == TemplateFromDocumentEvidence) lg

testTimeoutDocumentEvidenceLog :: TestEnv ()
testTimeoutDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  success <- randomUpdate $ \t->TimeoutDocument (documentid doc) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == TimeoutDocumentEvidence) lg

testUpdateFieldsEvidenceLog :: TestEnv ()
testUpdateFieldsEvidenceLog = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ isSignable &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  let sf = filter (\f -> not $ (sfType f) `elem` [FirstNameFT,LastNameFT, EmailFT]) $ signatoryfields $ signatorydetails sl
  case sf of
   (f:_) -> do
        v <-  rand 10 arbitrary
        success <- randomUpdate $ \t->UpdateFields (documentid doc) (signatorylinkid sl) [(sfType f,v)] (systemActor t)
        lg <- dbQuery $ GetEvidenceLog (documentid doc)
        case success of
                True ->
                    assertBool "if UpdateFields did change document it should add to the evidence (or not affect anything) " $
                        (isJust (find (\e -> evType e == UpdateFieldsEvidence) lg))
                False ->
                    assertEqual "if UpdateFields did not change any rows it should not add to the evidence" Nothing
                        (find (\e -> evType e == UpdateFieldsEvidence) lg)
   _ -> return ()

testPreparationToPendingEvidenceLog :: TestEnv ()
testPreparationToPendingEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == PreparationToPendingEvidence) lg

testMarkInvitationReadEvidenceLog :: TestEnv ()
testMarkInvitationReadEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = getAuthorSigLink doc
  success <- randomUpdate $ \t->MarkInvitationRead (documentid doc) (signatorylinkid sl) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == MarkInvitationReadEvidence) lg

testMarkDocumentSeenEvidenceLog :: TestEnv ()
testMarkDocumentSeenEvidenceLog  = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (all (isAuthor =>>^ isSignatory) . documentsignatorylinks))
  let Just sl = getAuthorSigLink doc
      k = unMagicHash $ signatorymagichash sl
      mh = unsafeMagicHash (k + 1)
  success <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  assert success
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.empty (systemActor t)
  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  assertRaisesKontra (\SignatoryTokenDoesNotMatch {} -> True) $ do
    _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh (systemActor t)
    return ()
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  let n = length (filter (\e -> evType e == MarkDocumentSeenEvidence) lg)
  assertBool ("Should have 3 seen events, but found " ++ show n) $ 3 == n

testErrorDocumentEvidenceLog :: TestEnv ()
testErrorDocumentEvidenceLog  = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (const True)
  success <- randomUpdate $ \t->ErrorDocument (documentid doc) "Some error" (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ErrorDocumentEvidence) lg

testDocumentFromSignatoryDataEvidenceLog :: TestEnv ()
testDocumentFromSignatoryDataEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (systemActor t)
  mdoc <- randomUpdate $ \t a b c d e f -> DocumentFromSignatoryData (documentid doc) a b c d e f [] (systemActor t)
  assertJust mdoc
  let ndoc = fromJust mdoc
  lg <- dbQuery $ GetEvidenceLog (documentid ndoc)
  assertJust $ find (\e -> evType e == AuthorUsesCSVEvidence)    lg

testSaveSigAttachmentEvidenceLog :: TestEnv ()
testSaveSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ isSignable)
  file <- addNewRandomFile
  randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (systemActor t)
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing
  randomUpdate $ \t->SaveSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) "attachment" (fileid file) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == SaveSigAttachmentEvidence) lg


testDeleteSigAttachmentAlreadySigned :: TestEnv ()
testDeleteSigAttachmentAlreadySigned = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author $ (    isSignable
                                                           &&^ isPreparation
                                                           &&^ ((all isSignatory) . documentsignatorylinks)
                                                           &&^ (((==) 2) . length .documentsignatorylinks))
  file <- addNewRandomFile
  let sl = (documentsignatorylinks doc) !! 1
  _<-randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ sl)
                        [SignatoryAttachment { signatoryattachmentfile        = Nothing
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (systemActor t)
  randomUpdate $ \t->PreparationToPending (documentid doc) (systemActor t) Nothing
  randomUpdate $ \t->SaveSigAttachment (documentid doc) (signatorylinkid $ sl) "attachment" (fileid file) (systemActor t)

  randomUpdate $ \t->DeleteSigAttachment (documentid doc) (signatorylinkid $ sl) (fileid file) (systemActor t)
  randomUpdate $ \t->SaveSigAttachment (documentid doc) (signatorylinkid $ sl) "attachment" (fileid file) (systemActor t)

  _ <- randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
  randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.empty (systemActor t)
  assertRaisesKontra (\SignatoryHasAlreadySigned {} -> True) $ do
    randomUpdate $ \t->DeleteSigAttachment (documentid doc) (signatorylinkid $ sl) (fileid file) (systemActor t)
  
testDeleteSigAttachmentEvidenceLog :: TestEnv ()
testDeleteSigAttachmentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  _<-randomUpdate $ \t->SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0)
                        [SignatoryAttachment { signatoryattachmentfile        = Just $ (fileid file)
                                             , signatoryattachmentname        = "attachment"
                                             , signatoryattachmentdescription = "gimme!"
                                             }] (systemActor t)
  randomUpdate $ \t->DeleteSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) (fileid file) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == DeleteSigAttachmentEvidence) lg

testNewDocumentEvidenceLog :: TestEnv ()
testNewDocumentEvidenceLog = do
  (_, _, ed) <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc"
  assertRight ed
  let Right d = ed
      did = documentid d
  lg <- dbQuery $ GetEvidenceLog did
  assertJust $ find (\e -> evType e == NewDocumentEvidence) lg

testAddDocumentAttachmentEvidenceLog :: TestEnv ()
testAddDocumentAttachmentEvidenceLog = do
  (_, _, ed) <- performNewDocumentWithRandomUser Nothing (Signable Contract) "doc"
  assertRight ed
  let Right d = ed
      did = documentid d
  file <- addNewRandomFile
  --execute
  success <- randomUpdate $ \t->AddDocumentAttachment did (fileid file) (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog did
  assertJust $ find (\e -> evType e == AddDocumentAttachmentEvidence) lg

testAddInvitationEvidenceLog :: TestEnv ()
testAddInvitationEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  success <- randomUpdate $ \t->AddInvitationEvidence (documentid doc) (signatorylinkid sl) Nothing (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == InvitationEvidence) lg

testAttachCSVUploadEvidenceLog :: TestEnv ()
testAttachCSVUploadEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPreparation &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  randomUpdate $ \c t->AttachCSVUpload (documentid doc) (signatorylinkid sl) c (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachCSVUploadEvidence) lg

testAttachFileEvidenceLog :: TestEnv ()
testAttachFileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  randomUpdate $ \t->AttachFile (documentid doc) (fileid file) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachFileEvidence) lg

testAttachSealedFileEvidenceLog :: TestEnv ()
testAttachSealedFileEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isClosed
  file <- addNewRandomFile
  randomUpdate $ \t->AttachSealedFile (documentid doc) (fileid file) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == AttachSealedFileEvidence) lg

testCancelDocumentEvidenceLog :: TestEnv ()
testCancelDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  randomUpdate $ \t-> CancelDocument (documentid doc) ManualCancel (systemActor t)
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == CancelDocumentEvidence) lg

testChangeSignatoryEmailWhenUndeliveredEvidenceLog :: TestEnv ()
testChangeSignatoryEmailWhenUndeliveredEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ ((<=) 2 . length . documentsignatorylinks))
  let Just sl = getSigLinkFor doc (not . (isAuthor::SignatoryLink->Bool))
  success <- randomUpdate $ \t-> ChangeSignatoryEmailWhenUndelivered (documentid doc) (signatorylinkid sl) Nothing "email@email.com" (systemActor t)
  assert success
  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == ChangeSignatoryEmailWhenUndeliveredEvidence) lg

testCloseDocumentEvidenceLog :: TestEnv ()
testCloseDocumentEvidenceLog = do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  forM_ (documentsignatorylinks doc) $ \sl -> when (isSignatory sl) $ do
    void $ randomUpdate $ \t->MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor t)
    randomUpdate $ \t->SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) Nothing SignatoryScreenshots.empty (systemActor t)
  randomUpdate $ \t-> CloseDocument (documentid doc) (systemActor t)

  lg <- dbQuery $ GetEvidenceLog (documentid doc)
  assertJust $ find (\e -> evType e == CloseDocumentEvidence) lg


performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> TestEnv (User, MinutesTime, Either String Document)
performNewDocumentWithRandomUser Nothing doctype title = do
  user <- addNewRandomUser
  time <- getMinutesTime
  let aa = authorActor time noIP (userid user) (getEmail user)
  mdoc <- randomUpdate $ NewDocument user title doctype 0 aa
  return (user, time, maybe (Left "no document") Right mdoc)
performNewDocumentWithRandomUser (Just company) doctype title = do
  user <- addNewRandomCompanyUser (companyid company) False
  time <- getMinutesTime
  let aa = authorActor time noIP (userid user) (getEmail user)
  mdoc <- randomUpdate $ NewDocument user title doctype 0 aa
  return (user, time, maybe (Left "no document") Right mdoc)

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
    assertEqual "Uses email identification only" StandardAuthentication (documentauthenticationmethod doc)
    assertEqual "Doc has user's footer" (customfooter $ usersettings user) (documentmailfooter $ documentui doc)
    assertEqual "In preparation" Preparation (documentstatus doc)
    assertEqual "1 signatory" 1 (length $ documentsignatorylinks doc)
    let siglink = head $ documentsignatorylinks doc
    assertBool "link is author and possibly signer" $
      (signatoryisauthor $ signatorydetails siglink)
    assertEqual "link first name matches author's" (getFirstName user) (getFirstName siglink)
    assertEqual "link last name matches author's" (getLastName user) (getLastName siglink)
    assertEqual "link email matches author's" (getEmail user) (getEmail siglink)
    assertEqual "link personal number matches author's" (getPersonalNumber user) (getPersonalNumber siglink)
    assertEqual "link company name matches company's" (getCompanyName mcompany) (getCompanyName siglink)
    assertEqual "link company number matches company's" (getCompanyNumber mcompany) (getCompanyNumber siglink)
    assertEqual "link signatory matches author id" (Just $ userid user) (maybesignatory siglink)

testCancelDocumentCancelsDocument :: TestEnv ()
testCancelDocumentCancelsDocument = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ isPending)
  time <- getMinutesTime
  randomUpdate $ CancelDocument (documentid doc) ManualCancel (authorActor time noIP (userid user) (getEmail user))

  Just canceleddoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  let doNotCompareStatusClass x = x { signatorylinkstatusclass = SCDraft }
  assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
  assertEqual "Updated modification time" time (documentmtime canceleddoc)
  assertEqual "Matching cancellation reason" (Just ManualCancel) (documentcancelationreason canceleddoc)
  assertEqual "Siglinks are unchanged"
                  (map doNotCompareStatusClass (documentsignatorylinks doc))
                  (map doNotCompareStatusClass (documentsignatorylinks canceleddoc))
  assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)

testCancelDocumentReturnsLeftIfDocInWrongState :: TestEnv ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (isSignable &&^ not . isPending)
  time <- getMinutesTime
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $
               randomUpdate $ CancelDocument (documentid doc) ManualCancel 
                              (authorActor time noIP (userid user) (getEmail user))

testSignatories1 :: Assertion
testSignatories1 =
  let s1 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField FirstNameFT "Eric" True []
                                               ,SignatoryField LastNameFT "Normand" True []
                                                ],
                             signatoryisauthor = True,
                             signatoryispartner = True
                            }
      s2 = SignatoryDetails {signatorysignorder = SignOrder 0,
                             signatoryfields = [SignatoryField LastNameFT "Normand" True []
                                               ,SignatoryField FirstNameFT "Eric" True []
                                                ],
                             signatoryisauthor = True,
                             signatoryispartner = True
                            }
  in assertBool "Signatories should be equal" (s1 == s2)

propSignatoryDetailsEq :: SignOrder -> SignatoryDetails -> Property
propSignatoryDetailsEq o1 sd =
   (o1 == o1) ==> sd{signatorysignorder = o1} == sd{signatorysignorder = o1}

propSignatoryDetailsNEq :: SignOrder -> SignOrder -> SignatoryDetails -> Property
propSignatoryDetailsNEq o1 o2 sd =
  (o1 /= o2) ==> sd{signatorysignorder = o1} /= sd{signatorysignorder = o2}

assertOneArchivedSigLink :: MonadIO m => Document -> m ()
assertOneArchivedSigLink doc =
  assertEqual "Expected one archived sig link"
              1
              (length . filter signatorylinkdeleted . documentsignatorylinks $ doc)

assertOneReallyDeletedSigLink :: MonadIO m => Document -> m ()
assertOneReallyDeletedSigLink doc =
  assertEqual "Expected one really deleted sig link"
              1
              (length . filter signatorylinkreallydeleted . documentsignatorylinks $ doc)

assertNoArchivedSigLink :: MonadIO m => Document -> m ()
assertNoArchivedSigLink doc =
  assertEqual "Expected no archived sig link"
              0
              (length . filter signatorylinkdeleted . documentsignatorylinks $ doc)

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
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertOneArchivedSigLink ndoc

testArchiveDocumentCompanyAdminRight :: TestEnv ()
testArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid adminuser) (documentid doc) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertOneArchivedSigLink ndoc

testRestoreArchivedDocumentAuthorRight :: TestEnv ()
testRestoreArchivedDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  randomUpdate $ \t->RestoreArchivedDocument author (documentid doc) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertNoArchivedSigLink ndoc

testRestoreArchiveDocumentCompanyAdminRight :: TestEnv ()
testRestoreArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  randomUpdate $ \t->RestoreArchivedDocument adminuser (documentid doc) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertNoArchivedSigLink ndoc

testReallyDeleteDocumentPrivateAuthorRight :: TestEnv ()
testReallyDeleteDocumentPrivateAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  assertOneArchivedSigLink ndoc
  success2 <- randomUpdate $ \t->ReallyDeleteDocument (userid author) (documentid doc) (systemActor t)
  Just ndoc2 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success2
  assertOneReallyDeletedSigLink ndoc2

testReallyDeleteDocumentCompanyAdminRight :: TestEnv ()
testReallyDeleteDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  success <- randomUpdate $ \t->ReallyDeleteDocument (userid adminuser) (documentid doc) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  assertOneReallyDeletedSigLink ndoc

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

testReallyDeleteDocumentCompanyAuthorLeft :: TestEnv ()
testReallyDeleteDocumentCompanyAuthorLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  success <- randomUpdate $ \t->ReallyDeleteDocument (userid author) (documentid doc) (systemActor t)
  assertBool "Not admin can only delete drafts" (not success || Preparation == documentstatus doc)

testReallyDeleteDocumentCompanyStandardLeft :: TestEnv ()
testReallyDeleteDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  randomUpdate $ \t->ArchiveDocument (userid author) (documentid doc) (systemActor t)
  success <- randomUpdate $ \t->ReallyDeleteDocument (userid standarduser) (documentid doc) (systemActor t)
  assertBool "Not admin can only delete drafts" (not success || Preparation == documentstatus doc)

testReallyDeleteNotArchivedLeft :: TestEnv ()
testReallyDeleteNotArchivedLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  success <- randomUpdate $ \t->ReallyDeleteDocument (userid author) (documentid doc) (systemActor t)
  assert $ not success

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

checkQueryContainsArchivedDocs :: DBQuery TestEnv q [Document] => (User -> q) -> TestEnv ()
checkQueryContainsArchivedDocs qry = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (isPreparation ||^ isClosed))
  docsbeforearchive <- dbQuery (qry author)
  assertEqual "Expecting no docs before archive" [] (map documentid docsbeforearchive)
  randomUpdate $ \t -> ArchiveDocument (userid author) (documentid doc) (systemActor t)
  docsafterarchive <- dbQuery (qry author)
  assertEqual "Expecting 1 doc after archive" [documentid doc] (map documentid docsafterarchive)
  _ <- randomUpdate $ \t -> ReallyDeleteDocument (userid author) (documentid doc) (systemActor t)
  docsafterdelete <- dbQuery (qry author)
  assertEqual "Expecting no docs after really deleting" [] (map documentid docsafterdelete)

testSetDocumentLangNotLeft :: TestEnv ()
testSetDocumentLangNotLeft = doTimes 10 $ do
  success <- randomUpdate $ \d l t -> SetDocumentLang d l (systemActor t)
  assert $ not success

testNewDocumentDependencies :: TestEnv ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  -- execute
  now <- liftIO $ getMinutesTime
  let aa = authorActor now noIP (userid author) (getEmail author)
  mdoc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype 0 aa)
  -- assert
  assertJust mdoc
  assertInvariants $ fromJust mdoc

testDocumentCanBeCreatedAndFetchedByID :: TestEnv ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  now <- liftIO $ getMinutesTime
  let aa = authorActor now noIP (userid author) (getEmail author)
  mdoc <- randomUpdate $ (\title doctype -> NewDocument author (fromSNN title) doctype 0 aa)
  let doc = case mdoc of
          Nothing -> error "No document"
          Just d  -> d
  -- execute
  mndoc <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  -- assert

  assertJust mndoc
  assert $ sameDocID doc (fromJust mndoc)
  assertInvariants (fromJust mndoc)

testDocumentAttachNotPreparationLeft :: TestEnv ()
testDocumentAttachNotPreparationLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \t->AttachFile (documentid doc) (fileid file) (systemActor t)

testDocumentAttachPreparationRight :: TestEnv ()
testDocumentAttachPreparationRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  randomUpdate $ \t -> AttachFile (documentid doc) (fileid file) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  assertInvariants ndoc


testNoDocumentAttachAlwaysLeft :: TestEnv ()
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
  -- setup
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ (\docid t -> AttachFile docid (fileid file) (systemActor t))
  --assert

testDocumentAttachHasAttachment :: TestEnv ()
testDocumentAttachHasAttachment = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  randomUpdate $ \t -> AttachFile (documentid doc) (fileid file) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  -- assertJust $ find ((== a) . filename) (documentfiles $ fromRight edoc)
  assertInvariants ndoc

testNoDocumentAttachSealedAlwaysLeft :: TestEnv ()
testNoDocumentAttachSealedAlwaysLeft = doTimes 10 $ do
  -- setup
  file <- addNewRandomFile
  --execute
  -- non-existent docid
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ (\docid -> AttachSealedFile docid (fileid file) (systemActor time))

testDocumentAttachSealedPendingRight :: TestEnv ()
testDocumentAttachSealedPendingRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocument ((randomDocumentAllowsDefault author) { randomDocumentAllowedTypes = [ Signable Offer
                                                                                                , Signable Contract
                                                                                                , Signable Offer
                                                                                                ]
                                                                 , randomDocumentAllowedStatuses = [Closed]
                                                                 })
  file <- addNewRandomFile
  time <- rand 10 arbitrary
  --execute
  success <- randomUpdate $ AttachSealedFile (documentid doc) (fileid file) (systemActor time)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  assert success
  assertBool "Should have new file attached, but it's not" $ Just (fileid file) == documentsealedfile ndoc


testGetTimedOutButPendingDocuments :: TestEnv ()
testGetTimedOutButPendingDocuments = doTimes 1 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isPending &&^ (isJust . documenttimeouttime))
  _doc2 <- addRandomDocumentWithAuthorAndCondition author (not . isPending)

  let t = unTimeoutTime $ fromJust $ documenttimeouttime doc
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
  sd <- signatoryDetailsFromUser author (False, False)
  --execute
  success <- dbUpdate $ ResetSignatoryDetails (documentid doc) [sd] (systemActor mt)
  --assert
  assert $ not success

testPreparationResetSignatoryDetailsAlwaysRight :: TestEnv ()
testPreparationResetSignatoryDetailsAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  mt <- rand 10 arbitrary
  --execute
  success <- dbUpdate $ ResetSignatoryDetails (documentid doc) [emptySignatoryDetails { signatoryisauthor = True }] (systemActor mt)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  assert success
  assertInvariants ndoc

testNoDocumentResetSignatoryDetailsAlwaysLeft :: TestEnv ()
testNoDocumentResetSignatoryDetailsAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  --author <- addNewRandomUser
  mt <- rand 10 arbitrary
  --execute
  -- non-existent docid
  success <- dbUpdate $ ResetSignatoryDetails a [emptySignatoryDetails { signatoryisauthor = True }] (systemActor mt)
  --assert
  assert $ not success

testNoDocumentAttachCSVUploadAlwaysLeft :: TestEnv ()
testNoDocumentAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    randomUpdate $ \did slid csv t->AttachCSVUpload did slid csv (systemActor t)

testNotPreparationAttachCSVUploadAlwaysLeft :: TestEnv ()
testNotPreparationAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  slid <- rand 10 $ elements [signatorylinkid sl | sl <- documentsignatorylinks doc, isSignatory sl]
  --execute
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \csv t -> AttachCSVUpload (documentid doc) slid csv (systemActor t)

testPreparationAttachCSVUploadAuthorIndexLeft :: TestEnv ()
testPreparationAttachCSVUploadAuthorIndexLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  (csvupload, t) <- rand 10 arbitrary
  let Just sl = getAuthorSigLink doc
  --execute
  assertRaisesKontra (\SignatoryIsAuthor {} -> True) $ do
    dbUpdate $ AttachCSVUpload (documentid doc)
          (signatorylinkid sl)
          (csvupload)
          (systemActor t)

testPreparationAttachCSVUploadNonExistingSignatoryLink :: TestEnv ()
testPreparationAttachCSVUploadNonExistingSignatoryLink = doTimes 3 $ do
  -- setup
  (csvupload, time) <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  slid <- unsafeSignatoryLinkID <$> rand 10 arbitrary
  --execute
  assertRaisesKontra (\SignatoryLinkDoesNotExist {} -> True) $ do
    dbUpdate $ AttachCSVUpload (documentid doc)
          slid csvupload (systemActor time)

testGetDocumentsSharedInCompany :: TestEnv ()
testGetDocumentsSharedInCompany = doTimes 10 $ do
  -- two companies, two users per company, two users outside of company
  -- each having a document here
  company1 <- addNewCompany
  company2 <- addNewCompany
  user1' <- addNewRandomUser
  user2' <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid user1') (Just (companyid company1))
  Just user1 <- dbQuery $ GetUserByID (userid user1')
  _ <- dbUpdate $ SetUserCompany (userid user2') (Just (companyid company1))
  Just user2 <- dbQuery $ GetUserByID (userid user2')
  user3' <- addNewRandomUser
  user4' <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany (userid user3') (Just (companyid company2))
  Just user3 <- dbQuery $ GetUserByID (userid user3')
  _ <- dbUpdate $ SetUserCompany (userid user4') (Just (companyid company2))
  Just user4 <- dbQuery $ GetUserByID (userid user4')
  user5 <- addNewRandomUser
  user6 <- addNewRandomUser

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

  dlist1 <- dbQuery $ GetAvailableTemplates (userid user1) [Offer, Order, Contract]
  dlist2 <- dbQuery $ GetAvailableTemplates (userid user2) [Offer, Order, Contract]
  dlist3 <- dbQuery $ GetAvailableTemplates (userid user3) [Offer, Order, Contract]
  dlist4 <- dbQuery $ GetAvailableTemplates (userid user4) [Offer, Order, Contract]
  dlist5 <- dbQuery $ GetAvailableTemplates (userid user5) [Offer, Order, Contract]
  dlist6 <- dbQuery $ GetAvailableTemplates (userid user6) [Offer, Order, Contract]

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

  actor <- unAuthorActor <$> rand 10 arbitrary

  success <- dbUpdate $ SetDocumentTitle (documentid doc1) title (actor)
  assert success
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc1

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
            , Desc DocumentOrderByProcess
            , Desc DocumentOrderByPartners
            ]
            (0,maxBound)
  return ()

testCreateFromSharedTemplate :: TestEnv ()
testCreateFromSharedTemplate = do
  user <- addNewRandomUser
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (const True)
  tmpdoc <- fmap fromJust $ dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           _ <- dbUpdate $ TemplateFromDocument docid (systemActor mt)
           fromJust <$> (dbQuery $ GetDocumentByDocumentID docid)
  newuser <- addNewRandomUser

  _ <- dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor newuser (documentid doc) (systemActor mt)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  let [author1] = filter isAuthor $ documentsignatorylinks doc
  let [author2] = filter isAuthor $ documentsignatorylinks ndoc
  let isCustom (SignatoryField { sfType = CustomFT _ _ }) = True
      isCustom _ = False
  if (fmap sfValue $ filter isCustom $ signatoryfields $ signatorydetails author1)
     == (fmap sfValue $ filter isCustom $ signatoryfields $ signatorydetails author2)
    then assertSuccess
    else assertFailure "Replacing signatory details based on user is loosing fields | SKRIVAPADEV-294"


testCreateFromTemplateCompanyField :: TestEnv ()
testCreateFromTemplateCompanyField = doTimes 10 $ do
  user <- addNewRandomUser
  company <- addNewCompany
  _ <- dbUpdate $ SetUserCompany (userid user) (Just (companyid company))
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (const True)
  tmpdoc <- fmap fromJust $ dbQuery $ GetDocumentByDocumentID docid
  mt <- rand 10 arbitrary
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else do
           _ <- dbUpdate $ TemplateFromDocument docid (systemActor mt)
           fromJust <$> (dbQuery $ GetDocumentByDocumentID docid)
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  doc' <- fromJust <$> (dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor user' (documentid doc) (systemActor mt))
  let [author] = filter isAuthor $ documentsignatorylinks doc'
  assertEqual "Author signatory link company name is not same as his company" (getCompanyName company) (getCompanyName author)


    
testAddDocumentAttachmentFailsIfNotPreparation :: TestEnv ()
testAddDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  file <- addNewRandomFile
  --execute
  success <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (systemActor t)
  --assert
  assert $ not success

testAddDocumentAttachmentOk :: TestEnv ()
testAddDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  success <- randomUpdate $ \t->AddDocumentAttachment (documentid doc) (fileid file) (systemActor t)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
  --assert
  assert success
  assertEqual "Author attachment was really attached" [fileid file]
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
  let sa = signatoryActor time noIP Nothing email1 sl
  randomUpdate $ SetSigAttachments (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) [att1, att2] sa
  edoc1 <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  randomUpdate $ DeleteSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) (fileid file1) sa
  Just ndoc1 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  randomUpdate $ SaveSigAttachment (documentid doc) (signatorylinkid $ (documentsignatorylinks doc) !! 0) name1 (fileid file2) sa
  Just ndoc2 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  --assert
  assertJust edoc1
  let doc1 = fromJust edoc1
  assertEqual "Both attachments were attached" 2 (length (signatoryattachments $ (documentsignatorylinks doc1) !! 0))

  assertBool "All signatory attachments are not connected to files" (all (isNothing . signatoryattachmentfile)
                                                                           (signatoryattachments $ (documentsignatorylinks ndoc1) !! 0))

  assertBool "Attachment connected to signatory"
                 (Just (fileid file2) `elem` map signatoryattachmentfile (signatoryattachments $ (documentsignatorylinks ndoc2) !! 0))

------------------------------------------------

testDocumentFromSignatoryDataFailsDoesntExist :: TestEnv ()
testDocumentFromSignatoryDataFailsDoesntExist = doTimes 10 $ do
  (did, a, b, c, d, e, f, g, aa :: AuthorActor) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData did a b c d e f g (unAuthorActor aa)
  assertNothing mdoc

testDocumentFromSignatoryDataSucceedsExists :: TestEnv ()
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  (time, a, b, c, d, e, f, g) <- rand 10 arbitrary
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc) a b c d e f g
          (authorActor time noIP (userid author) (getEmail author))
  assertJust mdoc

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
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testTimeoutDocumentSignableNotLeft :: TestEnv ()
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  actor <- unSystemActor <$> rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ \d-> TimeoutDocument d actor

testSignDocumentNonSignableLeft :: TestEnv ()
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si SignatoryScreenshots.empty (systemActor t)

testSignDocumentSignableNotPendingLeft :: TestEnv ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  let Just sl = getSigLinkFor doc author
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ \si t -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si SignatoryScreenshots.empty (systemActor t)

testSignDocumentSignablePendingRight :: TestEnv ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  let Just sl = find (isSignatory &&^ (not . hasSigned)) (documentsignatorylinks doc)
  time <- rand 10 arbitrary
  _ <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) (systemActor time)
  randomUpdate $ \si -> SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl) si SignatoryScreenshots.empty (systemActor time)


testSignDocumentNotLeft :: TestEnv ()
testSignDocumentNotLeft = doTimes 10 $ do
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    -- our machinery is broken here, baseline condition has only relations
    -- this should be ignored and properly return info about non existing document
    randomUpdate $ \d sl mh si t -> SignDocument d sl mh si SignatoryScreenshots.empty (systemActor t)

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
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testRejectDocumentNotSignableLeft :: TestEnv ()
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  let Just sl = getSigLinkFor doc author
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing
           (authorActor time noIP (userid author) (getEmail author))

testRejectDocumentSignableNotPendingLeft :: TestEnv ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ not . isPending)
  let Just sl = getSigLinkFor doc author
  time <- rand 10 arbitrary
  assertRaisesKontra (\DocumentStatusShouldBe {} -> True) $ do
    randomUpdate $ RejectDocument (documentid doc) (signatorylinkid sl) Nothing
           (authorActor time noIP (userid author) (getEmail author))

testRejectDocumentNotLeft :: TestEnv ()
testRejectDocumentNotLeft = doTimes 10 $ do
  (did, time, slid) <- rand 10 arbitrary
  let sa = signatoryActor time noIP Nothing "hello@hello.com" slid
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    randomUpdate $ RejectDocument did slid Nothing sa

testRejectDocumentSignablePendingRight :: TestEnv ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  slid <- rand 10 $ elements (map signatorylinkid . filter (signatoryispartner . signatorydetails) $ documentsignatorylinks doc)
  let Just sl = getSigLinkFor doc slid
  time <- rand 10 arbitrary
  let sa = signatoryActor time noIP Nothing (getEmail sl) slid
  randomUpdate $ RejectDocument (documentid doc) slid Nothing sa
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assertInvariants ndoc

testMarkInvitationRead :: TestEnv ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks))

  sl' <- rand 10 $ elements $ documentsignatorylinks doc
  let slid = signatorylinkid sl'
  time <- getMinutesTime
  success <- dbUpdate $ MarkInvitationRead (documentid doc) slid
          (signatoryActor time noIP (maybesignatory sl') (getEmail sl') slid)
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  let Just sl = getSigLinkFor ndoc slid
  assertEqual "Invitation read time should be set." (Just time) (maybereadinvite sl)

testMarkInvitationReadDocDoesntExist :: TestEnv ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  (did, slid, time, ip, eml) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist{} -> True) $ do
    _ <- randomUpdate $ MarkInvitationRead did slid
            (signatoryActor time ip Nothing eml slid)
    return ()
  return ()

testMarkDocumentSeenNotSignableLeft :: TestEnv ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }

  (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                (time, ip) <- rand 10 arbitrary
                let sa = signatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
                success <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                assert $ not success)

testMarkDocumentSeenClosedOrPreparationLeft :: TestEnv ()
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Closed, Preparation]
         }
  (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                (time, ip) <- rand 10 arbitrary
                let sa = signatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
                success <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                assert $ not success)

testMarkDocumentSeenNotLeft :: TestEnv ()
testMarkDocumentSeenNotLeft = doTimes 10 $ do
  (d, s, m, a) <- rand 10 arbitrary
  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
    success <- randomUpdate $ MarkDocumentSeen d s m (unSignatoryActor a)
    assert $ not success

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
                (time, ip) <- rand 10 arbitrary
                let sa = signatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
                success <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl) sa
                Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
                assert success
                let Just  tsl  = getSigLinkFor ndoc (signatorylinkid sl)
                assertBool "Signatorylink should be marked seen now." (hasSeen tsl))

testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: TestEnv ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
                (time, ip) <- rand 10 arbitrary
                let sa = signatoryActor time ip (maybesignatory sl) (getEmail sl) (signatorylinkid sl)
                assertRaisesKontra (\SignatoryTokenDoesNotMatch {} -> True) $ do
                  success <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh sa
                  assert $ not success)

testSetInvitationDeliveryStatusNotSignableLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  actor <- unSystemActor <$> rand 10 arbitrary
  let Just sl = getAuthorSigLink doc
  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ do
    success <- randomUpdate $ \st-> SetInvitationDeliveryStatus (documentid doc) (signatorylinkid sl) st actor
    assert $ not success


testSetInvitationDeliveryStatusNotLeft :: TestEnv ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  actor <- unSystemActor <$> rand 10 arbitrary
  --assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ do
  assertRaisesKontra (\DBBaseLineConditionIsFalse {} -> True) $ do
    success <- randomUpdate $ \d s st-> SetInvitationDeliveryStatus d s st actor
    assert $ not success

testSetInvitationDeliveryStatusSignableRight :: TestEnv ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  (st, actor) <- rand 10 arbitrary
  success <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc) slid st (unSystemActor actor)
  assert success

testSetDocumentTagsRight :: TestEnv ()
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  (tags, time) <- first S.fromList <$> rand 10 arbitrary
  let actor = authorActor time noIP (userid author) (getEmail author)
  success <- randomUpdate $ SetDocumentTags (documentid doc) tags actor
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  assertEqual "Tags should be equal" tags (documenttags ndoc)

testSetDocumentUINotLeft :: TestEnv ()
testSetDocumentUINotLeft = doTimes 10 $ do
  success <- randomUpdate $ (\did ui time -> SetDocumentUI did ui (systemActor time))
  assert $ not success

testSetDocumentUIRight :: TestEnv ()
testSetDocumentUIRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  ac <- unSystemActor <$> rand 10 arbitrary
  success <- randomUpdate $ (\ui -> SetDocumentUI (documentid doc) ui ac)
  assert success

testCloseDocumentSignableButNotEverybodyHasSigned :: TestEnv ()
testCloseDocumentSignableButNotEverybodyHasSigned = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = (\doc -> length (documentsignatorylinks doc) > 1) &&^
                                     (not . all (isSignatory =>>^ hasSigned) . documentsignatorylinks)
         }
  sa <- unSystemActor <$> rand 10 arbitrary

  assertRaisesKontra (\(SignatoryHasNotYetSigned {}) -> True) $ do
    randomUpdate $ CloseDocument (documentid doc) sa

testCloseDocumentNotSignableNothing :: TestEnv ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  sa <- unSystemActor <$> rand 10 arbitrary
  assertRaisesKontra (\(DocumentTypeShouldBe {}) -> True) $ do
    randomUpdate $ CloseDocument (documentid doc) sa

testCloseDocumentNotNothing :: TestEnv ()
testCloseDocumentNotNothing = doTimes 10 $ do
  sa <- unSystemActor <$> rand 10 arbitrary
  did <- rand 10 arbitrary
  assertRaisesKontra (\(DocumentDoesNotExist {}) -> True) $ do
    randomUpdate $ CloseDocument did sa

testCancelDocumentNotSignableNothing :: TestEnv ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  time <- rand 10 arbitrary
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  assertRaisesKontra (\DocumentTypeShouldBe {} -> True) $ 
               randomUpdate $ CancelDocument (documentid doc) ManualCancel
                              (authorActor time noIP (userid author) (getEmail author))

testCancelDocumentNotNothing :: TestEnv ()
testCancelDocumentNotNothing = doTimes 10 $ do
  aa <- unAuthorActor <$> rand 10 arbitrary

  assertRaisesKontra (\DocumentDoesNotExist {} -> True) $ 
             randomUpdate $ (\did -> CancelDocument did ManualCancel aa)

testSetDocumentTitleNotLeft :: TestEnv ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  (did, title, actor) <- rand 10 arbitrary
  success <- randomUpdate $ SetDocumentTitle did title (unAuthorActor actor)
  assert $ not success

testSetDocumentTitleRight :: TestEnv ()
testSetDocumentTitleRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed)
         }
  let title = "my new cool title"
  actor <- unAuthorActor <$> rand 10 arbitrary
  success <- randomUpdate $ SetDocumentTitle (documentid doc) title actor
  Just ndoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc

  assert success
  assertEqual "Title is set properly" title (documenttitle ndoc)

testSetDocumentDaysToSignNotLeft :: TestEnv ()
testSetDocumentDaysToSignNotLeft = doTimes 10 $ do
  (did, d, actor) <- rand 10 arbitrary
  success <- randomUpdate $ SetDaysToSign did d (unAuthorActor actor)
  assert $ not success

testSetDocumentDaysToSignRight :: TestEnv ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = not . isClosed
         }
  actor <- unAuthorActor <$> rand 10 arbitrary
  let daystosign = 15
  success1 <- randomUpdate $ SetDaysToSign (documentid doc) daystosign actor
  Just ndoc1 <- dbQuery $ GetDocumentByDocumentID $ documentid doc

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
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
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
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
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
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  doc <- fromJust <$> (dbQuery $ GetDocumentByDocumentID did)
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
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
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
    _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
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

testGetDocumentsByAuthorFiltering :: TestEnv ()
testGetDocumentsByAuthorFiltering = doTimes 10 $ do
    company <- addNewCompany
    u1 <- addNewRandomUser -- author
    u2 <- addNewRandomUser -- some other user
    _ <- dbUpdate $ SetUserCompany (userid u1) (Just (companyid company))
    Just u1' <- dbQuery $ GetUserByID (userid u1)
    _ <- addRandomDocumentWithAuthor u1'

    docs   <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid u1)] [DocumentFilterByAuthor (userid u1)] [] (0,maxBound)
    nodocs <- dbQuery $ GetDocuments [DocumentsVisibleToUser (userid u1)] [DocumentFilterByAuthor (userid u2)] [] (0,maxBound)

    assertEqual "Should have one document returned" 1 (length docs)
    assertEqual "Should have no documents" 0 (length nodocs)

testStatusClassSignedWhenAllSigned :: TestEnv ()
testStatusClassSignedWhenAllSigned = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isClosed &&^ ((<=) 2 . length . (filter isSignatory) . documentsignatorylinks))
  Just doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)

  assertEqual "Statusclass for signed documents is signed" SCSigned (documentstatusclass doc')


testStatusClassDeliveryProblemWhenUndelivered :: TestEnv ()
testStatusClassDeliveryProblemWhenUndelivered = doTimes 10 $ do
  author <- addNewRandomUser
  time <- getMinutesTime
  doc <- addRandomDocumentWithAuthorAndCondition author
            (    isSignable
             &&^ isPending
             &&^ ((all isSignatory) . documentsignatorylinks)
             &&^ ((<=) 2 . length . documentsignatorylinks))
  _ <- dbUpdate $ SetInvitationDeliveryStatus (documentid doc) (signatorylinkid $ last $ documentsignatorylinks doc) Undelivered (systemActor time)
  Just doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)

  assertEqual "Statusclass for signed documents is signed" SCDeliveryProblem (documentstatusclass doc')
