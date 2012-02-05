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


docStateTests :: Nexus -> Test
docStateTests conn = testGroup "DocState" [
  dataStructureProperties,
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
  edoc1 <- randomUpdate $ NewDocument singleuser (Just company) (BS.fromString "doc title") (Signable Contract) time
  assertLeft edoc1
  edoc2 <- randomUpdate $ NewDocument companyuser Nothing (BS.fromString "doc title") (Signable Contract) time
  validTest $ assertLeft edoc2

performNewDocumentWithRandomUser :: Maybe Company -> DocumentType -> String -> DB (User, MinutesTime, Either String Document)
performNewDocumentWithRandomUser Nothing doctype title = do
  user <- addNewRandomUser
  time <- getMinutesTime
  edoc <- randomUpdate $ NewDocument user Nothing (BS.fromString title) doctype time
  return (user, time, edoc)
performNewDocumentWithRandomUser (Just company) doctype title = do
  user <- addNewRandomCompanyUser (companyid company) False
  time <- getMinutesTime
  edoc <- randomUpdate $ NewDocument user (Just company) (BS.fromString title) doctype time
  return (user, time, edoc)

assertGoodNewDocument :: Maybe Company -> DocumentType -> String -> Bool -> (User, MinutesTime, Either String Document) -> DB (Maybe (DB ()))
assertGoodNewDocument mcompany doctype title authorsigns (user, time, edoc) = do
  assertRight edoc
  let (Right doc) = edoc
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
    then
      assertEqual "link is author and signer" [SignatoryPartner,SignatoryAuthor] (signatoryroles siglink)
    else
      assertEqual "link is just author" [SignatoryAuthor] (signatoryroles siglink)
  assertEqual "link first name matches author's" (getFirstName user) (getFirstName siglink)
  assertEqual "link last name matches author's" (getLastName user) (getLastName siglink)
  assertEqual "link email matches author's" (getEmail user) (getEmail siglink)
  assertEqual "link personal number matches author's" (getPersonalNumber user) (getPersonalNumber siglink)
  assertEqual "link company name matches company's" (getCompanyName mcompany) (getCompanyName siglink)
  assertEqual "link company number matches company's" (getCompanyNumber mcompany) (getCompanyNumber siglink)
  assertEqual "link signatory matches author id" (Just $ userid user) (maybesignatory siglink)
  assertEqual "link signatory matches author company" (companyid <$> mcompany) (maybecompany siglink)
  validTest $ assertBool "success" True

testCancelDocumentCancelsDocument :: DB ()
testCancelDocumentCancelsDocument = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> isSignable d && documentstatus d `elem` [AwaitingAuthor, Pending])
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel time
  assertRight edoc
  let (Right canceleddoc) = edoc
  assertEqual "In canceled state" Canceled (documentstatus canceleddoc)
  assertEqual "Updated modification time" time (documentmtime canceleddoc)
  assertEqual "Matching cancellation reason" (Just ManualCancel) (documentcancelationreason canceleddoc)
  assertEqual "Siglinks are unchanged" (documentsignatorylinks doc) (documentsignatorylinks canceleddoc)
  assertEqual "Doc title is unchanged" (documenttitle doc) (documenttitle canceleddoc)
  validTest $ assertBool "successful" True

testCancelDocumentReturnsLeftIfDocInWrongState :: DB ()
testCancelDocumentReturnsLeftIfDocInWrongState = doTimes 10 $ do
  user <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition user (\d -> isSignable d && not (documentstatus d `elem` [AwaitingAuthor, Pending]))
  time <- getMinutesTime
  edoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel time
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

-- FIXME: arguments to this test are exhausting after 0 tries, so commenting out.
{-propSignatoryDetailsEq :: SignatoryDetails -> SignatoryDetails -> Property
propSignatoryDetailsEq sd1 sd2 =
   (signatorysignorder sd1 == signatorysignorder sd2) && (sort $ signatoryfields sd1) == (sort $ signatoryfields sd2) ==>
   sd1 == sd2-}

propSignatoryDetailsNEq :: SignatoryDetails -> SignatoryDetails ->  Property
propSignatoryDetailsNEq sd1 sd2 =
  (signatorysignorder sd1 /= signatorysignorder sd2) || (sort $ signatoryfields sd1) /= (sort $ signatoryfields sd2) ==>
  sd1 /= sd2

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
  etdoc <- randomUpdate $ ArchiveDocument author (documentid doc)
  validTest $ assertLeft etdoc

testArchiveDocumentAuthorRight :: DB ()
testArchiveDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ ArchiveDocument author (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertOneArchivedSigLink etdoc

testArchiveDocumentCompanyAdminRight :: DB ()
testArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ ArchiveDocument adminuser (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertOneArchivedSigLink etdoc

testRestoreArchivedDocumentAuthorRight :: DB ()
testRestoreArchivedDocumentAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ RestoreArchivedDocument author (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertNoArchivedSigLink etdoc

testRestoreArchiveDocumentCompanyAdminRight :: DB ()
testRestoreArchiveDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ RestoreArchivedDocument adminuser (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertNoArchivedSigLink etdoc

testReallyDeleteDocumentPrivateAuthorRight :: DB ()
testReallyDeleteDocumentPrivateAuthorRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc' <- randomUpdate $ ArchiveDocument author (documentid doc)
  _ <- validTest $ assertRight etdoc'
  _ <- validTest $ assertOneArchivedSigLink etdoc'
  etdoc <- randomUpdate $ ReallyDeleteDocument author (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertOneReallyDeletedSigLink etdoc

testReallyDeleteDocumentCompanyAdminRight :: DB ()
testReallyDeleteDocumentCompanyAdminRight = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  adminuser <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ ReallyDeleteDocument adminuser (documentid doc)
  _ <- validTest $ assertRight etdoc
  validTest $ assertOneReallyDeletedSigLink etdoc

-- for this stuff postgres implementation is stricter, with happstack it just left the doc unchanged
testArchiveDocumentUnrelatedUserLeft :: DB ()
testArchiveDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomAdvancedUser
  etdoc <- randomUpdate $ ArchiveDocument unrelateduser (documentid doc)
  validTest $ assertLeft etdoc

testArchiveDocumentCompanyStandardLeft :: DB ()
testArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ ArchiveDocument standarduser (documentid doc)
  validTest $ assertLeft etdoc

testRestoreArchivedDocumentUnrelatedUserLeft :: DB ()
testRestoreArchivedDocumentUnrelatedUserLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  unrelateduser <- addNewRandomAdvancedUser
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ RestoreArchivedDocument unrelateduser (documentid doc)
  validTest $ assertLeft etdoc

testRestoreArchiveDocumentCompanyStandardLeft :: DB ()
testRestoreArchiveDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ RestoreArchivedDocument standarduser (documentid doc)
  validTest $ assertLeft etdoc

testReallyDeleteDocumentCompanyAuthorLeft :: DB ()
testReallyDeleteDocumentCompanyAuthorLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ ReallyDeleteDocument author (documentid doc)
  validTest $ assertLeft etdoc

testReallyDeleteDocumentCompanyStandardLeft :: DB ()
testReallyDeleteDocumentCompanyStandardLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) False
  standarduser <- addNewRandomCompanyUser (companyid company) False
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  etdoc <- randomUpdate $ ReallyDeleteDocument standarduser (documentid doc)
  validTest $ assertLeft etdoc

testReallyDeleteNotArchivedLeft :: DB ()
testReallyDeleteNotArchivedLeft = doTimes 10 $ do
  company <- addNewCompany
  author <- addNewRandomCompanyUser (companyid company) True
  doc <- addRandomDocumentWithAuthorAndCondition author (\d -> isPreparation d || isClosed d)
  etdoc <- randomUpdate $ ReallyDeleteDocument author (documentid doc)
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
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  docsafterarchive <- dbQuery (qry author)
  _ <- validTest $ assertEqual "Expecting no docs after archive" [] (map documentid docsafterarchive)
  _ <- randomUpdate $ RestoreArchivedDocument author (documentid doc)
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
  _ <- randomUpdate $ ArchiveDocument author (documentid doc)
  docsafterarchive <- dbQuery (qry author)
  _ <- validTest $ assertEqual "Expecting 1 doc after archive" [documentid doc] (map documentid docsafterarchive)
  _ <- randomUpdate $ ReallyDeleteDocument author (documentid doc)
  docsafterdelete <- dbQuery (qry author)
  validTest $ assertEqual "Expecting no docs after really deleting" [] (map documentid docsafterdelete)

testSetDocumentLocaleNotLeft :: DB ()
testSetDocumentLocaleNotLeft = doTimes 10 $ do
  edoc <- randomUpdate $ SetDocumentLocale
  validTest $ do
    assertLeft edoc

testNewDocumentDependencies :: DB ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  -- execute
  now <- liftIO $ getMinutesTime
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype now)
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
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype now)
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
  edoc <- randomUpdate $ (\title doctype -> NewDocument author mcompany title doctype now)

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
  edoc <- randomUpdate $ AttachFile (documentid doc) (fileid file)
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
  edoc <- randomUpdate $ AttachFile (documentid doc) (fileid file)
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
  edoc <- randomUpdate $ (\docid -> AttachFile docid (fileid file))
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
  edoc <- randomUpdate $ AttachFile (documentid doc) (fileid file)
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
  edoc <- randomUpdate $ (\docid -> AttachSealedFile docid (fileid file))
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
  --execute
  edoc <- randomUpdate $ AttachSealedFile (documentid doc) (fileid file)
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
  edoc <- randomUpdate $ ChangeMainfile (documentid doc) (fileid file)
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
  edoc <- randomUpdate $ (\docid -> ChangeMainfile docid (fileid file))
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
  edoc <- dbUpdate $ ResetSignatoryDetails (documentid doc) [(sd, [SignatoryAuthor])] mt
  --assert
  validTest $ assertLeft edoc

testPreparationResetSignatoryDetailsAlwaysRight :: DB ()
testPreparationResetSignatoryDetailsAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  mt <- rand 10 arbitrary
  --execute
  edoc <- dbUpdate $ ResetSignatoryDetails (documentid doc) [(emptySignatoryDetails, [SignatoryAuthor])] mt
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
  edoc <- dbUpdate $ ResetSignatoryDetails a [(emptySignatoryDetails, [SignatoryAuthor])] mt
  --assert
  validTest $ assertLeft edoc

testNoDocumentAttachCSVUploadAlwaysLeft :: DB ()
testNoDocumentAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  edoc <- randomUpdate $ AttachCSVUpload
  --assert
  validTest $ assertLeft edoc

testNotPreparationAttachCSVUploadAlwaysLeft :: DB ()
testNotPreparationAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  edoc <- randomUpdate $ AttachCSVUpload (documentid doc)
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadAuthorIndexLeft :: DB ()
testPreparationAttachCSVUploadAuthorIndexLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  csvupload <- rand 10 arbitrary
  let Just ai = authorIndex (documentsignatorylinks doc)
  --execute
  edoc <- dbUpdate $ AttachCSVUpload (documentid doc)
          (signatorylinkid ((documentsignatorylinks doc) !! ai))
          (csvupload { csvsignatoryindex = ai })
  --assert
  validTest $ assertLeft edoc

authorIndex :: [SignatoryLink] -> Maybe Int
authorIndex sls = case catMaybes $ zipWith (\sl i -> if isAuthor sl then Just i else Nothing) sls [0..] of
  [] -> Nothing
  x:_ -> Just x

testPreparationAttachCSVUploadNonExistingSignatoryLink :: DB ()
testPreparationAttachCSVUploadNonExistingSignatoryLink = doTimes 3 $ do
  -- setup
  csvupload <- rand 10 arbitrary
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- dbUpdate $ AttachCSVUpload (documentid doc) 
          (SignatoryLinkID 0) csvupload
  --assert
  validTest $ assertLeft edoc


testCreateFromSharedTemplate :: DB ()
testCreateFromSharedTemplate = do
  user <- addNewRandomAdvancedUser
  docid <- fmap documentid $ addRandomDocumentWithAuthorAndCondition user (not . isAttachment)
  tmpdoc <- fmap fromJust $ dbQuery $ GetDocumentByDocumentID docid
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else fmap fromRight $ dbUpdate (TemplateFromDocument docid)
  newuser <- addNewRandomAdvancedUser
  mt <- rand 10 arbitrary
  doc' <- fmap fromRight $ dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor newuser Nothing (documentid doc) mt
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
  edoc <- randomUpdate $ AddDocumentAttachment (documentid doc) (fileid file)
  --assert
  validTest $ assertLeft edoc

testAddDocumentAttachmentOk :: DB ()
testAddDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  file <- addNewRandomFile
  --execute
  edoc <- randomUpdate $ AddDocumentAttachment (documentid doc) (fileid file)
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
  edoc1 <- randomUpdate $ UpdateSigAttachments (documentid doc) [att1, att2]

  edoc2 <- randomUpdate $ DeleteSigAttachment (documentid doc) email1 (fileid file1)

  edoc3 <- randomUpdate $ SaveSigAttachment (documentid doc) name1 email1 (fileid file2)

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
  mdoc <- randomUpdate $ DocumentFromSignatoryData
  validTest $ assertLeft mdoc

testDocumentFromSignatoryDataSucceedsExists :: DB ()
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc)
  validTest $ assertRight mdoc

testTimeoutDocumentNonSignableLeft :: DB ()
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  etdoc <- dbUpdate $ TimeoutDocument (documentid doc) mt
  validTest $ assertLeft etdoc

testTimeoutDocumentSignableNotPendingLeft :: DB ()
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ TimeoutDocument (documentid doc)
  validTest $ assertLeft etdoc

testTimeoutDocumentSignablePendingRight :: DB ()
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  --execute
  etdoc <- randomUpdate $ TimeoutDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testTimeoutDocumentSignableNotLeft :: DB ()
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ TimeoutDocument
  validTest $ assertLeft etdoc

testSignDocumentNonSignableLeft :: DB ()
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc

testSignDocumentSignableNotPendingLeft :: DB ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc

testSignDocumentSignablePendingRight :: DB ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (any (isSignatory &&^ (not . hasSigned) &&^ hasSeen) . documentsignatorylinks))
  let Just sl = find (isSignatory &&^ (not . hasSigned) &&^ hasSeen) (documentsignatorylinks doc)
  etdoc <- randomUpdate $ SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
  etdoc2 <- if (isRight etdoc &&
                all (isSignatory =>>^ hasSigned) (documentsignatorylinks $ fromRight etdoc))
            then randomUpdate $ CloseDocument (documentid doc)
            else return etdoc

  validTest $ do
    assertRight etdoc2
    assertInvariants $ fromRight etdoc2

testSignDocumentNotLeft :: DB ()
testSignDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SignDocument
  validTest $ assertLeft etdoc

testPreparationToPendingNotSignableLeft :: DB ()
testPreparationToPendingNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }
  etdoc <- randomUpdate $ PreparationToPending (documentid doc)
  validTest $ assertLeft etdoc

testPreparationToPendingSignableNotPreparationLeft :: DB ()
testPreparationToPendingSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = documentAllStatuses \\ [Preparation]
         }
  etdoc <- randomUpdate $ PreparationToPending (documentid doc)
  validTest $ assertLeft etdoc

testPreparationToPendingNotLeft :: DB ()
testPreparationToPendingNotLeft = doTimes 100 $ do
  etdoc <- randomUpdate $ PreparationToPending
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
  etdoc <- randomUpdate $ PreparationToPending (documentid doc)
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
  etdoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ assertLeft etdoc

testRejectDocumentSignableNotPendingLeft :: DB ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ assertLeft etdoc

testRejectDocumentNotLeft :: DB ()
testRejectDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate RejectDocument
  validTest $ assertLeft etdoc

testRejectDocumentSignablePendingRight :: DB ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  edoc <- randomUpdate $ RejectDocument (documentid doc) slid
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: DB ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
         (isPending &&^ (all (isNothing . maybereadinvite) . documentsignatorylinks))
  forM_ (documentsignatorylinks doc) $ \sl -> Log.debug $ "maybereadinvite: " ++ show (maybereadinvite sl)
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  time <- getMinutesTime
  edoc <- dbUpdate $ MarkInvitationRead (documentid doc) slid time
  validTest $ do
    assertRight edoc
    let Just sl = getSigLinkFor (fromRight edoc) slid
    assertEqual "Invitation read time should be set." (Just time) (maybereadinvite sl)

testMarkInvitationReadDocDoesntExist :: DB ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  _ <- randomUpdate $ MarkInvitationRead
  validTest $ assertSuccess

testMarkDocumentSeenNotSignableLeft :: DB ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         }

  validTest (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
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
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertLeft etdoc)

testMarkDocumentSeenNotLeft :: DB ()
testMarkDocumentSeenNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ MarkDocumentSeen
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
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
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
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh
                assertLeft etdoc)

testSetInvitationDeliveryStatusNotSignableLeft :: DB ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
  validTest $ assertLeft edoc


testSetInvitationDeliveryStatusNotLeft :: DB ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus
  validTest $ assertLeft etdoc

testSetInvitationDeliveryStatusSignableRight :: DB ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  slid <- rand 10 $ elements (map signatorylinkid (documentsignatorylinks doc))
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc) slid
  validTest $ assertRight etdoc

testSetDocumentTimeoutTimeNotSignableLeft :: DB ()
testSetDocumentTimeoutTimeNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc)
  validTest $ assertLeft edoc

testSetDocumentTimeoutTimeSignableRight :: DB ()
testSetDocumentTimeoutTimeSignableRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  etdoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc)
  validTest $ assertRight etdoc

testSetDocumentTimeoutTimeNotLeft :: DB ()
testSetDocumentTimeoutTimeNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTimeoutTime
  validTest $ assertLeft etdoc

testSetDocumentTagsNotLeft :: DB ()
testSetDocumentTagsNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTags
  validTest $ assertLeft etdoc

testSetDocumentTagsRight :: DB ()
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  tags <- rand 10 arbitrary
  edoc <- randomUpdate $ SetDocumentTags (documentid doc) tags
  validTest $ do
    assertRight edoc
    assertEqual "Tags should be equal" tags $ documenttags (fromRight edoc)

testSetDocumentUINotLeft :: DB ()
testSetDocumentUINotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentUI
  validTest $ assertLeft etdoc

testSetDocumentUIRight :: DB ()
testSetDocumentUIRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  etdoc <- randomUpdate $ SetDocumentUI (documentid doc)
  validTest $ assertRight etdoc

testCloseDocumentSignableAwaitingAuthorJust :: DB ()
testCloseDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor]
         , randomDocumentCondition = ((hasSeen . getAuthorSigLink) &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . documentsignatorylinks))
         }

  let Just sl = getAuthorSigLink doc
  etdoc <- randomUpdate (SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl)) >>
           randomUpdate (CloseDocument (documentid doc))
  validTest $ assertRight etdoc

testCloseDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCloseDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor, Pending]
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertLeft etdoc

testCloseDocumentNotSignableNothing :: DB ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertLeft etdoc

testCloseDocumentNotNothing :: DB ()
testCloseDocumentNotNothing = doTimes 10 $ do
  etdoc <- randomUpdate $ CloseDocument
  validTest $ assertLeft etdoc


testCancelDocumentSignableAwaitingAuthorJust :: DB ()
testCancelDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor]
         , randomDocumentCondition = ((hasSeen . getAuthorSigLink) &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . documentsignatorylinks))
         }

  let Just sl = getAuthorSigLink doc
  etdoc <- randomUpdate (SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl)) >>
           randomUpdate (CancelDocument (documentid doc) ManualCancel)
  validTest $ assertRight etdoc

testCancelDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCancelDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor, Pending]
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  etdoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel

  validTest $ assertRight etdoc

testCancelDocumentNotSignableNothing :: DB ()
testCancelDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  etdoc <- randomUpdate $ CancelDocument (documentid doc) ManualCancel
  validTest $ assertLeft etdoc

testCancelDocumentNotNothing :: DB ()
testCancelDocumentNotNothing = doTimes 10 $ do
  etdoc <- randomUpdate $ (\did -> CancelDocument did ManualCancel)
  validTest $ assertLeft etdoc


testPendingToAwaitingAuthorDocumentSignableAwaitingAuthorJust :: DB ()
testPendingToAwaitingAuthorDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = ((hasSeen . getAuthorSigLink) &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . documentsignatorylinks))
         }

  let Just sl = getAuthorSigLink doc
  etdoc <- randomUpdate (PendingToAwaitingAuthor (documentid doc)) >>
           randomUpdate (SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl))
           
  validTest $ assertRight etdoc

testPendingToAwaitingAuthorDocumentSignableNotAwaitingAuthorNothing :: DB ()
testPendingToAwaitingAuthorDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [Pending]
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  etdoc <- randomUpdate $ PendingToAwaitingAuthor (documentid doc)

  validTest $ assertRight etdoc

testPendingToAwaitingAuthorDocumentNotSignableNothing :: DB ()
testPendingToAwaitingAuthorDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentAllTypes \\ documentSignableTypes
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }
  etdoc <- randomUpdate $ PendingToAwaitingAuthor (documentid doc)
  validTest $ assertLeft etdoc

testPendingToAwaitingAuthorDocumentNotNothing :: DB ()
testPendingToAwaitingAuthorDocumentNotNothing = doTimes 10 $ do
  etdoc <- randomUpdate $ (\did -> PendingToAwaitingAuthor did)
  validTest $ assertLeft etdoc


testSetDocumentTitleNotLeft :: DB ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTitle
  validTest $ assertLeft etdoc

testSetDocumentTitleRight :: DB ()
testSetDocumentTitleRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed)
         }
  let title = BS.fromString "my new cool title"
  etdoc <- randomUpdate $ SetDocumentTitle (documentid doc) title
  validTest $ do
    assertRight etdoc
    let Right doc' = etdoc
    assertEqual "Title is set properly" title (documenttitle doc')

testSetDocumentDaysToSignNotLeft :: DB ()
testSetDocumentDaysToSignNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDaysToSign
  validTest $ assertLeft etdoc

testRemoveDocumentDaysToSignNotLeft :: DB ()
testRemoveDocumentDaysToSignNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ RemoveDaysToSign
  validTest $ assertLeft etdoc

testSetDocumentDaysToSignRight :: DB ()
testSetDocumentDaysToSignRight = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentCondition = (not . isClosed) &&^ (isNothing . documentdaystosign)
         }
  let daystosign = 15
  etdoc1 <- randomUpdate $ SetDaysToSign (documentid doc) daystosign
  etdoc2 <- randomUpdate $ RemoveDaysToSign (documentid doc)
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
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name value]
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
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name value]
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
  _ <- dbUpdate $ SetUserCompany (userid author) (Just (companyid company))
  Just author' <- dbQuery $ GetUserByID (userid author)
  did <- addRandomDocumentWithAuthor author'
  _ <- dbUpdate $ SetDocumentTags did [DocumentTag name1 value1, DocumentTag name2 value2]
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
