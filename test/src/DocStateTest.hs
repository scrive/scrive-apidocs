{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import Test.HUnit (assert, assertFailure, Assertion)
import Test.Framework

import User.UserState
import Doc.DocState
import Doc.DocUtils
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.CompanyState
import Doc.Invariants
import MinutesTime

import Happstack.State
import Data.Maybe
import Control.Monad
import Data.List
import Test.QuickCheck

docStateTests :: Test
docStateTests = testGroup "DocState" [
  testThat "SetDocumentTitle fails when doc doesn't exist" testSetDocumentTitleNotLeft,
  
  testThat "CloseDocument fails when doc is not signable" testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" testCloseDocumentNotNothing,
  testThat "CloseDocument succeeds when doc is signable and awaiting author" testCloseDocumentSignableAwaitingAuthorJust,
  testThat "CloseDocument fails when doc is signable and awaiting author" testCloseDocumentSignableNotAwaitingAuthorNothing,
  
  testThat "SetDocumentTags fails when does not exist" testSetDocumentTagsNotLeft,
  testThat "SetDocumentTags succeeds" testSetDocumentTagsRight,
  
  testThat "SetDocumentUI fails when does not exist" testSetDocumentUINotLeft,
  testThat "SetDocumentUI succeeds" testSetDocumentUIRight,
  
  testThat "SetDocumentTimeoutTime fails when does not exist" testSetDocumentTimeoutTimeNotLeft,
  testThat "SetDocumentTimeoutTime fails when not signable" testSetDocumentTimeoutTimeNotSignableLeft,
  testThat "SetDocumentTimeoutTime succeeds when signable" testSetDocumentTimeoutTimeSignableRight,
  
  testThat "SetInvitationDeliveryStatus fails when not signable" testSetInvitationDeliveryStatusNotSignableLeft,
  testThat "SetInvitationDeliveryStatus fails when doc does not exist" testSetInvitationDeliveryStatusNotLeft,
  testThat "SetInvitationDeliveryStatus succeeds if signable" testSetInvitationDeliveryStatusSignableRight,  
  
  testThat "MarkDocumentSeen fails when not signable" testMarkDocumentSeenNotSignableLeft,
  testThat "MarkDocumentSeen fails when closed or preparation" testMarkDocumentSeenClosedOrPreparationLeft,
  testThat "MarkDocumentSeen fails when doc does not exist" testMarkDocumentSeenNotLeft,
  testThat "MarkDocumentSeen succeeds when siglink and magichash match" testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight,
  testThat "MarkDocumentSeen fails when the siglink matches but magichash does not" testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft,
  
  testThat "MarkInvitationRead never fails" testMarkInvitationRead,
  testThat "MarkInvitationRead never fails when doc doesn't exist" testMarkInvitationReadDocDoesntExist,
  
  testThat "RejectDocument succeeds when signable and pending" testRejectDocumentSignablePendingRight,
  testThat "RejectDocument fails when document doesn't exist" testRejectDocumentNotLeft,
  testThat "RejectDocument fails when signable but not pending" testRejectDocumentSignableNotPendingLeft,
  testThat "RejectDocument fails when not signable" testRejectDocumentNotSignableLeft,
  
  testThat "AuthorSignDocument succeeds when signable and preparation" testAuthorSignDocumentSignablePreparationRight,
  testThat "AuthorSignDocument fails when document doesn't exist" testAuthorSignDocumentNotLeft,
  testThat "AuthorSignDocument fails when signable but not preparation" testAuthorSignDocumentSignableNotPreparationLeft,
  testThat "AuthorSignDocument fails when not signable" testAuthorSignDocumentNotSignableLeft,
  
  testThat "AuthorSendDocument succeeds when signable and preparation" testAuthorSendDocumentSignablePreparationRight,
  testThat "AuthorSendDocument fails when document doesn't exist" testAuthorSendDocumentNotLeft,
  testThat "AuthorSendDocument fails when signable but not preparation" testAuthorSendDocumentSignableNotPreparationLeft,
  testThat "AuthorSendDocument fails when not signable" testAuthorSendDocumentNotSignableLeft,
  
  testThat "SignDocument fails when doc doesn't exist" testSignDocumentNotLeft,
  testThat "SignDocument succeeds when doc is Signable and Pending" testSignDocumentSignablePendingRight,
  testThat "SignDocument fails when the document is Signable but not in Pending" testSignDocumentSignableNotPendingLeft,
  testThat "SignDocument fails when document is not signable" testSignDocumentNonSignableLeft,  
  
  testThat "TimeoutDocument fails when doc doesn't exist" testTimeoutDocumentSignableNotLeft,
  testThat "TimeoutDocument succeeds when doc is Signable and Pending" testTimeoutDocumentSignablePendingRight,
  testThat "TimeoutDocument fails when the document is Signable but not in Pending" testTimeoutDocumentSignableNotPendingLeft,
  testThat "create document and check invariants" testNewDocumentDependencies,
  testThat "can create new document and read it back with the returned id" testDocumentCanBeCreatedAndFetchedByID,
  testThat "can create new document and read it back with GetDocuments" testDocumentCanBeCreatedAndFetchedByAllDocs,
  
  testThat "when I call update document, it doesn't change the document id" testDocumentUpdateDoesNotChangeID,
  testThat "when I call update document, i can change the title" testDocumentUpdateCanChangeTitle,
  
  testThat "when I attach a file to a real document, it ALWAYS returns Right" testDocumentAttachAlwaysRight,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" testDocumentAttachHasAttachment,
  
  testThat "when I attach a sealed file to a bad docid, it always returns left" testNoDocumentAttachSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" testDocumentAttachSealedAlwaysRight,
  
  testThat "when I call updateDocument, it fails when the doc doesn't exist" testNoDocumentUpdateDocumentAlwaysLeft,
  testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" testNotPreparationUpdateDocumentAlwaysLeft,
  testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" testPreparationUpdateDocumentAlwaysRight,

  testThat "when I create document from shared template author custom fields are stored" testCreateFromSharedTemplate,

  testThat "when I call updateDocumentSimple, it fails when the doc doesn't exist" testNoDocumentUpdateDocumentSimpleAlwaysLeft,
  testThat "When I call updateDocumentSimple with a doc that is not in Preparation, always returns left" testNotPreparationUpdateDocumentSimpleAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" testPreparationUpdateDocumentSimpleAlwaysRight,
  testThat "when I call attachcsvupload with a doc that does not exist, always returns left" testNoDocumentAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload with a doc that is not in preparation, always returns left" testNotPreparationAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload and the csvindex is the author, return left" testPreparationAttachCSVUploadAuthorIndexLeft,
  testThat "when I call attachcsvupload and the csvindex is negative, return left" testPreparationAttachCSVUploadIndexNeg,
  testThat "when I call attachcsvupload and the csvindex is too large, return Left" testPreparationAttachCSVUploadIndexGreaterThanLength,
  testThat "updateDocumentAttachment fails if not in preparation" testUpdateDocumentAttachmentFailsIfNotPreparation,
  testThat "updateDocumentAttachment fails if in preparation but not all ids are found" testUpdateDocumentAttachmentFailsNotFound,
  testThat "updateDocumentAttachment doesn't fail if there's no attachments" testUpdateDocumentAttachmentOk,
  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "documentFromSignatoryData fails when document doesn't exist" testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "documentFromSignatoryData succeeds when document exists" testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" testTimeoutDocumentNonSignableLeft
  ]

testNewDocumentDependencies :: Assertion
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (query . GetCompany) $ usercompany author
  -- execute
  edoc <- randomUpdate $ NewDocument author mcompany 
  -- assert
  validTest $ do 
    assertRight edoc
    assertInvariants $ fromRight edoc
  
testDocumentCanBeCreatedAndFetchedByID :: Assertion
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (query . GetCompany) $ usercompany author
  Right doc <- randomUpdate $ NewDocument author mcompany
  -- execute
  mdoc <- query $ GetDocumentByDocumentID (documentid doc)
  -- assert
  validTest $ do
    assertJust mdoc
    assert $ sameDocID doc (fromJust mdoc)
    assertInvariants (fromJust mdoc)

testDocumentCanBeCreatedAndFetchedByAllDocs :: Assertion
testDocumentCanBeCreatedAndFetchedByAllDocs = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (query . GetCompany) $ usercompany author
  -- execute
  Right doc <- randomUpdate $ NewDocument author mcompany
  docs <- query $ GetDocuments Nothing
  -- assert
  validTest $ do
    assertJust $ find (sameDocID doc) docs
    assertInvariants $ fromJust $ find (sameDocID doc) docs

testDocumentUpdateDoesNotChangeID :: Assertion
testDocumentUpdateDoesNotChangeID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  (mt, a, b, c, d, e, f, g) <- rand 10 arbitrary
  doc <-  addRandomDocumentWithAuthorAndCondition author isPreparation

  let sd = signatoryDetailsFromUser author Nothing
  -- execute      
  enewdoc <- update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f g
  --assert
  validTest $ do
    assertRight enewdoc
    assert $ sameDocID doc $ fromRight enewdoc
    assertInvariants $ fromRight enewdoc

testDocumentUpdateCanChangeTitle :: Assertion
testDocumentUpdateCanChangeTitle = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  (mt, a, b, c, d, e, f, g) <- rand 10 arbitrary
  
  --execute
  let sd = signatoryDetailsFromUser author Nothing
  enewdoc <- update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f g
  --assert
  validTest $ do
    assertRight enewdoc
    assert $ (documenttitle $ fromRight enewdoc) == a
    assertInvariants $ fromRight enewdoc
    
testDocumentAttachAlwaysRight :: Assertion
testDocumentAttachAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  --execute
  edoc <- randomUpdate $ AttachFile (documentid doc)
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testNoDocumentAttachAlwaysLeft :: Assertion
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ AttachFile
  --assert
  validTest $ do
    assertLeft edoc

testDocumentAttachHasAttachment :: Assertion
testDocumentAttachHasAttachment = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  a <- rand 10 arbitrary
  --execute
  edoc <- randomUpdate $ AttachFile (documentid doc) a
  --assert
  validTest $ do
    assertRight edoc
    assertJust $ find ((== a) . filename) (documentfiles $ fromRight edoc)
    assertInvariants $ fromRight edoc

testNoDocumentAttachSealedAlwaysLeft :: Assertion
testNoDocumentAttachSealedAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ AttachSealedFile
  --assert
  validTest $ assertLeft edoc
  
testDocumentAttachSealedAlwaysRight :: Assertion
testDocumentAttachSealedAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  --execute
  edoc <- randomUpdate $ AttachSealedFile (documentid doc)
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testNotPreparationUpdateDocumentAlwaysLeft :: Assertion
testNotPreparationUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  (mt, a, b, c, d, e, f, g) <- rand 10 arbitrary

  let sd = signatoryDetailsFromUser author Nothing
  -- execute      
  enewdoc <- update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f g

  --assert
  validTest $ assertLeft enewdoc

testPreparationUpdateDocumentAlwaysRight :: Assertion
testPreparationUpdateDocumentAlwaysRight = doTimes 10 $ do
  -- setup
  (mt, a, b, c, d, e, f, g) <- rand 10 arbitrary
  
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  let sd = signatoryDetailsFromUser author Nothing
                     
  --execute
  enewdoc <- update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f g
  
  --assert
  validTest $ do
    assertRight enewdoc
    assertInvariants $ fromRight enewdoc

testNoDocumentUpdateDocumentAlwaysLeft :: Assertion
testNoDocumentUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ UpdateDocument
  --assert
  validTest $ assertLeft edoc

testNotPreparationUpdateDocumentSimpleAlwaysLeft :: Assertion
testNotPreparationUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  let sd = signatoryDetailsFromUser author Nothing
  --execute
  edoc <- update $ UpdateDocumentSimple (documentid doc) (sd, author) []
  --assert
  validTest $ assertLeft edoc

testPreparationUpdateDocumentSimpleAlwaysRight :: Assertion
testPreparationUpdateDocumentSimpleAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- update $ UpdateDocumentSimple (documentid doc) (emptySignatoryDetails, author) []
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testNoDocumentUpdateDocumentSimpleAlwaysLeft :: Assertion
testNoDocumentUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  author <- addNewRandomUser
  --execute
  -- non-existent docid
  edoc <- update $ UpdateDocumentSimple a (emptySignatoryDetails, author) []  
  --assert
  validTest $ assertLeft edoc
    
testNoDocumentAttachCSVUploadAlwaysLeft :: Assertion
testNoDocumentAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  edoc <- randomUpdate $ AttachCSVUpload
  --assert
  validTest $ assertLeft edoc

testNotPreparationAttachCSVUploadAlwaysLeft :: Assertion
testNotPreparationAttachCSVUploadAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  edoc <- randomUpdate $ AttachCSVUpload (documentid doc)
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadAuthorIndexLeft :: Assertion
testPreparationAttachCSVUploadAuthorIndexLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  csvupload <- rand 10 arbitrary
  let Just ai = authorIndex (documentsignatorylinks doc)
  --execute                     
  edoc <- update $ AttachCSVUpload (documentid doc) (csvupload { csvsignatoryindex = ai })
  --assert
  validTest $ assertLeft edoc

authorIndex :: [SignatoryLink] -> Maybe Int
authorIndex sls = case catMaybes $ zipWith (\sl i -> if isAuthor sl then Just i else Nothing) sls [0..] of
  [] -> Nothing
  x:_ -> Just x

testPreparationAttachCSVUploadIndexNeg :: Assertion
testPreparationAttachCSVUploadIndexNeg = doTimes 10 $ do
  -- setup
  csvupload <- untilCondition (\c -> (csvsignatoryindex c) < 0) $ rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ AttachCSVUpload (documentid doc) csvupload
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadIndexGreaterThanLength :: Assertion
testPreparationAttachCSVUploadIndexGreaterThanLength = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  if length (documentsignatorylinks doc) > 10
    then invalidateTest
    else do
    csvupload <- untilCondition (\c -> (csvsignatoryindex c) >= length (documentsignatorylinks doc)) 
                  $ rand 10 arbitrary
    --execute                     
    edoc <- update $ AttachCSVUpload (documentid doc) csvupload
    --assert
    validTest $ assertLeft edoc

testCreateFromSharedTemplate::Assertion
testCreateFromSharedTemplate = do
  user <- addNewRandomUser
  docid <- addRandomDocumentWithAuthor user
  tmpdoc <- fmap fromJust $ query $ GetDocumentByDocumentID docid
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else fmap fromRight $ update (TemplateFromDocument docid)
  newuser <- addNewRandomUser
  doc' <- fmap fromRight $ update $ SignableFromDocumentIDWithUpdatedAuthor newuser Nothing (documentid doc)
  let [author1] = filter isAuthor $ documentsignatorylinks doc
  let [author2] = filter isAuthor $ documentsignatorylinks doc'
  if (fmap fieldvalue $ signatoryotherfields $ signatorydetails author1) == (fmap fieldvalue $ signatoryotherfields $ signatorydetails author2)
    then assertSuccess
    else assertFailure "Replacing signatory details based on user is loosing fields | SKRIVAPADEV-294" 
           
testUpdateDocumentAttachmentFailsIfNotPreparation :: Assertion
testUpdateDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute                     
  edoc <- randomUpdate $ UpdateDocumentAttachments (documentid doc)
  --assert
  validTest $ assertLeft edoc

testUpdateDocumentAttachmentFailsNotFound :: Assertion
testUpdateDocumentAttachmentFailsNotFound = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ UpdateDocumentAttachments (documentid doc) [DocumentID 1] []
  --assert
  validTest $ assertLeft edoc
  
testUpdateDocumentAttachmentOk :: Assertion
testUpdateDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ UpdateDocumentAttachments (documentid doc) [] []
  --assert
  validTest $ assertRight edoc
          
testDocumentFromSignatoryDataFailsDoesntExist :: Assertion
testDocumentFromSignatoryDataFailsDoesntExist = doTimes 10 $ do
  mdoc <- randomUpdate $ DocumentFromSignatoryData
  validTest $ assertLeft mdoc

testDocumentFromSignatoryDataSucceedsExists :: Assertion
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc)
  validTest $ assertRight mdoc
          
testTimeoutDocumentNonSignableLeft :: Assertion
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  etdoc <- update $ TimeoutDocument (documentid doc) mt
  validTest $ assertLeft etdoc
    
testTimeoutDocumentSignableNotPendingLeft :: Assertion
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ TimeoutDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testTimeoutDocumentSignablePendingRight :: Assertion
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  --execute      
  etdoc <- randomUpdate $ TimeoutDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testTimeoutDocumentSignableNotLeft :: Assertion
testTimeoutDocumentSignableNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ TimeoutDocument
  validTest $ assertLeft etdoc

testSignDocumentNonSignableLeft :: Assertion
testSignDocumentNonSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testSignDocumentSignableNotPendingLeft :: Assertion
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do  
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testSignDocumentSignablePendingRight :: Assertion
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (any (isSignatory &&^ (not . hasSigned) &&^ hasSeen) . documentsignatorylinks))
  let Just sl = find (isSignatory &&^ (not . hasSigned) &&^ hasSeen) (documentsignatorylinks doc)
  etdoc <- randomUpdate $ SignDocument (documentid doc) (signatorylinkid sl)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testSignDocumentNotLeft :: Assertion
testSignDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SignDocument
  validTest $ assertLeft etdoc

testAuthorSendDocumentNotSignableLeft :: Assertion
testAuthorSendDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ assertLeft etdoc
  
testAuthorSendDocumentSignableNotPreparationLeft :: Assertion
testAuthorSendDocumentSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPreparation))
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSendDocumentNotLeft :: Assertion
testAuthorSendDocumentNotLeft = doTimes 100 $ do
  etdoc <- randomUpdate $ AuthorSendDocument
  validTest $ assertLeft etdoc
  
testAuthorSendDocumentSignablePreparationRight :: Assertion
testAuthorSendDocumentSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPreparation)
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testAuthorSignDocumentNotSignableLeft :: Assertion
testAuthorSignDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc
  
testAuthorSignDocumentSignableNotPreparationLeft :: Assertion
testAuthorSignDocumentSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPreparation))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSignDocumentNotLeft :: Assertion
testAuthorSignDocumentNotLeft = doTimes 10 $ do
  edoc <- randomUpdate AuthorSignDocument
  validTest $ assertLeft edoc
  
testAuthorSignDocumentSignablePreparationRight :: Assertion
testAuthorSignDocumentSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isSignable &&^ isPreparation &&^ (not . hasSigned . getAuthorSigLink))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc


testRejectDocumentNotSignableLeft :: Assertion
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ RejectDocument (documentid doc)  
  validTest $ assertLeft etdoc
  
testRejectDocumentSignableNotPendingLeft :: Assertion
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ assertLeft etdoc

testRejectDocumentNotLeft :: Assertion
testRejectDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate RejectDocument
  validTest $ assertLeft etdoc
  
testRejectDocumentSignablePendingRight :: Assertion
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  edoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: Assertion
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  _edoc <- randomUpdate $ MarkInvitationRead (documentid doc)
  validTest $ assertSuccess
        
testMarkInvitationReadDocDoesntExist :: Assertion
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  _ <- randomUpdate $ MarkInvitationRead
  validTest $ assertSuccess
    
testMarkDocumentSeenNotSignableLeft :: Assertion
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  validTest (forEachSignatoryLink doc $ \sl ->             
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertLeft etdoc)
  
testMarkDocumentSeenClosedOrPreparationLeft :: Assertion
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (isClosed ||^ isPreparation))
  validTest (forEachSignatoryLink doc $ \sl -> 
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertLeft etdoc)

testMarkDocumentSeenNotLeft :: Assertion
testMarkDocumentSeenNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ MarkDocumentSeen
  validTest $ assertLeft etdoc
    
forEachSignatoryLink :: Document -> (SignatoryLink -> Assertion) -> Assertion
forEachSignatoryLink doc fn =
  let f [] = return ()
      f (sl:sls) = do
        fn sl
        f sls 
  in f (documentsignatorylinks doc)

testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight :: Assertion
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertRight etdoc)
        
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: Assertion
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl)
                assertLeft etdoc)

testSetInvitationDeliveryStatusNotSignableLeft :: Assertion
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
  validTest $ assertLeft edoc
        
  
testSetInvitationDeliveryStatusNotLeft :: Assertion
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus
  validTest $ assertLeft etdoc
  
testSetInvitationDeliveryStatusSignableRight :: Assertion
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
  validTest $ assertRight etdoc
  
testSetDocumentTimeoutTimeNotSignableLeft :: Assertion
testSetDocumentTimeoutTimeNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc)
  validTest $ assertLeft edoc
        
testSetDocumentTimeoutTimeSignableRight :: Assertion
testSetDocumentTimeoutTimeSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  etdoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc)
  validTest $ assertRight etdoc

testSetDocumentTimeoutTimeNotLeft :: Assertion
testSetDocumentTimeoutTimeNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTimeoutTime
  validTest $ assertLeft etdoc
  
testSetDocumentTagsNotLeft :: Assertion
testSetDocumentTagsNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTags
  validTest $ assertLeft etdoc

testSetDocumentTagsRight :: Assertion
testSetDocumentTagsRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  edoc <- randomUpdate $ SetDocumentTags (documentid doc)
  validTest $ assertRight edoc

testSetDocumentUINotLeft :: Assertion
testSetDocumentUINotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentUI
  validTest $ assertLeft etdoc

testSetDocumentUIRight :: Assertion
testSetDocumentUIRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  etdoc <- randomUpdate $ SetDocumentUI (documentid doc)
  validTest $ assertRight etdoc
          
testCloseDocumentSignableAwaitingAuthorJust :: Assertion
testCloseDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
           (isSignable &&^ isAwaitingAuthor &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . documentsignatorylinks))
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertJust etdoc
    
testCloseDocumentSignableNotAwaitingAuthorNothing :: Assertion
testCloseDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
         (isSignable &&^ (not . isAwaitingAuthor) &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . filter (not . isAuthor) . documentsignatorylinks))
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertNothing etdoc

testCloseDocumentNotSignableNothing :: Assertion
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertNothing etdoc
    
testCloseDocumentNotNothing :: Assertion
testCloseDocumentNotNothing = doTimes 10 $ do
  etdoc <- randomUpdate $ CloseDocument
  validTest $ assertNothing etdoc
  
testSetDocumentTitleNotLeft :: Assertion
testSetDocumentTitleNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTitle
  validTest $ assertLeft etdoc

assertInvariants :: Document -> Assertion
assertInvariants document = do
  now <- getMinutesTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a
