{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import DB.Classes
import User.Model
import Doc.DocState
import Doc.DocUtils
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MinutesTime

import Happstack.State
import Data.Maybe
import Database.HDBC.PostgreSQL
import Control.Monad
import Data.List
import Test.Framework
import Test.QuickCheck

docStateTests :: Connection -> Test
docStateTests conn = testGroup "DocState" [
  testThat "SetDocumentTitle fails when doc doesn't exist" conn testSetDocumentTitleNotLeft,
  
  testThat "CloseDocument fails when doc is not signable" conn testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" conn testCloseDocumentNotNothing,
  testThat "CloseDocument succeeds when doc is signable and awaiting author" conn testCloseDocumentSignableAwaitingAuthorJust,
  testThat "CloseDocument fails when doc is signable and awaiting author" conn testCloseDocumentSignableNotAwaitingAuthorNothing,
  
  testThat "SetDocumentTags fails when does not exist" conn testSetDocumentTagsNotLeft,
  testThat "SetDocumentTags succeeds" conn testSetDocumentTagsRight,
  
  testThat "SetDocumentUI fails when does not exist" conn testSetDocumentUINotLeft,
  testThat "SetDocumentUI succeeds" conn testSetDocumentUIRight,
  
  testThat "SetDocumentTimeoutTime fails when does not exist" conn testSetDocumentTimeoutTimeNotLeft,
  testThat "SetDocumentTimeoutTime fails when not signable" conn testSetDocumentTimeoutTimeNotSignableLeft,
  testThat "SetDocumentTimeoutTime succeeds when signable" conn testSetDocumentTimeoutTimeSignableRight,
  
  testThat "SetInvitationDeliveryStatus fails when not signable" conn testSetInvitationDeliveryStatusNotSignableLeft,
  testThat "SetInvitationDeliveryStatus fails when doc does not exist" conn testSetInvitationDeliveryStatusNotLeft,
  testThat "SetInvitationDeliveryStatus succeeds if signable" conn testSetInvitationDeliveryStatusSignableRight,  
  
  testThat "MarkDocumentSeen fails when not signable" conn testMarkDocumentSeenNotSignableLeft,
  testThat "MarkDocumentSeen fails when closed or preparation" conn testMarkDocumentSeenClosedOrPreparationLeft,
  testThat "MarkDocumentSeen fails when doc does not exist" conn testMarkDocumentSeenNotLeft,
  testThat "MarkDocumentSeen succeeds when siglink and magichash match" conn testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight,
  testThat "MarkDocumentSeen fails when the siglink matches but magichash does not" conn testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft,
  
  testThat "MarkInvitationRead never fails" conn testMarkInvitationRead,
  testThat "MarkInvitationRead never fails when doc doesn't exist" conn testMarkInvitationReadDocDoesntExist,
  
  testThat "RejectDocument succeeds when signable and pending" conn testRejectDocumentSignablePendingRight,
  testThat "RejectDocument fails when document doesn't exist" conn testRejectDocumentNotLeft,
  testThat "RejectDocument fails when signable but not pending" conn testRejectDocumentSignableNotPendingLeft,
  testThat "RejectDocument fails when not signable" conn testRejectDocumentNotSignableLeft,
  
  testThat "AuthorSignDocument succeeds when signable and preparation" conn testAuthorSignDocumentSignablePreparationRight,
  testThat "AuthorSignDocument fails when document doesn't exist" conn testAuthorSignDocumentNotLeft,
  testThat "AuthorSignDocument fails when signable but not preparation" conn testAuthorSignDocumentSignableNotPreparationLeft,
  testThat "AuthorSignDocument fails when not signable" conn testAuthorSignDocumentNotSignableLeft,
  
  testThat "AuthorSendDocument succeeds when signable and preparation" conn testAuthorSendDocumentSignablePreparationRight,
  testThat "AuthorSendDocument fails when document doesn't exist" conn testAuthorSendDocumentNotLeft,
  testThat "AuthorSendDocument fails when signable but not preparation" conn testAuthorSendDocumentSignableNotPreparationLeft,
  testThat "AuthorSendDocument fails when not signable" conn testAuthorSendDocumentNotSignableLeft,
  
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
  
  testThat "when I call update document, it doesn't change the document id" conn testDocumentUpdateDoesNotChangeID,
  testThat "when I call update document, i can change the title" conn testDocumentUpdateCanChangeTitle,
  
  testThat "when I attach a file to a real document, it ALWAYS returns Right" conn testDocumentAttachAlwaysRight,
  testThat "when I attach a file to a bad docid, it ALWAYS returns Left" conn testNoDocumentAttachAlwaysLeft,
  testThat "when I attach a file, the file is attached" conn testDocumentAttachHasAttachment,
  
  testThat "when I attach a sealed file to a bad docid, it always returns left" conn testNoDocumentAttachSealedAlwaysLeft,
  testThat "when I attach a sealed file to a real doc, it always returns Right" conn testDocumentAttachSealedAlwaysRight,
  
  testThat "when I call updateDocument, it fails when the doc doesn't exist" conn testNoDocumentUpdateDocumentAlwaysLeft,
  testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" conn testNotPreparationUpdateDocumentAlwaysLeft,
  testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" conn testPreparationUpdateDocumentAlwaysRight,

  testThat "when I create document from shared template author custom fields are stored" conn testCreateFromSharedTemplate,

  testThat "when I call updateDocumentSimple, it fails when the doc doesn't exist" conn testNoDocumentUpdateDocumentSimpleAlwaysLeft,
  testThat "When I call updateDocumentSimple with a doc that is not in Preparation, always returns left" conn testNotPreparationUpdateDocumentSimpleAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" conn testPreparationUpdateDocumentSimpleAlwaysRight,
  testThat "when I call attachcsvupload with a doc that does not exist, always returns left" conn testNoDocumentAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload with a doc that is not in preparation, always returns left" conn testNotPreparationAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload and the csvindex is the author, return left" conn testPreparationAttachCSVUploadAuthorIndexLeft,
  testThat "when I call attachcsvupload and the csvindex is negative, return left" conn testPreparationAttachCSVUploadIndexNeg,
  testThat "when I call attachcsvupload and the csvindex is too large, return Left" conn testPreparationAttachCSVUploadIndexGreaterThanLength,
  testThat "updateDocumentAttachment fails if not in preparation" conn testUpdateDocumentAttachmentFailsIfNotPreparation,
  testThat "updateDocumentAttachment fails if in preparation but not all ids are found" conn testUpdateDocumentAttachmentFailsNotFound,
  testThat "updateDocumentAttachment doesn't fail if there's no attachments" conn testUpdateDocumentAttachmentOk,
  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "documentFromSignatoryData fails when document doesn't exist" conn testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "documentFromSignatoryData succeeds when document exists" conn testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" conn testTimeoutDocumentNonSignableLeft
  ]

testNewDocumentDependencies :: DB ()
testNewDocumentDependencies = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  -- execute
  edoc <- randomUpdate $ NewDocument author mcompany 
  -- assert
  validTest $ do 
    assertRight edoc
    assertInvariants $ fromRight edoc
  
testDocumentCanBeCreatedAndFetchedByID :: DB ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  Right doc <- randomUpdate $ NewDocument author mcompany
  -- execute
  mdoc <- query $ GetDocumentByDocumentID (documentid doc)
  -- assert
  validTest $ do
    assertJust mdoc
    assert $ sameDocID doc (fromJust mdoc)
    assertInvariants (fromJust mdoc)

testDocumentCanBeCreatedAndFetchedByAllDocs :: DB ()
testDocumentCanBeCreatedAndFetchedByAllDocs = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  -- execute
  Right doc <- randomUpdate $ NewDocument author mcompany
  docs <- query $ GetDocuments Nothing
  -- assert
  validTest $ do
    assertJust $ find (sameDocID doc) docs
    assertInvariants $ fromJust $ find (sameDocID doc) docs

testDocumentUpdateDoesNotChangeID :: DB ()
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

testDocumentUpdateCanChangeTitle :: DB ()
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
    
testDocumentAttachAlwaysRight :: DB ()
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

testNoDocumentAttachAlwaysLeft :: DB ()
testNoDocumentAttachAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ AttachFile
  --assert
  validTest $ do
    assertLeft edoc

testDocumentAttachHasAttachment :: DB ()
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

testNoDocumentAttachSealedAlwaysLeft :: DB ()
testNoDocumentAttachSealedAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ AttachSealedFile
  --assert
  validTest $ assertLeft edoc
  
testDocumentAttachSealedAlwaysRight :: DB ()
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

testNotPreparationUpdateDocumentAlwaysLeft :: DB ()
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

testPreparationUpdateDocumentAlwaysRight :: DB ()
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

testNoDocumentUpdateDocumentAlwaysLeft :: DB ()
testNoDocumentUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  --execute
  -- non-existent docid
  edoc <- randomUpdate $ UpdateDocument
  --assert
  validTest $ assertLeft edoc

testNotPreparationUpdateDocumentSimpleAlwaysLeft :: DB ()
testNotPreparationUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  let sd = signatoryDetailsFromUser author Nothing
  --execute
  edoc <- update $ UpdateDocumentSimple (documentid doc) (sd, author) []
  --assert
  validTest $ assertLeft edoc

testPreparationUpdateDocumentSimpleAlwaysRight :: DB ()
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

testNoDocumentUpdateDocumentSimpleAlwaysLeft :: DB ()
testNoDocumentUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  author <- addNewRandomUser
  --execute
  -- non-existent docid
  edoc <- update $ UpdateDocumentSimple a (emptySignatoryDetails, author) []  
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
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute
  edoc <- randomUpdate $ AttachCSVUpload (documentid doc)
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadAuthorIndexLeft :: DB ()
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

testPreparationAttachCSVUploadIndexNeg :: DB ()
testPreparationAttachCSVUploadIndexNeg = doTimes 10 $ do
  -- setup
  csvupload <- untilCondition (\c -> (csvsignatoryindex c) < 0) $ rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ AttachCSVUpload (documentid doc) csvupload
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadIndexGreaterThanLength :: DB ()
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

testCreateFromSharedTemplate::DB ()
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
           
testUpdateDocumentAttachmentFailsIfNotPreparation :: DB ()
testUpdateDocumentAttachmentFailsIfNotPreparation = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  --execute                     
  edoc <- randomUpdate $ UpdateDocumentAttachments (documentid doc)
  --assert
  validTest $ assertLeft edoc

testUpdateDocumentAttachmentFailsNotFound :: DB ()
testUpdateDocumentAttachmentFailsNotFound = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ UpdateDocumentAttachments (documentid doc) [DocumentID 1] []
  --assert
  validTest $ assertLeft edoc
  
testUpdateDocumentAttachmentOk :: DB ()
testUpdateDocumentAttachmentOk = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- update $ UpdateDocumentAttachments (documentid doc) [] []
  --assert
  validTest $ assertRight edoc
          
testDocumentFromSignatoryDataFailsDoesntExist :: DB ()
testDocumentFromSignatoryDataFailsDoesntExist = doTimes 10 $ do
  mdoc <- randomUpdate $ DocumentFromSignatoryData
  validTest $ assertLeft mdoc

testDocumentFromSignatoryDataSucceedsExists :: DB ()
testDocumentFromSignatoryDataSucceedsExists = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  mdoc <- randomUpdate $ DocumentFromSignatoryData (documentid doc)
  validTest $ assertRight mdoc
          
testTimeoutDocumentNonSignableLeft :: DB ()
testTimeoutDocumentNonSignableLeft = doTimes 10 $ do
  mt <- rand 10 arbitrary
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  -- execute
  etdoc <- update $ TimeoutDocument (documentid doc) mt
  validTest $ assertLeft etdoc
    
testTimeoutDocumentSignableNotPendingLeft :: DB ()
testTimeoutDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ TimeoutDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testTimeoutDocumentSignablePendingRight :: DB ()
testTimeoutDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
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
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testSignDocumentSignableNotPendingLeft :: DB ()
testSignDocumentSignableNotPendingLeft = doTimes 10 $ do  
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ SignDocument (documentid doc)
  validTest $ assertLeft etdoc
    
testSignDocumentSignablePendingRight :: DB ()
testSignDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending &&^ (any (isSignatory &&^ (not . hasSigned) &&^ hasSeen) . documentsignatorylinks))
  let Just sl = find (isSignatory &&^ (not . hasSigned) &&^ hasSeen) (documentsignatorylinks doc)
  etdoc <- randomUpdate $ SignDocument (documentid doc) (signatorylinkid sl)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testSignDocumentNotLeft :: DB ()
testSignDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SignDocument
  validTest $ assertLeft etdoc

testAuthorSendDocumentNotSignableLeft :: DB ()
testAuthorSendDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ assertLeft etdoc
  
testAuthorSendDocumentSignableNotPreparationLeft :: DB ()
testAuthorSendDocumentSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPreparation))
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSendDocumentNotLeft :: DB ()
testAuthorSendDocumentNotLeft = doTimes 100 $ do
  etdoc <- randomUpdate $ AuthorSendDocument
  validTest $ assertLeft etdoc
  
testAuthorSendDocumentSignablePreparationRight :: DB ()
testAuthorSendDocumentSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isSignable &&^ isPreparation &&^ (any isSignatory . documentsignatorylinks))
  etdoc <- randomUpdate $ AuthorSendDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc

testAuthorSignDocumentNotSignableLeft :: DB ()
testAuthorSignDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc
  
testAuthorSignDocumentSignableNotPreparationLeft :: DB ()
testAuthorSignDocumentSignableNotPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPreparation))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ assertLeft etdoc

testAuthorSignDocumentNotLeft :: DB ()
testAuthorSignDocumentNotLeft = doTimes 10 $ do
  edoc <- randomUpdate AuthorSignDocument
  validTest $ assertLeft edoc
  
testAuthorSignDocumentSignablePreparationRight :: DB ()
testAuthorSignDocumentSignablePreparationRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author
         (isSignable &&^ isPreparation &&^ (not . hasSigned . getAuthorSigLink) &&^ (isSignatory . getAuthorSigLink))
  etdoc <- randomUpdate $ AuthorSignDocument (documentid doc)
  validTest $ do
    assertRight etdoc
    assertInvariants $ fromRight etdoc


testRejectDocumentNotSignableLeft :: DB ()
testRejectDocumentNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ RejectDocument (documentid doc)  
  validTest $ assertLeft etdoc
  
testRejectDocumentSignableNotPendingLeft :: DB ()
testRejectDocumentSignableNotPendingLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . isPending))
  etdoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ assertLeft etdoc

testRejectDocumentNotLeft :: DB ()
testRejectDocumentNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate RejectDocument
  validTest $ assertLeft etdoc
  
testRejectDocumentSignablePendingRight :: DB ()
testRejectDocumentSignablePendingRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ isPending)
  edoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: DB ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  _edoc <- randomUpdate $ MarkInvitationRead (documentid doc)
  validTest $ assertSuccess
        
testMarkInvitationReadDocDoesntExist :: DB ()
testMarkInvitationReadDocDoesntExist = doTimes 10 $ do
  _ <- randomUpdate $ MarkInvitationRead
  validTest $ assertSuccess
    
testMarkDocumentSeenNotSignableLeft :: DB ()
testMarkDocumentSeenNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  validTest (forEachSignatoryLink doc $ \sl ->             
              when (isNothing $ maybeseeninfo sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertLeft etdoc)
  
testMarkDocumentSeenClosedOrPreparationLeft :: DB ()
testMarkDocumentSeenClosedOrPreparationLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (isClosed ||^ isPreparation))
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
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) (signatorymagichash sl)
                assertRight etdoc)
        
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: DB ()
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (isSignable &&^ (not . (isClosed ||^ isPreparation)))
  validTest (forEachSignatoryLink doc $ \sl ->
              when (not $ hasSeen sl) $ do
                mh <- untilCondition (\a -> a /= (signatorymagichash sl)) $ rand 1000 arbitrary
                etdoc <- randomUpdate $ MarkDocumentSeen (documentid doc) (signatorylinkid sl) mh
                assertLeft etdoc)

testSetInvitationDeliveryStatusNotSignableLeft :: DB ()
testSetInvitationDeliveryStatusNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
  validTest $ assertLeft edoc
        
  
testSetInvitationDeliveryStatusNotLeft :: DB ()
testSetInvitationDeliveryStatusNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus
  validTest $ assertLeft etdoc
  
testSetInvitationDeliveryStatusSignableRight :: DB ()
testSetInvitationDeliveryStatusSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author isSignable
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
  validTest $ assertRight etdoc
  
testSetDocumentTimeoutTimeNotSignableLeft :: DB ()
testSetDocumentTimeoutTimeNotSignableLeft = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  edoc <- randomUpdate $ SetDocumentTimeoutTime (documentid doc)
  validTest $ assertLeft edoc
        
testSetDocumentTimeoutTimeSignableRight :: DB ()
testSetDocumentTimeoutTimeSignableRight = doTimes 10 $ do
  author <- addNewRandomUser
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
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  edoc <- randomUpdate $ SetDocumentTags (documentid doc)
  validTest $ assertRight edoc

testSetDocumentUINotLeft :: DB ()
testSetDocumentUINotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentUI
  validTest $ assertLeft etdoc

testSetDocumentUIRight :: DB ()
testSetDocumentUIRight = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  etdoc <- randomUpdate $ SetDocumentUI (documentid doc)
  validTest $ assertRight etdoc
          
testCloseDocumentSignableAwaitingAuthorJust :: DB ()
testCloseDocumentSignableAwaitingAuthorJust = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
           (isSignable &&^ isAwaitingAuthor &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . documentsignatorylinks))
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertJust etdoc
    
testCloseDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCloseDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author 
         (isSignable &&^ (not . isAwaitingAuthor) &&^ (all (((not . isAuthor) &&^ isSignatory) =>>^ hasSigned) . filter (not . isAuthor) . documentsignatorylinks))
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertNothing etdoc

testCloseDocumentNotSignableNothing :: DB ()
testCloseDocumentNotSignableNothing = doTimes 10 $ do
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isSignable)
  etdoc <- randomUpdate $ CloseDocument (documentid doc)
  validTest $ assertNothing etdoc
    
testCloseDocumentNotNothing :: DB ()
testCloseDocumentNotNothing = doTimes 10 $ do
  etdoc <- randomUpdate $ CloseDocument
  validTest $ assertNothing etdoc
  
testSetDocumentTitleNotLeft :: DB ()
testSetDocumentTitleNotLeft = doTimes 10 $ do
  etdoc <- randomUpdate $ SetDocumentTitle
  validTest $ assertLeft etdoc

assertInvariants :: Document -> DB ()
assertInvariants document = do
  now <- getMinutesTime
  case invariantProblems now document of
    Nothing -> assertSuccess
    Just a  -> assertFailure a
