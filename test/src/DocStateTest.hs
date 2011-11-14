{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import DB.Classes
import User.Model
import Doc.Transitory
import Doc.DocUtils
import Doc.DocStateData()
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.Model
import Doc.Invariants
import MinutesTime

import Data.Maybe
import Data.Convertible(convert)
import Database.HDBC(SqlValue)
import Database.HDBC.PostgreSQL
import Control.Monad
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import File.FileID

docStateTests :: Connection -> Test
docStateTests conn = testGroup "DocState" [
  
  testThat "SetDocumentLocale fails when doc doesn't exist" conn testSetDocumentLocaleNotLeft,
  
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
  {-
  testThat "when I call updateDocument, it fails when the doc doesn't exist" conn testNoDocumentUpdateDocumentAlwaysLeft,
  testThat "When I call updateDocument with a doc that is not in Preparation, always returns left" conn testNotPreparationUpdateDocumentAlwaysLeft,
  testThat "when I call updatedocument with a doc that is in Preparation, it always returns Right" conn testPreparationUpdateDocumentAlwaysRight,
-}
  testThat "when I create document from shared template author custom fields are stored" conn testCreateFromSharedTemplate,

  testThat "when I call updateDocumentSimple, it fails when the doc doesn't exist" conn testNoDocumentUpdateDocumentSimpleAlwaysLeft,
  testThat "When I call updateDocumentSimple with a doc that is not in Preparation, always returns left" conn testNotPreparationUpdateDocumentSimpleAlwaysLeft,
  testThat "when I call updatedocumentSimple with a doc that is in Preparation, it always returns Right" conn testPreparationUpdateDocumentSimpleAlwaysRight,
  testThat "when I call attachcsvupload with a doc that does not exist, always returns left" conn testNoDocumentAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload with a doc that is not in preparation, always returns left" conn testNotPreparationAttachCSVUploadAlwaysLeft,
  testThat "when I call attachcsvupload and the csvindex is the author, return left" conn testPreparationAttachCSVUploadAuthorIndexLeft,
  testThat "when I call attachcsvupload and the csvindex is negative, return left" conn testPreparationAttachCSVUploadIndexNeg,
  testThat "when I call attachcsvupload and the csvindex is too large, return Left" conn testPreparationAttachCSVUploadIndexGreaterThanLength,
  testThat "addDocumentAttachment fails if not in preparation" conn testAddDocumentAttachmentFailsIfNotPreparation,
  testThat "addDocumentAttachment doesn't fail if there's no attachments" conn testAddDocumentAttachmentOk,
  
  testThat "removeDocumentAttachment fails if not in preparation" conn testRemoveDocumentAttachmentFailsIfNotPreparation,
  testThat "removeDocumentAttachment doesn't fail if there's no attachments" conn testRemoveDocumentAttachmentOk,
    
  -- we need to do one that tests updateDocumentAttachment where there is an attachment
  testThat "documentFromSignatoryData fails when document doesn't exist" conn testDocumentFromSignatoryDataFailsDoesntExist,
  testThat "documentFromSignatoryData succeeds when document exists" conn testDocumentFromSignatoryDataSucceedsExists,
  testThat "TimeoutDocument fails when document is not signable" conn testTimeoutDocumentNonSignableLeft,
  testProperty "bitfieldDeriveConvertibleId" propbitfieldDeriveConvertibleId
  ]

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
  edoc <- randomUpdate $ NewDocument author mcompany 
  -- assert
  validTest $ do 
    assertRight edoc
    assertInvariants $ fromRight edoc
  
testDocumentCanBeCreatedAndFetchedByID :: DB ()
testDocumentCanBeCreatedAndFetchedByID = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  mcompany <- maybe (return Nothing) (dbQuery . GetCompany) $ usercompany author
  Right doc <- randomUpdate $ NewDocument author mcompany
  -- execute
  mdoc <- doc_query $ GetDocumentByDocumentID (documentid doc)
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
  Right doc <- randomUpdate $ NewDocument author mcompany
  docs <- doc_query $ GetDocuments Nothing
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
  enewdoc <- doc_update $ Reset mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality
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
  enewdoc <- doc_update $ UpdateDocument mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality
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


{-
testNotPreparationUpdateDocumentAlwaysLeft :: DB ()
testNotPreparationUpdateDocumentAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  (mt, a, b, c, d, e, f) <- rand 10 arbitrary

  let sd = signatoryDetailsFromUser author Nothing
  -- execute      
  enewdoc <- doc_update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f AdvancedFunctionality

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
  enewdoc <- doc_update $ UpdateDocument mt (documentid doc) a b c d (sd, r, userid author, Nothing) e f AdvancedFunctionality
  
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
testNotPreparationUpdateDocumentSimpleAlwaysLeft :: DB ()
testNotPreparationUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author (not . isPreparation)
  let sd = signatoryDetailsFromUser author Nothing
  --execute
  edoc <- doc_update $ UpdateDocumentSimple (documentid doc) (sd, author) []
  --assert
  validTest $ assertLeft edoc

testPreparationUpdateDocumentSimpleAlwaysRight :: DB ()
testPreparationUpdateDocumentSimpleAlwaysRight = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute
  edoc <- doc_update $ UpdateDocumentSimple (documentid doc) (emptySignatoryDetails, author) []
  --assert
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testNoDocumentUpdateDocumentSimpleAlwaysLeft :: DB ()
testNoDocumentUpdateDocumentSimpleAlwaysLeft = doTimes 10 $ do
  -- setup
  a <- rand 10 arbitrary
  author <- addNewRandomAdvancedUser
  --execute
  -- non-existent docid
  edoc <- doc_update $ UpdateDocumentSimple a (emptySignatoryDetails, author) []  
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
  edoc <- doc_update $ AttachCSVUpload (documentid doc) (csvupload { csvsignatoryindex = ai })
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
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  --execute                     
  edoc <- doc_update $ AttachCSVUpload (documentid doc) csvupload
  --assert
  validTest $ assertLeft edoc

testPreparationAttachCSVUploadIndexGreaterThanLength :: DB ()
testPreparationAttachCSVUploadIndexGreaterThanLength = doTimes 10 $ do
  -- setup
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthorAndCondition author isPreparation
  if length (documentsignatorylinks doc) > 10
    then invalidateTest
    else do
    csvupload <- untilCondition (\c -> (csvsignatoryindex c) >= length (documentsignatorylinks doc)) 
                  $ rand 10 arbitrary
    --execute                     
    edoc <- doc_update $ AttachCSVUpload (documentid doc) csvupload
    --assert
    validTest $ assertLeft edoc

testCreateFromSharedTemplate :: DB ()
testCreateFromSharedTemplate = do
  user <- addNewRandomAdvancedUser
  docid <- addRandomDocumentWithAuthor user
  tmpdoc <- fmap fromJust $ doc_query $ GetDocumentByDocumentID docid
  doc <- if (isTemplate tmpdoc)
         then return tmpdoc
         else fmap fromRight $ doc_update (TemplateFromDocument docid)
  newuser <- addNewRandomAdvancedUser
  mt <- rand 10 arbitrary
  doc' <- fmap fromRight $ doc_update $ SignableFromDocumentIDWithUpdatedAuthor newuser Nothing (documentid doc) mt
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
  validTest $ assertRight edoc
          
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
  etdoc <- doc_update $ TimeoutDocument (documentid doc) mt
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
  edoc <- randomUpdate $ RejectDocument (documentid doc)
  validTest $ do
    assertRight edoc
    assertInvariants $ fromRight edoc

testMarkInvitationRead :: DB ()
testMarkInvitationRead = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocumentWithAuthor' author
  _edoc <- randomUpdate $ MarkInvitationRead (documentid doc)
  validTest $ assertSuccess
        
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
                assertRight etdoc)
        
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
  etdoc <- randomUpdate $ SetInvitationDeliveryStatus (documentid doc)
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
  edoc <- randomUpdate $ SetDocumentTags (documentid doc)
  validTest $ assertRight edoc

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
  etdoc <- msum [randomUpdate $ SignDocument (documentid doc) (signatorylinkid sl) (signatorymagichash sl),
                 randomUpdate $ CloseDocument (documentid doc)]
  validTest $ assertRight etdoc
    
testCloseDocumentSignableNotAwaitingAuthorNothing :: DB ()
testCloseDocumentSignableNotAwaitingAuthorNothing = doTimes 10 $ do
  author <- addNewRandomAdvancedUser
  doc <- addRandomDocument (randomDocumentAllowsDefault author)
         { randomDocumentAllowedTypes = documentSignableTypes
         , randomDocumentAllowedStatuses = [AwaitingAuthor, Pending]
         , randomDocumentCondition = (not . (all (isSignatory =>>^ hasSigned) . documentsignatorylinks))
         }

  etdoc <- msum [randomUpdate $ CloseDocument (documentid doc)]
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

propbitfieldDeriveConvertibleId :: [SignatoryRole] -> Bool
propbitfieldDeriveConvertibleId ss =
  let ss' = nub (sort ss)
  in ss' == convert (convert ss' :: SqlValue)
