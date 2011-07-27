{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import Test.HUnit (assert, assertFailure, Assertion,  assertEqual)
import Test.Framework

import User.UserState
import Doc.DocState
import Doc.DocUtils
import Misc
import Util.SignatoryLinkUtils
import Doc.DocInfo
import TestingUtil
import Company.CompanyState

import Happstack.State
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

docStateTests :: Test
docStateTests = testGroup "DocState" [
  testThat "SetDocumentTitle fails when doc doesn't exist" testSetDocumentTitleNotLeft,
  
  testThat "CloseDocument fails when doc is not signable" testCloseDocumentNotSignableNothing,
  testThat "CloseDocument fails when doc doesn't exist" testCloseDocumentNotNothing,
  testThat "CloseDocument succeeds when doc is signable" testCloseDocumentSignableJust,
  
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
testNewDocumentDependencies = doTimes 100 $ do
  -- setup
  author <- addNewRandomUser
  stdgn <- newStdGen
  let a = unGen arbitrary stdgn 100
      b = unGen arbitrary stdgn 10
      c = unGen arbitrary stdgn 100
      
  mcompany <- case usercompany author of  
    Nothing -> return Nothing
    Just cid -> query $ GetCompany cid

  -- execute
  edoc <- update $ NewDocument author mcompany a b c
  case edoc of 
    Left msg -> validTest $ assertFailure $ "Could not run NewDocument " ++ msg
    Right doc -> validTest $ assertInvariants doc
  
  
testDocumentCanBeCreatedAndFetchedByID :: Assertion
testDocumentCanBeCreatedAndFetchedByID = doTimes 100 $ do
  -- setup
  author <- addNewRandomUser
  stdgn <- newStdGen
  let a = unGen arbitrary stdgn 100
      b = unGen arbitrary stdgn 10
      c = unGen arbitrary stdgn 100
      
  mcompany <- case usercompany author of  
    Nothing -> return Nothing
    Just cid -> query $ GetCompany cid

  Right doc <- update $ NewDocument author mcompany a b c
  -- execute
  mdoc <- query $ GetDocumentByDocumentID (documentid doc)
  -- assert
  case mdoc of
    Just resdoc -> validTest $ assert $ sameDocID doc resdoc
    Nothing -> validTest $ assertFailure "Could not read in new document I just created."

testDocumentCanBeCreatedAndFetchedByAllDocs :: Assertion
testDocumentCanBeCreatedAndFetchedByAllDocs = doTimes 100 $ do
  -- setup
  author <- addNewRandomUser
  stdgn <- newStdGen
  let a = unGen arbitrary stdgn 100
      b = unGen arbitrary stdgn 10
      c = unGen arbitrary stdgn 100

  mcompany <- case usercompany author of  
    Nothing -> return Nothing
    Just cid -> query $ GetCompany cid

  -- execute
  Right doc <- update $ NewDocument author mcompany a b c
  docs <- query $ GetDocuments Nothing
  -- assert
  case find (sameDocID doc) docs of
    Just _ -> validTest assertSuccess
    Nothing -> validTest $ assertFailure "Could not read in new document I just created."

testDocumentUpdateDoesNotChangeID :: Assertion
testDocumentUpdateDoesNotChangeID = doTimes 10 $ do
  -- setup
  author <- addNewRandomUser
  stdgn <- newStdGen
  let mt = unGen arbitrary stdgn 100
      a  = unGen arbitrary stdgn 100
      b  = unGen arbitrary stdgn 100      
      c  = unGen arbitrary stdgn 100      
      d  = unGen arbitrary stdgn 100      
      e  = unGen arbitrary stdgn 100
      f  = unGen arbitrary stdgn 100      
      g  = unGen arbitrary stdgn 100      
  doc <-  addRandomDocumentWithAuthor' author
  if not $ isPreparation doc
    then invalidateTest
    else do
      --execute
      let sd = signatoryDetailsFromUser author Nothing
      enewdoc <- update $ UpdateDocument mt (documentid doc) a b c d (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) e f g
      --assert
      case enewdoc of
        Left msg -> validTest $ assertFailure $ "Could not run UpdateDocument: " ++ msg
        Right newdoc -> validTest $ assertEqual "document ids should be equal" (documentid doc) (documentid newdoc)

testDocumentUpdateCanChangeTitle :: Assertion
testDocumentUpdateCanChangeTitle = do
  -- setup
  mt <- whatTimeIsIt
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  --execute
  let sd = signatoryDetailsFromUser author Nothing
  if not $ isPreparation doc 
    then invalidateTest
    else do
      enewdoc <- update $ UpdateDocument mt (documentid doc) "New Title" [] Nothing "" (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) [EmailIdentification] Nothing AdvancedFunctionality 
      --assert
      case enewdoc of
        Left msg -> assertFailure $ "Could not run UpdateDocument: " ++ msg
        Right newdoc -> assertEqual "document name should be different" (documenttitle newdoc) "New Title"
    
testDocumentAttachAlwaysRight :: Assertion
testDocumentAttachAlwaysRight = do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  --execute
  edoc <- update $ AttachFile (documentid doc) "some file" "some content"
  --assert
  case edoc of
    Left msg -> assertFailure $ "Could not run AttachFile: " ++ msg
    Right _newdoc -> assertSuccess

testNoDocumentAttachAlwaysLeft :: Assertion
testNoDocumentAttachAlwaysLeft = doTimes 100 $ do
  -- setup
  stdgn <- newStdGen
  let a = unGen arbitrary stdgn 100
      b = unGen arbitrary stdgn 10
      c = unGen arbitrary stdgn 100
  --execute
  -- non-existent docid
  edoc <- update $ AttachFile a b c
  --assert
  case edoc of
    Left _msg     -> return $ Just $ assertSuccess
    Right _newdoc -> return $ Just $ assertFailure "Should not succeed if no document"

testDocumentAttachHasAttachment :: Assertion
testDocumentAttachHasAttachment = doTimes 100 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  stdgn <- newStdGen  
  let a = unGen arbitrary stdgn 10
      b = unGen arbitrary stdgn 100
  --execute
  edoc <- update $ AttachFile (documentid doc) a b
  --assert
  case edoc of
    Left msg -> return $ Just $ assertFailure $ "Could not run AttachFile: " ++ msg
    Right newdoc -> case find ((== a) . filename) (documentfiles newdoc) of
      Just _ -> return $ Just $ assertSuccess
      _ -> return $ Just $ assertFailure "File does not exist or wrong name"

testNoDocumentAttachSealedAlwaysLeft :: Assertion
testNoDocumentAttachSealedAlwaysLeft = do
  -- setup
  --execute
  -- non-existent docid
  edoc <- update $ AttachSealedFile (DocumentID 4) "some file" "some content"
  --assert
  case edoc of
    Left _msg     -> assertSuccess
    Right _newdoc -> assertFailure "Should not succeed if no document"

testDocumentAttachSealedAlwaysRight :: Assertion
testDocumentAttachSealedAlwaysRight = doTimes 100 $ do
  -- setup
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  --execute
  if not $ isSignable doc 
    then invalidateTest
    else do 
      edoc <- update $ AttachSealedFile (documentid doc) "some file" "some content"
      --assert
      case edoc of
        Left msg -> validTest $ assertFailure $ "Could not run AttachFile: " ++ msg
        Right _newdoc -> validTest $ assertSuccess

testNotPreparationUpdateDocumentAlwaysLeft :: Assertion
testNotPreparationUpdateDocumentAlwaysLeft = do
  -- setup
  do100Times' $ do
                 mt <- whatTimeIsIt
                 author <- addNewRandomUser
                 doc <- addRandomDocumentWithAuthor' author
                 if isPreparation doc
                   then return Nothing
                   else do
                     --execute
                     edoc <- update $ UpdateDocument mt (documentid doc) "" []  Nothing "" (emptySignatoryDetails, [], UserID 1, Nothing) [] Nothing BasicFunctionality
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should not succeed if not preparation"
  assertSuccess

testPreparationUpdateDocumentAlwaysRight :: Assertion
testPreparationUpdateDocumentAlwaysRight = do
  -- setup
  mt <- whatTimeIsIt
  author <- addNewRandomUser
  doTimes 10 $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | not $ isPreparation doc -> return Nothing
                   Just _doc -> do
                     --execute
                     edoc <- update $ UpdateDocument mt docid "" []  Nothing "" (emptySignatoryDetails, [], UserID 1, Nothing) [] Nothing BasicFunctionality
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ assertFailure "Should always succeed if not preparation"
                       Right _newdoc -> return $ Just $ return ()
  assertSuccess


testNoDocumentUpdateDocumentAlwaysLeft :: Assertion
testNoDocumentUpdateDocumentAlwaysLeft = do
  -- setup
  mt <- whatTimeIsIt
  --execute
  -- non-existent docid
  edoc <- update $ UpdateDocument mt (DocumentID 24) "" []  Nothing "" (emptySignatoryDetails, [], UserID 1, Nothing) [] Nothing BasicFunctionality
  --assert
  case edoc of
    Left _msg     -> assertSuccess
    Right _newdoc -> assertFailure "Should not succeed if no document"

testNotPreparationUpdateDocumentSimpleAlwaysLeft :: Assertion
testNotPreparationUpdateDocumentSimpleAlwaysLeft = do
  -- setup
  author <- addNewRandomUser
  do100Times' $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | isPreparation doc -> return Nothing
                   Just _doc -> do
                     --execute
                     edoc <- update $ UpdateDocumentSimple docid (emptySignatoryDetails, author) []
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should not succeed if not preparation"
  assertSuccess

testPreparationUpdateDocumentSimpleAlwaysRight :: Assertion
testPreparationUpdateDocumentSimpleAlwaysRight = do
  -- setup
  author <- addNewRandomUser
  doTimes 10 $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | not $ isPreparation doc -> return Nothing
                   Just _doc -> do
                     --execute
                     edoc <- update $ UpdateDocumentSimple docid (emptySignatoryDetails, author) []
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ assertFailure "Should always succeed if not preparation"
                       Right _newdoc -> return $ Just $ return ()
  assertSuccess


testNoDocumentUpdateDocumentSimpleAlwaysLeft :: Assertion
testNoDocumentUpdateDocumentSimpleAlwaysLeft = do
  -- setup
  author <- addNewRandomUser
  --execute
  -- non-existent docid
  edoc <- update $ UpdateDocumentSimple (DocumentID 24) (emptySignatoryDetails, author) []  
  --assert
  case edoc of
    Left _msg     -> assertSuccess
    Right _newdoc -> assertFailure "Should not succeed if no document"

-- AttachCSVUpload 
    
testNoDocumentAttachCSVUploadAlwaysLeft :: Assertion
testNoDocumentAttachCSVUploadAlwaysLeft = do
  -- setup
  --execute
  -- non-existent docid
  stdgn <- newStdGen
  let csvupload = unGen arbitrary stdgn 10
  edoc <- update $ AttachCSVUpload (DocumentID 24) csvupload
  --assert
  case edoc of
    Left _msg     -> assertSuccess
    Right _newdoc -> assertFailure "Should not succeed if no document"

testNotPreparationAttachCSVUploadAlwaysLeft :: Assertion
testNotPreparationAttachCSVUploadAlwaysLeft = do
  -- setup
  author <- addNewRandomUser
  do100Times' $ do
                 docid <- addRandomDocumentWithAuthor author
                 stdgn <- newStdGen
                 let csvupload = unGen arbitrary stdgn 10
                 mdoc <- query $ GetDocumentByDocumentID docid
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | isPreparation doc -> return Nothing
                   Just _doc -> do
                     --execute
                     edoc <- update $ AttachCSVUpload docid csvupload
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should not succeed if not preparation"
  assertSuccess

testPreparationAttachCSVUploadAuthorIndexLeft :: Assertion
testPreparationAttachCSVUploadAuthorIndexLeft = do
  -- setup
  author <- addNewRandomUser
  doTimes 10 $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 stdgn <- newStdGen
                 let csvupload = unGen arbitrary stdgn 10
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | not $ isPreparation doc -> return Nothing
                   Just doc -> do
                     let Just ai = authorIndex (documentsignatorylinks doc)
                     --execute                     
                     edoc <- update $ AttachCSVUpload docid (csvupload { csvsignatoryindex = ai })
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should fail if author is csvsignatoryindex"
  assertSuccess

authorIndex :: [SignatoryLink] -> Maybe Int
authorIndex sls = case catMaybes $ zipWith (\sl i -> if isAuthor sl then Just i else Nothing) sls [0..] of
  [] -> Nothing
  x:_ -> Just x

testPreparationAttachCSVUploadIndexNeg :: Assertion
testPreparationAttachCSVUploadIndexNeg = do
  -- setup
  author <- addNewRandomUser
  doTimes 10 $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 stdgn <- newStdGen
                 let csvupload = unGen arbitrary stdgn 10
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | not $ isPreparation doc -> return Nothing
                   Just _   | (csvsignatoryindex csvupload) >= 0 -> return Nothing
                   Just _doc -> do
                     --execute                     
                     edoc <- update $ AttachCSVUpload docid csvupload
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should fail if csvsignatoryindex is negative"
  assertSuccess


testPreparationAttachCSVUploadIndexGreaterThanLength :: Assertion
testPreparationAttachCSVUploadIndexGreaterThanLength = do
  -- setup
  author <- addNewRandomUser
  doTimes 10 $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 stdgn <- newStdGen
                 let csvupload = unGen arbitrary stdgn 10
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | not $ isPreparation doc -> return Nothing
                   Just doc | (csvsignatoryindex csvupload) < length (documentsignatorylinks doc) -> return Nothing
                   Just _doc -> do
                     --execute                     
                     edoc <- update $ AttachCSVUpload docid csvupload
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ return ()
                       Right _newdoc -> return $ Just $ assertFailure "Should fail if csvsignatoryindex is too high"
  assertSuccess

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
testUpdateDocumentAttachmentFailsIfNotPreparation =
  doTimes 100 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isPreparation doc -> return Nothing
      Just _doc -> do
        --execute                     
        edoc <- update $ UpdateDocumentAttachments docid [] []
        --assert
        case edoc of
          Left _msg     -> return $ Just $ return ()
          Right _newdoc -> return $ Just $ assertFailure "Should fail if document is not in preparation"

testUpdateDocumentAttachmentFailsNotFound :: Assertion
testUpdateDocumentAttachmentFailsNotFound = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isPreparation doc -> return Nothing
      Just _doc -> do
        --execute                     
        edoc <- update $ UpdateDocumentAttachments docid [DocumentID 1] []
        --assert
        case edoc of
          Left _msg     -> return $ Just $ return ()
          Right _newdoc -> return $ Just $ assertFailure "Should fail if documentid is not found"
  
testUpdateDocumentAttachmentOk :: Assertion
testUpdateDocumentAttachmentOk = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isPreparation doc -> return Nothing
      Just _doc -> do
        --execute                     
        edoc <- update $ UpdateDocumentAttachments docid [] []
        --assert
        case edoc of
          Left _msg     -> return $ Just $ assertFailure "Should not fail if empty attachments."
          Right _newdoc -> return $ Just $ return ()
          
          
testDocumentFromSignatoryDataFailsDoesntExist :: Assertion
testDocumentFromSignatoryDataFailsDoesntExist = do
  doTimes 100 $ do
    stdgn <- newStdGen    
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
        c = unGen arbitrary stdgn 10
        d = unGen arbitrary stdgn 10
        e = unGen arbitrary stdgn 10
        f = unGen arbitrary stdgn 10
        g = unGen arbitrary stdgn 10
        h = unGen arbitrary stdgn 10
        count = unGen arbitrary stdgn 10
        i = unGen (vectorOf count arbitrary) stdgn 10
    mdoc <- update $ DocumentFromSignatoryData a b c d e f g h i
    case mdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if document does not exist."
      
        
testDocumentFromSignatoryDataSucceedsExists :: Assertion
testDocumentFromSignatoryDataSucceedsExists = do
  doTimes 100 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    stdgn <- newStdGen    
    let b = unGen arbitrary stdgn 10
        c = unGen arbitrary stdgn 10
        d = unGen arbitrary stdgn 10
        e = unGen arbitrary stdgn 10
        f = unGen arbitrary stdgn 10
        g = unGen arbitrary stdgn 10
        h = unGen arbitrary stdgn 10
        count = unGen arbitrary stdgn 10
        i = unGen (vectorOf count arbitrary) stdgn 10
    mdoc <- update $ DocumentFromSignatoryData docid b c d e f g h i
    case mdoc of
      Left _ -> return $ Just $ assertFailure "Should succeed if document exists."
      Right _ -> return $ Just $ return ()
          
testTimeoutDocumentNonSignableLeft :: Assertion
testTimeoutDocumentNonSignableLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        etdoc <- update $ TimeoutDocument docid mt
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not signable"
    
testTimeoutDocumentSignableNotPendingLeft :: Assertion
testTimeoutDocumentSignableNotPendingLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isPending doc -> return Nothing
      Just _doc -> do
        etdoc <- update $ TimeoutDocument docid mt
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not pending"
    
testTimeoutDocumentSignablePendingRight :: Assertion
testTimeoutDocumentSignablePendingRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not $ isPending doc -> return Nothing
      Just _doc -> do
        etdoc <- update $ TimeoutDocument docid mt
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testTimeoutDocumentSignableNotLeft :: Assertion
testTimeoutDocumentSignableNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    etdoc <- update $ TimeoutDocument did mt
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."


testSignDocumentNonSignableLeft :: Assertion
testSignDocumentNonSignableLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
            d = unGen arbitrary stdgn 10
        etdoc <- update $ SignDocument docid a mt b c d
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not signable"

    
testSignDocumentSignableNotPendingLeft :: Assertion
testSignDocumentSignableNotPendingLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isPending doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
            d = unGen arbitrary stdgn 10
        etdoc <- update $ SignDocument docid a mt b c d
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not pending"
    
testSignDocumentSignablePendingRight :: Assertion
testSignDocumentSignablePendingRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not $ isPending doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
            d = unGen arbitrary stdgn 10
        etdoc <- update $ SignDocument docid a mt b c d
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testSignDocumentNotLeft :: Assertion
testSignDocumentNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
        c = unGen arbitrary stdgn 10
        d = unGen arbitrary stdgn 10
    etdoc <- update $ SignDocument did a mt b c d
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."

testAuthorSendDocumentNotSignableLeft :: Assertion
testAuthorSendDocumentNotSignableLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSendDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not signable"
  
testAuthorSendDocumentSignableNotPreparationLeft :: Assertion
testAuthorSendDocumentSignableNotPreparationLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isPreparation doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSendDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not pending"

testAuthorSendDocumentNotLeft :: Assertion
testAuthorSendDocumentNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
    etdoc <- update $ AuthorSendDocument did mt a b
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."
  
testAuthorSendDocumentSignablePreparationRight :: Assertion
testAuthorSendDocumentSignablePreparationRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not $ isPreparation doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSendDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testAuthorSignDocumentNotSignableLeft :: Assertion
testAuthorSignDocumentNotSignableLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSignDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not signable"
  
testAuthorSignDocumentSignableNotPreparationLeft :: Assertion
testAuthorSignDocumentSignableNotPreparationLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isPreparation doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSignDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not pending"

testAuthorSignDocumentNotLeft :: Assertion
testAuthorSignDocumentNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
    etdoc <- update $ AuthorSignDocument did mt a b
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."
  
testAuthorSignDocumentSignablePreparationRight :: Assertion
testAuthorSignDocumentSignablePreparationRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not $ isPreparation doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ AuthorSignDocument docid mt a b
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testRejectDocumentNotSignableLeft :: Assertion
testRejectDocumentNotSignableLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
        etdoc <- update $ RejectDocument docid a mt b c
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not signable"
  
testRejectDocumentSignableNotPendingLeft :: Assertion
testRejectDocumentSignableNotPendingLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isPending doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
        etdoc <- update $ RejectDocument docid a mt b c
        case etdoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Should fail if not pending"

testRejectDocumentNotLeft :: Assertion
testRejectDocumentNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
        c = unGen arbitrary stdgn 10
    etdoc <- update $ RejectDocument did a mt b c
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."
  
testRejectDocumentSignablePendingRight :: Assertion
testRejectDocumentSignablePendingRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not $ isPending doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
            c = unGen arbitrary stdgn 10
        etdoc <- update $ RejectDocument docid a mt b c
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testMarkInvitationRead :: Assertion
testMarkInvitationRead = doTimes 100 $ do
  mt <- whatTimeIsIt    
  author <- addNewRandomUser
  doc <- addRandomDocumentWithAuthor' author
  stdgn <- newStdGen
  let a = unGen arbitrary stdgn 10
  _ <- update $ MarkInvitationRead (documentid doc) a mt
  return $ Just $ return ()
        
testMarkInvitationReadDocDoesntExist :: Assertion
testMarkInvitationReadDocDoesntExist = do
  doTimes 100 $ do
    mt <- whatTimeIsIt        
    stdgn <- newStdGen
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
    _ <- update $ MarkInvitationRead a b mt
    return $ Just $ return ()
    
testMarkDocumentSeenNotSignableLeft :: Assertion
testMarkDocumentSeenNotSignableLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just doc -> do
        return $ Just <$> msum $ for (documentsignatorylinks doc) $ \sl ->
          if isNothing $ maybeseeninfo sl 
          then do
            stdgn <- newStdGen
            let a = unGen arbitrary stdgn 10
            etdoc <- update $ MarkDocumentSeen docid (signatorylinkid sl) (signatorymagichash sl) mt a
            case etdoc of
              Left _ -> return ()
              Right _ -> assertFailure "Should fail if document is not signable"
          else return ()
  
testMarkDocumentSeenClosedOrPreparationLeft :: Assertion
testMarkDocumentSeenClosedOrPreparationLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | not (isClosed doc || isPreparation doc) -> return Nothing
      Just doc -> do
        return $ Just <$> msum $ for (documentsignatorylinks doc) $ \sl ->
          if isNothing $ maybeseeninfo sl 
          then do
            stdgn <- newStdGen
            let a = unGen arbitrary stdgn 10
            etdoc <- update $ MarkDocumentSeen docid (signatorylinkid sl) (signatorymagichash sl) mt a
            case etdoc of
              Left _ -> return ()
              Right _ -> assertFailure "Should succeed if document is signable and closed or preparation"
          else return ()

testMarkDocumentSeenNotLeft :: Assertion
testMarkDocumentSeenNotLeft = do
  doTimes 100 $ do
    mt <- whatTimeIsIt
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
        c = unGen arbitrary stdgn 10
    etdoc <- update $ MarkDocumentSeen did a b mt c
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."
  
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight :: Assertion
testMarkDocumentSeenSignableSignatoryLinkIDAndMagicHashAndNoSeenInfoRight = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isClosed doc || isPreparation doc -> return Nothing      
      Just doc -> do
        return $ Just <$> msum $ for (documentsignatorylinks doc) $ \sl ->
          if isNothing $ maybeseeninfo sl 
          then do
            stdgn <- newStdGen
            let a = unGen arbitrary stdgn 10
            etdoc <- update $ MarkDocumentSeen docid (signatorylinkid sl) (signatorymagichash sl) mt a
            case etdoc of
              Left _ -> assertFailure "Should succeed if document exists, is Signable, and is Pending"
              Right _ -> return ()
          else return ()
        
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft :: Assertion
testMarkDocumentSeenSignableSignatoryLinkIDBadMagicHashLeft = do
  doTimes 10 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just doc | isClosed doc || isPreparation doc -> return Nothing      
      Just doc -> do
        return $ Just <$> msum $ for (documentsignatorylinks doc) $ \sl ->
          if isNothing $ maybeseeninfo sl 
          then do
            stdgn <- newStdGen
            let a = unGen arbitrary stdgn 10
                b = unGen arbitrary stdgn 10
            etdoc <- update $ MarkDocumentSeen docid (signatorylinkid sl) b mt a
            case etdoc of
              Left _ -> return ()
              Right _ -> assertFailure "Should fail if the magich hash is incorrect"
          else return ()

testSetInvitationDeliveryStatusNotSignableLeft :: Assertion
testSetInvitationDeliveryStatusNotSignableLeft = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _ -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        edoc <- update $ SetInvitationDeliveryStatus docid a b
        case edoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Not signable should fail"
        
  
testSetInvitationDeliveryStatusNotLeft :: Assertion
testSetInvitationDeliveryStatusNotLeft = do
  doTimes 100 $ do
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
    etdoc <- update $ SetInvitationDeliveryStatus did a b
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."
  

testSetInvitationDeliveryStatusSignableRight :: Assertion
testSetInvitationDeliveryStatusSignableRight = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ SetInvitationDeliveryStatus docid a b
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()
  
testSetDocumentTimeoutTimeNotSignableLeft :: Assertion
testSetDocumentTimeoutTimeNotSignableLeft = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _ -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
        edoc <- update $ SetDocumentTimeoutTime docid a
        case edoc of
          Left _ -> return $ Just $ return ()
          Right _ -> return $ Just $ assertFailure "Not signable should fail"
        
testSetDocumentTimeoutTimeSignableRight :: Assertion
testSetDocumentTimeoutTimeSignableRight = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
        etdoc <- update $ SetDocumentTimeoutTime docid a
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testSetDocumentTimeoutTimeNotLeft :: Assertion
testSetDocumentTimeoutTimeNotLeft = do
  doTimes 100 $ do
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
    etdoc <- update $ SetDocumentTimeoutTime did a
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."

testSetDocumentTagsNotLeft :: Assertion
testSetDocumentTagsNotLeft = do
  doTimes 100 $ do
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
    etdoc <- update $ SetDocumentTags did a
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."

testSetDocumentTagsRight :: Assertion
testSetDocumentTagsRight = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
        etdoc <- update $ SetDocumentTags docid a
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()

testSetDocumentUINotLeft :: Assertion
testSetDocumentUINotLeft = do
  doTimes 100 $ do
    stdgn <- newStdGen    
    let did = unGen arbitrary stdgn 1000
    let a = unGen arbitrary stdgn 10
    etdoc <- update $ SetDocumentUI did a
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if it doesn't exist."

testSetDocumentUIRight :: Assertion
testSetDocumentUIRight = do
  doTimes 10 $ do
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
        etdoc <- update $ SetDocumentUI docid a
        case etdoc of
          Left _ -> return $ Just $ assertFailure "Should succeed if document exists, is Signable, and is Pending"
          Right _ -> return $ Just $ return ()
          
testCloseDocumentSignableJust :: Assertion
testCloseDocumentSignableJust = do
  doTimes 10 $ do
    mt <- whatTimeIsIt
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | not $ isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ CloseDocument docid mt a b
        case etdoc of
          Nothing -> return $ Just $ assertFailure "Should succeed if signable"
          Just _ -> return $ Just $ return ()
    
testCloseDocumentNotSignableNothing :: Assertion
testCloseDocumentNotSignableNothing = do
  doTimes 10 $ do
    mt <- whatTimeIsIt
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just doc | isSignable doc -> return Nothing
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
            b = unGen arbitrary stdgn 10
        etdoc <- update $ CloseDocument docid mt a b
        case etdoc of
          Nothing -> return $ Just $ return ()
          Just _ -> return $ Just $ assertFailure "Should fail if not signable"
    
testCloseDocumentNotNothing :: Assertion
testCloseDocumentNotNothing = do
  doTimes 10 $ do
    stdgn <- newStdGen
    let a = unGen arbitrary stdgn 10
        b = unGen arbitrary stdgn 10
        docid = unGen arbitrary stdgn 10
        mt = unGen arbitrary stdgn 10
    etdoc <- update $ CloseDocument docid mt a b
    case etdoc of
      Nothing -> return $ Just $ return ()
      Just _ -> return $ Just $ assertFailure "Should fail if not existing"

testSetDocumentTitleNotLeft :: Assertion
testSetDocumentTitleNotLeft = do
  doTimes 10 $ do
    stdgn <- newStdGen
    let a = unGen arbitrary stdgn 10
        docid = unGen arbitrary stdgn 10
    etdoc <- update $ SetDocumentTitle docid a
    case etdoc of
      Left _ -> return $ Just $ return ()
      Right _ -> return $ Just $ assertFailure "Should fail if not existing"

apply :: a -> (a -> b) -> b
apply a f = f a

assertInvariants :: Document -> Assertion
assertInvariants document =
  case catMaybes $ map (apply document) documentInvariants of
    [] -> assertSuccess
    a  -> assertFailure $ (show $ documentid document) ++ ": " ++ intercalate ";" a

documentInvariants :: [Document -> Maybe String]
documentInvariants = [
  documentHasOneAuthor
                     ]

{- |
   Test the invariant that a document must have exactly one author.
-}
documentHasOneAuthor :: Document -> Maybe String
documentHasOneAuthor document =
  case filter isAuthor $ documentsignatorylinks document of
    [_] -> Nothing
    a -> Just $ "document must have one author (has " ++ show (length a) ++ ")"

