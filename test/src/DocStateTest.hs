{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DocStateTest (docStateTests) where

import Test.HUnit (assert, assertFailure, Assertion, assertBool, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import StateHelper
import User.Password
import User.UserState
import Doc.DocState
import Doc.DocUtils
import MinutesTime
import Happstack.State
import Misc
import Payments.PaymentsState as Payments
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Mails.MailsUtil
import qualified AppLogger as Log

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
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

testThat :: String -> Assertion -> Test
testThat s a = testCase s (withTestState a)

testNewDocumentDependencies :: Assertion
testNewDocumentDependencies = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  -- execute
  doc <- update $ NewDocument author "Test New Document No Company" (Signable Contract) mt
  -- assert
  assertInvariants doc
  
testDocumentCanBeCreatedAndFetchedByID :: Assertion
testDocumentCanBeCreatedAndFetchedByID = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser

  doc <- update $ NewDocument author "Test New Document No Company" (Signable Contract) mt
      
  mdoc <- query $ GetDocumentByDocumentID (documentid doc)
  -- assert
  case mdoc of
    Just resdoc -> assert $ sameDocID doc resdoc
    Nothing -> assertFailure "Could not read in new document I just created."

testDocumentCanBeCreatedAndFetchedByAllDocs :: Assertion
testDocumentCanBeCreatedAndFetchedByAllDocs = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  -- execute
  doc <- update $ NewDocument author "Test New Document No Company" (Signable Contract) mt
  docs <- query $ GetDocuments Nothing
      
  -- assert
  case find (sameDocID doc) docs of
    Just _ -> assertSuccess
    Nothing -> assertFailure "Could not read in new document I just created."

testDocumentUpdateDoesNotChangeID :: Assertion
testDocumentUpdateDoesNotChangeID = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  doc <- assumingBasicContract mt author
  --execute
  let sd = signatoryDetailsFromUser author
  enewdoc <- update $ UpdateDocument mt (documentid doc) "Test Document" [] Nothing "" (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) [EmailIdentification] Nothing AdvancedFunctionality 
  --assert
  case enewdoc of
    Left msg -> assertFailure $ "Could not run UpdateDocument: " ++ msg
    Right newdoc -> assertEqual "document ids should be equal" (documentid doc) (documentid newdoc)

testDocumentUpdateCanChangeTitle :: Assertion
testDocumentUpdateCanChangeTitle = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  doc <- assumingBasicContract mt author
  --execute
  let sd = signatoryDetailsFromUser author
  enewdoc <- update $ UpdateDocument mt (documentid doc) "New Title" [] Nothing "" (sd, [SignatoryAuthor, SignatoryPartner], userid author, Nothing) [EmailIdentification] Nothing AdvancedFunctionality 
  --assert
  case enewdoc of
    Left msg -> assertFailure $ "Could not run UpdateDocument: " ++ msg
    Right newdoc -> assertEqual "document name should be different" (documenttitle newdoc) "New Title"
    
testDocumentAttachAlwaysRight :: Assertion
testDocumentAttachAlwaysRight = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  doc <- assumingBasicContract mt author
  --execute
  edoc <- update $ AttachFile (documentid doc) "some file" "some content"
  --assert
  case edoc of
    Left msg -> assertFailure $ "Could not run AttachFile: " ++ msg
    Right _newdoc -> assertSuccess

testNoDocumentAttachAlwaysLeft :: Assertion
testNoDocumentAttachAlwaysLeft = do
  -- setup
  --execute
  -- non-existent docid
  edoc <- update $ AttachFile (DocumentID 4) "some file" "some content"
  --assert
  case edoc of
    Left _msg     -> assertSuccess
    Right _newdoc -> assertFailure "Should not succeed if no document"

testDocumentAttachHasAttachment :: Assertion
testDocumentAttachHasAttachment = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  doc <- assumingBasicContract mt author
  let fname = "some file" :: BS.ByteString
      content  = "some content" :: BS.ByteString
  --execute
  edoc <- update $ AttachFile (documentid doc) fname content
  --assert
  case edoc of
    Left msg -> assertFailure $ "Could not run AttachFile: " ++ msg
    Right newdoc -> case find ((== fname) . filename) (documentfiles newdoc) of
      Just _ -> assertSuccess
      _ -> assertFailure "File does exist or wrong name"

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
testDocumentAttachSealedAlwaysRight = do
  -- setup
  mt <- whatTimeIsIt
  author <- assumingBasicUser
  doc <- assumingBasicContract mt author
  --execute
  edoc <- update $ AttachSealedFile (documentid doc) "some file" "some content"
  --assert
  case edoc of
    Left msg -> assertFailure $ "Could not run AttachFile: " ++ msg
    Right _newdoc -> assertSuccess

testNotPreparationUpdateDocumentAlwaysLeft :: Assertion
testNotPreparationUpdateDocumentAlwaysLeft = do
  -- setup
  mt <- whatTimeIsIt
  author <- addNewRandomUser
  do100Times' $ do
                 docid <- addRandomDocumentWithAuthor author
                 mdoc <- query $ GetDocumentByDocumentID docid
                 case mdoc of
                   Nothing -> return $ Just $ assertFailure "Could not stored document."
                   Just doc | isPreparation doc -> return Nothing
                   Just _doc -> do
                     --execute
                     edoc <- update $ UpdateDocument mt docid "" []  Nothing "" (emptySignatoryDetails, [], UserID 1, Nothing) [] Nothing BasicFunctionality
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
   doc' <- fmap fromRight $ update $ SignableFromDocumentIDWithUpdatedAuthor newuser (documentid doc)
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
testMarkInvitationRead = do
  doTimes 100 $ do
    mt <- whatTimeIsIt    
    author <- addNewRandomUser
    docid <- addRandomDocumentWithAuthor author
    mdoc <- query $ GetDocumentByDocumentID docid
    case mdoc of
      Nothing -> return $ Just $ assertFailure "Could not stored document."
      Just _doc -> do
        stdgn <- newStdGen
        let a = unGen arbitrary stdgn 10
        _ <- update $ MarkInvitationRead docid a mt
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

assertSuccess :: Assertion
assertSuccess = assertBool "not success?!" True

addNewUser :: String -> String -> String -> IO (Maybe User)
addNewUser firstname secondname email = 
  update $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) NoPassword False Nothing Nothing

whatTimeIsIt :: IO (MinutesTime)
whatTimeIsIt = liftIO $ getMinutesTime

assumingBasicContract :: MinutesTime -> User -> IO (Document)
assumingBasicContract mt author = do
  doc <- update $ NewDocument author "Test Document" (Signable Contract) mt
  mdoc <- query $ GetDocumentByDocumentID (documentid doc)
  case mdoc of
    Nothing -> do
      assertFailure "Could not create + store document."
      return blankDocument
    Just d -> return d

assumingBasicUser :: IO (User)
assumingBasicUser = do
  muser <- addNewUser "Eric" "Normand" "eric@fds.com"
  case muser of
    Just user -> return user
    Nothing -> do
      assertFailure "Cannot create a new user (in setup)"
      return blankUser
      
addNewRandomUser :: IO (User)
addNewRandomUser = do
  stdgn <- newStdGen
  let fn = unGen arbitrary stdgn 10
      ln = unGen arbitrary stdgn 10
      em = unGen arbEmail  stdgn 10
  muser <- addNewUser fn ln (BS.toString em)
  case muser of
    Just user -> return user
    Nothing -> do
      Log.debug "Could not create user, trying again."
      addNewRandomUser

blankUser :: User
blankUser = User {  
                   userid                  =  UserID 0
                 , userpassword            =  NoPassword
                 , usersupervisor          =  Nothing
                 , useriscompanyadmin = False
                 , useraccountsuspended    =  False  
                 , userhasacceptedtermsofservice = Nothing
                 , userfreetrialexpirationdate = Nothing
                 , usersignupmethod = AccountRequest
                 , userinfo = UserInfo {
                                    userfstname = BS.empty
                                  , usersndname = BS.empty
                                  , userpersonalnumber = BS.empty
                                  , usercompanyname =  BS.empty
                                  , usercompanyposition =  BS.empty
                                  , usercompanynumber  =  BS.empty
                                  , useraddress =  BS.empty
                                  , userzip = BS.empty
                                  , usercity  = BS.empty
                                  , usercountry = BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail =  Email BS.empty 
                                   }
                , usersettings  = UserSettings {
                                    accounttype = PrivateAccount
                                  , accountplan = Basic
                                  , signeddocstorage = Nothing
                                  , userpaymentmethod = Undefined
                                  , preferreddesignmode = Nothing
                                  , lang = Misc.defaultValue
                                  }                   
                , userpaymentpolicy = Payments.initialPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount
              , userfriends = []
              , userinviteinfo = Nothing
              , userlogininfo = LoginInfo
                                { lastsuccesstime = Nothing
                                , lastfailtime = Nothing
                                , consecutivefails = 0
                                }
              , userservice = Nothing
              , usercompany = Nothing
              , usermailapi = Nothing
              , userdeleted = False
              }

blankDocument :: Document 
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = AdvancedFunctionality
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Doc.DocState.Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          , documentattachments          = []
          }

emptySignatoryDetails :: SignatoryDetails
emptySignatoryDetails = SignatoryDetails
    { signatoryfstname        = ""
    , signatorysndname        = ""
    , signatorycompany        = ""
    , signatorypersonalnumber = ""
    , signatorycompanynumber  = ""
    , signatoryemail          = ""
    , signatorysignorder = SignOrder 1
    , signatoryfstnameplacements        = []
    , signatorysndnameplacements        = []
    , signatorycompanyplacements        = []
    , signatoryemailplacements          = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements  = []
    , signatoryotherfields              = []
    }
    
instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.fromString arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a'..'z']

arbEmail :: Gen BS.ByteString
arbEmail = do
  n <- arbString 1 7
  d <- arbString 3 7
  return $ BS.fromString (n ++ "@" ++ d ++ ".com")
    
instance Arbitrary SignatoryDetails where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    cn <- arbitrary
    pn <- arbitrary
    cm <- arbitrary
    em <- arbEmail
    ofields <- arbitrary
    return $ SignatoryDetails { signatoryfstname        = fn
                              , signatorysndname        = ln
                              , signatorycompany        = cn
                              , signatorypersonalnumber = pn
                              , signatorycompanynumber  = cm
                              , signatoryemail          = em
                              , signatorysignorder = SignOrder 1
                              , signatoryfstnameplacements        = []
                              , signatorysndnameplacements        = []
                              , signatorycompanyplacements        = []
                              , signatoryemailplacements          = []
                              , signatorypersonalnumberplacements = []
                              , signatorycompanynumberplacements  = []
                              , signatoryotherfields              = ofields
                              }

instance Arbitrary FieldDefinition where
   arbitrary = do
    name <- arbitrary
    value <- arbitrary
    filledByAuthor <- arbitrary
    return $ FieldDefinition { fieldlabel = name,
                               fieldvalue = value,
                               fieldplacements = [],
                               fieldfilledbyauthor = filledByAuthor
                             }
                      
instance Arbitrary UserInfo where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    cn <- arbitrary
    pn <- arbitrary
    cm <- arbitrary
    em <- arbEmail

    return $ UserInfo { userfstname     = fn
                      , usersndname     = ln
                      , userpersonalnumber  = pn
                      , usercompanyname     = cn
                      , usercompanyposition = ""
                      , usercompanynumber   = cm
                      , useraddress         = ""
                      , userzip             = ""
                      , usercity            = ""
                      , usercountry         = ""
                      , userphone           = ""
                      , usermobile          = ""
                      , useremail           = Email em
                      }

addRandomDocumentWithAuthor :: User -> IO (DocumentID)
addRandomDocumentWithAuthor user = do
  stdgen <- newStdGen
  let roles = unGen (elements [[SignatoryAuthor], [SignatoryAuthor, SignatoryPartner], [SignatoryPartner, SignatoryAuthor]])
              stdgen 10000
  let doc = unGen arbitrary stdgen 10
      sls = 1 + (abs $ unGen arbitrary stdgen 10)
      sldets = unGen (vectorOf sls arbitrary) stdgen 10
      slr = unGen (vectorOf sls $ elements [[], [SignatoryPartner]]) stdgen 10000
  slinks <- sequence $ zipWith (\a r -> update $ (SignLinkFromDetailsForTest a r)) sldets slr
  asd <- extendRandomness $ signatoryDetailsFromUser user
  asl <- update $ SignLinkFromDetailsForTest asd roles
  let adoc = doc { documentsignatorylinks = slinks ++ 
                                            [asl { maybesignatory = Just (userid user) }]
                 }
  update $ StoreDocumentForTesting adoc

instance Arbitrary DocumentID where
  arbitrary = do
    ds <- arbitrary
    return $ DocumentID ds

instance Arbitrary DocumentType where
  arbitrary = elements [ Signable Contract
                       , Signable Order
                       , Signable Offer
                       , Template Contract
                       , Template Order
                       , Template Offer
                       , Attachment
                       , AttachmentTemplate
                       ]
         

instance Arbitrary Document where
  arbitrary = do
    ds <- arbitrary
    dt <- arbitrary
    return $ blankDocument { documentstatus = ds 
                           , documenttype = dt
                           }

instance Arbitrary DocumentStatus where
  arbitrary = elements [ Preparation
                       , Pending
                       , Closed
                       , Canceled
                       , Timedout
                       , Rejected
                       , AwaitingAuthor
                       , DocumentError "Bad document."
                       ]

do100Times' :: IO (Maybe Assertion) -> IO ()
do100Times' action = doTimes 100 action
             
doTimes :: Int -> IO (Maybe Assertion) -> IO ()
doTimes i action 
  | i == 0 = return ()
  | otherwise = do
    res <- action
    case res of
      Nothing -> doTimes i action
      Just ass -> do
        _ <- ass
        doTimes (i - 1) action

instance Arbitrary CSVUpload where
  arbitrary = do
    a <- arbitrary
    cols <- arbitrary
    rows <- arbitrary
    b <- vectorOf rows (vectorOf cols arbitrary)
    c <- arbitrary
    return $ CSVUpload { csvtitle = a
                       , csvcontents = b
                       , csvsignatoryindex = c
                       }


{- | Sometimes we get and object that is not as random as we would expect (from some reason)
     Like author signatorylink that by default does not have any fields attached
     This is a class to make it more random - so to attach this fields for example.
-}
class ExtendWithRandomnes a where
    moreRandom :: a -> Gen a
    extendRandomness ::  a -> IO a
    extendRandomness a = do
          stdgen <- newStdGen
          return $ unGen (moreRandom a) stdgen 10

        
instance ExtendWithRandomnes SignatoryDetails where
    moreRandom sl = do
        ofields <- arbitrary
        return $ sl {signatoryotherfields = ofields}
    
instance Arbitrary SignatoryLinkID where
  arbitrary = do
    si <- arbitrary
    return $ SignatoryLinkID si

instance Arbitrary SignatureProvider where
  arbitrary = elements [ BankIDProvider
                       , TeliaProvider
                       , NordeaProvider
                       ]

instance Arbitrary SignatureInfo where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    g <- arbitrary
    return $ SignatureInfo { signatureinfotext        = a
                           , signatureinfosignature   = b
                           , signatureinfocertificate = c
                           , signatureinfoprovider    = d
                           , signaturefstnameverified = e
                           , signaturelstnameverified = f
                           , signaturepersnumverified = g
                           }

instance Arbitrary MagicHash where
  arbitrary = do
    a <- arbitrary
    return $ MagicHash a

instance Arbitrary MailsDeliveryStatus where
  arbitrary = elements [ Delivered 
                       , Undelivered
                       , Unknown
                       , Deferred]

instance Arbitrary TimeoutTime where
  arbitrary = do
    a <- arbitrary
    return $ TimeoutTime a

instance Arbitrary MinutesTime where
  arbitrary = do
    a <- arbitrary
    return $ fromSeconds a

instance Arbitrary DocumentTag where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ DocumentTag a b

instance Arbitrary DocumentUI where
  arbitrary = do
    a <- arbitrary
    return $ DocumentUI a
