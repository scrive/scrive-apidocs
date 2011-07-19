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
import Doc.DocStateQuery
import Util.SignatoryLinkUtils
import Doc.DocInfo
import qualified AppLogger as Log

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Maybe
import Control.Monad.Trans
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

docStateTests :: Test
docStateTests = testGroup "DocState" [
  testThat "A supervisor's friends can view a document"testSupervisorsFriendsCanSee,
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
  testThat "updateDocumentAttachment doesn't fail if there's no attachments" testUpdateDocumentAttachmentOk
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
  enewdoc <- update $ UpdateDocument mt (documentid doc) "Test Document" [] Nothing "" (sd, [SignatoryAuthor, SignatoryPartner], (userid author, Nothing)) [EmailIdentification] Nothing AdvancedFunctionality 
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
  enewdoc <- update $ UpdateDocument mt (documentid doc) "New Title" [] Nothing "" (sd, [SignatoryAuthor, SignatoryPartner], (userid author, Nothing)) [EmailIdentification] Nothing AdvancedFunctionality 
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
                     edoc <- update $ UpdateDocument mt docid "" []  Nothing "" (emptySignatoryDetails, [], (UserID 1, Nothing)) [] Nothing BasicFunctionality
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
                     edoc <- update $ UpdateDocument mt docid "" []  Nothing "" (emptySignatoryDetails, [], (UserID 1, Nothing)) [] Nothing BasicFunctionality
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
  edoc <- update $ UpdateDocument mt (DocumentID 24) "" []  Nothing "" (emptySignatoryDetails, [], (UserID 1, Nothing)) [] Nothing BasicFunctionality
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
                     edoc <- update $ UpdateDocumentSimple docid (emptySignatoryDetails, (UserID 1, Nothing)) []
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
                     edoc <- update $ UpdateDocumentSimple docid (emptySignatoryDetails, (UserID 1, Nothing)) []
                     --assert
                     case edoc of
                       Left _msg     -> return $ Just $ assertFailure "Should always succeed if not preparation"
                       Right _newdoc -> return $ Just $ return ()
  assertSuccess


testNoDocumentUpdateDocumentSimpleAlwaysLeft :: Assertion
testNoDocumentUpdateDocumentSimpleAlwaysLeft = do
  -- setup
  --execute
  -- non-existent docid
  edoc <- update $ UpdateDocumentSimple (DocumentID 24) (emptySignatoryDetails, (UserID 1, Nothing)) []  
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
  
testSupervisorsFriendsCanSee :: Assertion
testSupervisorsFriendsCanSee = do
  friend <- assumingNewUser "Friend" "Amigo" "abc@friend.com"
  super <- assumingNewUser "Super" "Visor" "super@visor.com"
  author <- assumingNewUserWithSupervisor (userid super) "Author" "Pendragon" "author@contract.com"
  _ <- update $ AddViewerByEmail (userid super) (Email "abc@friend.com")
  
  docid <- addRandomDocumentWithAuthor author
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> assertFailure "Could not stored document."
    Just doc -> do
      canView <- canUserViewDoc friend doc
      if canView then assertSuccess else assertFailure "Supervisor's friends cannot view document"
                  

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

addNewUserWithSupervisor :: UserID -> String -> String -> String -> IO (Maybe User)
addNewUserWithSupervisor superid = addNewUser' (Just superid)

addNewUser :: String -> String -> String -> IO (Maybe User)
addNewUser = addNewUser' Nothing

addNewUser' :: Maybe UserID -> String -> String -> String -> IO (Maybe User)
addNewUser' msuperid firstname secondname email = 
  update $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) NoPassword msuperid Nothing Nothing

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

assumingNewUser :: String -> String -> String -> IO User
assumingNewUser fn ln em = do
  muser <- addNewUser fn ln em
  case muser of
    Just user -> return user
    Nothing -> do
      assertFailure "Cannot create a new user (in setup)"
      return blankUser

assumingNewUserWithSupervisor :: UserID -> String -> String -> String -> IO User
assumingNewUserWithSupervisor sid fn ln em = do
  muser <- addNewUserWithSupervisor sid fn ln em
  case muser of
    Just user -> return user
    Nothing -> do
      assertFailure "Cannot create a new user (in setup)"
      return blankUser
  

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
              , userrecordstatus = LiveUser
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
          , documentoriginalcompany      = Nothing
          , documentrecordstatus         = LiveDocument
          , documentquarantineexpiry     = Nothing
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



instance Arbitrary Document where
  arbitrary = do
    ds <- arbitrary
    return $ blankDocument { documentstatus = ds }

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
    
