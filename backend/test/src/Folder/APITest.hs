{-# LANGUAGE OverloadedStrings #-}
module Folder.APITest (folderApiTests) where

import Data.Aeson hiding ((<?>))
import Data.Aeson.Types hiding ((<?>))
import Data.Unjson (field, fieldBy, fieldOptBy, objectOf)
import Happstack.Server
import Test.Framework
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AE
import qualified Data.Text as T
import qualified Data.Unjson as Unjson

import AccessControl.Model
import AccessControl.Types
import Context
import DB
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Mock.TestUtils
import Doc.DocStateData
import Doc.DocumentMonad (withDocument)
import Doc.Model
import Folder.API
import Folder.JSON
import Folder.Model
import MinutesTime
import TestingUtil
import TestKontra as T
import User.Model
import qualified Folder.Internal

folderApiTests :: TestEnvSt -> Test
folderApiTests env = testGroup
  "FolderAPI"
  [ testThat "Test creating groups works in API "              env testFolderAPICreate
  , testThat "Test updating groups works in API"               env testFolderAPIUpdate
  , testThat "Test reading groups works in API"                env testFolderAPIGet
  , testThat "Test folder deletion endpoint works in API"      env testFolderAPIDelete
  , testThat "Test listing documents in a folder works in API" env testFolderAPIListDocs
  , testThat "Test deleting empty non-home folder which user has permissons upon works"
             env
             testDeleteFolder
  , testThat
    "Test deleting non-home folder (which user has permissons upon) with children fails"
    env
    testCannotDeleteFolderWithChild
  , testThat
    "Test deleting non-home folder (which user has permissons upon) with documents fails"
    env
    testCannotDeleteFolderWithDocument
  , testThat
    "Test deleting non-home folder (which user has permissons upon) with only trashed documents fails"
    env
    testCannotDeleteFolderWithOnlyTrashedDocument
  , testThat
    "Test deleting non-home folder (which user has permissons upon) with only purged documents works"
    env
    testCanDeleteFolderWithOnlyPurgedDocument
  , testThat "Test deleting own home folder (which user has permissons upon) fails"
             env
             testCannotDeleteHomeFolder
  ]

-- API tests
testFolderAPICreate :: TestEnv ()
testFolderAPICreate = do
  user      <- instantiateRandomUser
  ctx       <- mkContextWithUser defaultLang user
  -- make root of folder struct
  fdrRootID <- view #id <$> do
    dbUpdate . FolderCreate $ defaultFolder

  folderNameTest <- rand 10 arbitraryName
  let fdrTest = set #name folderNameTest . set #parentID (Just fdrRootID) $ defaultFolder
      newFolderJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren fdrTest
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  -- should not be able to create a child since the user is not an admin of the
  -- above folder root
  void $ jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 403

  -- make admin of folder
  let folderAdminR = FolderAdminAR fdrRootID
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id) folderAdminR
  -- should now be able to create a child
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

  -- check that API returns what it's supposed to
  Just newFdrFromDB <- dbQuery . FolderGet $ newFdrFromAPI ^. #id
  assertEqual "New folder from API equals the one we sent in"
              newFdrFromAPI
              (set #id (newFdrFromAPI ^. #id) fdrTest)
              -- since id is 0 for default

  assertEqual "New folder from API equals corresponding one in DB"
              newFdrFromAPI
              newFdrFromDB

testFolderAPIUpdate :: TestEnv ()
testFolderAPIUpdate = do
  grpAdmin <- instantiateRandomUser
  user     <- instantiateRandomUser
  ctxAdmin <- mkContextWithUser defaultLang grpAdmin
  ctxUser  <- mkContextWithUser defaultLang user
  fdrRoot  <- dbUpdate $ FolderCreate (set #name "Folder root" defaultFolder)
  let folderAdminRoot = FolderAdminAR (fdrRoot ^. #id)
      admid           = grpAdmin ^. #id
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminRoot
  subs@[fdr1        , fdr2         ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     childrenFdr1, _childrenFdr2] <- forM subs $ createChildrenForParentByAPI ctxAdmin

  let folderAdminUserFdr1 = FolderAdminAR (fdr1 ^. #id)
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id) folderAdminUserFdr1

  -- `user` should be able to update fdr1-rooted tree
  forM_ childrenFdr1 (updateTestHelper ctxUser)

  fdr <-
    dbUpdate
    . FolderCreate
    $ (set #parentID (Just $ fdr1 ^. #id) . set #name "Folder for move test")
        defaultFolder
  allChildrenFdr1 <- concatMap fwcToList
    <$> dbQuery (FolderGetAllChildrenRecursive $ fdr1 ^. #id)
  assertEqual "Children of folder are the same"
              (sortByFolderID allChildrenFdr1)
              (sortByFolderID $ [fdr] <> childrenFdr1)

  let fdrTryMoving = set #parentID (Just $ fdr2 ^. #id) fdr
  -- `user` should _not_ be able to change parent of fdrTryMoving to fdr2 before being
  -- an admin of both parents
  void $ fdrAPIUpdate ctxUser fdrTryMoving 403

  -- make `user` admin of `fdr2`
  void . dbUpdate $ AccessControlInsertRoleForUser (user ^. #id)
                                                   (FolderAdminAR (fdr2 ^. #id))

  -- move should now be successful
  movedFdr1'        <- jsonToFolder <$> fdrAPIUpdate ctxUser fdrTryMoving 200

  Just moveddbFdr1' <- dbQuery . FolderGet $ fdrTryMoving ^. #id
  assertEqual "Moved folder from API should equal the corresponding one in DB"
              moveddbFdr1'
              movedFdr1'
  assertEqual "Moved folder from API should equal the one specified"
              moveddbFdr1'
              fdrTryMoving

  fromDBchildrenFdr1' <- do
    dbQuery . FolderGetImmediateChildren $ fdr1 ^. #id
  assertEqual "Children of folder are the same"
              (sortByFolderID fromDBchildrenFdr1')
              (sortByFolderID childrenFdr1)

  parentsOfFdr <- dbQuery . FolderGetParents $ fdr ^. #id
  assertEqual "Parent path of moved folder is correct" parentsOfFdr [fdr2, fdrRoot]
  where
    sortByFolderID :: [Folder] -> [Folder]
    sortByFolderID = sortBy (\f1 f2 -> compare (f1 ^. #id) (f2 ^. #id))

    fdrUpdateParams :: Folder -> [(Text, Input)]
    fdrUpdateParams fdr =
      let fdrwc   = folderToFolderWithChildren fdr
          fdrJSON = AE.encodingToLazyByteString $ encodeFolderWithChildren fdrwc
      in  [("folder", inTextBS fdrJSON)]

    fdrAPIUpdate :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIUpdate ctx fdr code = do
      let apiCall = folderAPIUpdate $ fdr ^. #id
      jsonTestRequestHelper ctx POST (fdrUpdateParams fdr) apiCall code

    updateTestHelper :: Context -> Folder -> TestEnv ()
    updateTestHelper ctx fdr = do
      name <- rand 10 arbitraryName
      let fdr' = set #name name fdr
      updatedFdr <- jsonToFolder <$> fdrAPIUpdate ctx fdr' 200
      assertEqual "New folder from API should equal the one specified" fdr' updatedFdr
      Just dbFdr <- dbQuery . FolderGet $ fdr ^. #id
      assertEqual "New folder from API should equal the corresponding one in DB"
                  dbFdr
                  updatedFdr
      -- reset for easier testing in calling body
      void $ jsonToFolder <$> fdrAPIUpdate ctx fdr 200

testFolderAPIGet :: TestEnv ()
testFolderAPIGet = do
  grpAdmin <- instantiateRandomUser
  ctxAdmin <- mkContextWithUser defaultLang grpAdmin
  fdrRoot  <- dbUpdate $ FolderCreate (set #name "Folder root" defaultFolder)
  let fdrRootID    = fdrRoot ^. #id
      folderAdminR = FolderAdminAR fdrRootID
      admid        = grpAdmin ^. #id
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminR
  subs@[_fdr1          , _fdr2          ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     _childrenOfFDR1, _childrenOfFDR2] <- do
    forM subs $ createChildrenForParentByAPI ctxAdmin
  fdrwcfuFromAPI <- fdrAPIGet ctxAdmin fdrRoot 200
  fdrChildren    <- dbQuery $ FolderGetImmediateChildren (fdrRoot ^. #id)
  let fdrwc =
        FolderWithChildren fdrRoot $ map (\c -> FolderWithChildren c []) fdrChildren
      fdrwcbs =
        fromJust . Aeson.decode . AE.encodingToLazyByteString $ encodeFolderWithChildren
          fdrwc
  assertEqual "Folder data output corresponds to what was sent in" fdrwcfuFromAPI fdrwcbs
  -- signatories should have access to folders for documents they participate in siging in
  mockDoc <- testDocApiV2New' ctxAdmin
  let signatoryEmail = "jakub.janczak@scrive.com" :: String
  signatoryUser <- instantiateUser $ randomUserTemplate
    { firstName = return "Jakub"
    , lastName  = return "Janczak"
    , email     = return $ T.pack signatoryEmail
    }
  signatoryUserCtx <- mkContextWithUser defaultLang signatoryUser
  let signatorySigLink =
        setMockSigLinkStandardField "mobile" "+48666666666"
          . setMockSigLinkStandardField "email" signatoryEmail
          $ defaultMockSigLink
  let approverEmail = "barbara.streisand@scrive.com" :: String
  approverUser <- instantiateUser $ randomUserTemplate
    { firstName = return "Jakub"
    , lastName  = return "Janczak"
    , email     = return $ T.pack approverEmail
    }
  approverUserCtx <- mkContextWithUser defaultLang approverUser
  let approverSigLink =
        setMockSigLinkStandardField "mobile" "+48666666666"
          . setMockSigLinkStandardField "email" approverEmail
          . setMockDocSigLinkSignatoryRole SignatoryRoleApprover
          $ defaultMockSigLink

  void $ testDocApiV2AddParties ctxAdmin
                                [signatorySigLink, approverSigLink]
                                (getMockDocId mockDoc)
  void $ testDocApiV2Start' ctxAdmin (getMockDocId mockDoc)
  void $ jsonTestRequestHelper signatoryUserCtx
                               GET
                               []
                               (folderAPIGet (fromJust $ grpAdmin ^. #homeFolderID))
                               200
  void $ jsonTestRequestHelper approverUserCtx
                               GET
                               []
                               (folderAPIGet (fromJust $ grpAdmin ^. #homeFolderID))
                               200

  where
    fdrAPIGet :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIGet ctx fdr code = do
      let apiCall = folderAPIGet $ fdr ^. #id
      jsonTestRequestHelper ctx GET [] apiCall code

testFolderAPIDelete :: TestEnv ()
testFolderAPIDelete = do
  user       <- instantiateUser $ randomUserTemplate { isCompanyAdmin = True }  -- TODO: Fix properly!
  ctx        <- mkContextWithUser defaultLang user
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust $ user ^. #homeFolderID)
  -- I don't want to test deletion of the user home folder, so let's create a subfolder.
  let fdrTestDeleteChild = set #parentID (Just $ userFolder ^. #id) defaultFolder
      newFolderJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren fdrTestDeleteChild
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200
  void $ jsonTestRequestHelper ctx
                               POST
                               reqNewFolderPrms
                               (folderAPIDelete $ newFdrFromAPI ^. #id)
                               200

testFolderAPIListDocs :: TestEnv ()
testFolderAPIListDocs = do
  user       <- instantiateUser $ randomUserTemplate { isCompanyAdmin = True }  -- TODO: Fix properly!
  ctx        <- mkContextWithUser defaultLang user
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust $ user ^. #homeFolderID)
  let numDocsToStart = 5
      fdrid          = userFolder ^. #id
  replicateM_ numDocsToStart $ testDocApiV2New' ctx
  -- add a subfolder to org folder tree and start documents there to ensure that we do not
  -- list documents contained in children of a specified folder
  let newFolderBase = set #parentID (Just $ userFolder ^. #id) defaultFolder
      newFolderBaseJSONBS =
        AE.encodingToLazyByteString
          . encodeFolderWithChildren
          $ folderToFolderWithChildren newFolderBase
      reqNewFolderPrms = [("folder", inTextBS newFolderBaseJSONBS)]
  newFdr <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200
  -- HACK to enable user to start documents in another folder
  dbUpdate $ FolderSetUserHomeFolder (user ^. #id) (newFdr ^. #id)
  user' <- fromJust <$> dbQuery (GetUserByID (user ^. #id))
  let ctx'            = set #maybeUser (Just user') ctx
      numDocsToStart' = 3
  replicateM_ numDocsToStart' $ testDocApiV2New' ctx'
  -- switch back to original home folder for user
  dbUpdate $ FolderSetUserHomeFolder (user ^. #id) fdrid
  docsVal <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs fdrid) 200
  assertEqual "Listing by folder id does not retrieve documents in child folders"
              numDocsToStart
              (getTotalReturned docsVal)
  -- list child folder
  docsVal' <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs (newFdr ^. #id)) 200
  assertEqual "Listing works for child of user home folder"
              numDocsToStart'
              (getTotalReturned docsVal')
  where
    getTotalReturned val =
      fromJust $ parseMaybe (withObject "List call output" (.: "total_matching")) val

testDeleteFolder :: TestEnv ()
testDeleteFolder = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  ctx          <- mkContextWithUser defaultLang user
  req          <- mkRequest POST []
  fidChild     <- createFolder $ user ^. #homeFolderID
  res          <- fst <$> runTestKontra req ctx (folderAPIDelete fidChild)
  deletedAfter <- isNothing <$> dbQuery (FolderGet fidChild)
  assertEqual "Can delete folder without child folders or documents" 200 $ rsCode res
  assertBool "New child Folder is gone after deletion" deletedAfter

testCannotDeleteFolderWithChild :: TestEnv ()
testCannotDeleteFolderWithChild = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  ctx         <- mkContextWithUser defaultLang user
  req         <- mkRequest POST []
  fidNonRoot  <- createFolder $ user ^. #homeFolderID
  fidChild    <- createFolder $ Just fidNonRoot
  res         <- fst <$> runTestKontra req ctx (folderAPIDelete fidNonRoot)
  existsAfter <- all isJust <$> mapM (dbQuery . FolderGet) [fidNonRoot, fidChild]
  assertEqual "Cannot delete folder with child folders" 400 $ rsCode res
  assertBool "New non-root and child Folder Folder remain after attempted deletion"
             existsAfter

testCannotDeleteFolderWithDocument :: TestEnv ()
testCannotDeleteFolderWithDocument = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest POST []
  fidChild <- createFolder $ user ^. #homeFolderID
  void $ addRandomDocument (rdaDefault user) { rdaFolderId = fidChild }
  res <- fst <$> runTestKontra req ctx (folderAPIDelete fidChild)
  assertEqual "Cannot delete folder with documents" 400 $ rsCode res
  existsAfter <- isJust <$> dbQuery (FolderGet fidChild)
  assertBool "New child Folder remains after attempted deletion" existsAfter

testCannotDeleteFolderWithOnlyTrashedDocument :: TestEnv ()
testCannotDeleteFolderWithOnlyTrashedDocument = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest POST []
  fidChild <- createFolder $ user ^. #homeFolderID
  doc      <- addRandomDocument (rdaDefault user) { rdaFolderId = fidChild }
  withDocument doc $ void . randomUpdate =<< runArchiveAction user ArchiveDocument
  res         <- fst <$> runTestKontra req ctx (folderAPIDelete fidChild)
  existsAfter <- isJust <$> dbQuery (FolderGet fidChild)
  assertEqual "Cannot delete folder with trashed document" 400 $ rsCode res
  assertBool "New child Folder remains after attempted deletion" existsAfter

testCanDeleteFolderWithOnlyPurgedDocument :: TestEnv ()
testCanDeleteFolderWithOnlyPurgedDocument = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  ctx      <- mkContextWithUser defaultLang user
  req      <- mkRequest POST []
  fidChild <- createFolder $ user ^. #homeFolderID
  doc      <- addRandomDocument (rdaDefault user) { rdaFolderId = fidChild }
  withDocument doc $ do
    void . randomUpdate =<< runArchiveAction user ArchiveDocument
    void . randomUpdate =<< runArchiveAction user ReallyDeleteDocument
  modifyTestTime (31 `daysAfter`)
  void . dbUpdate $ PurgeDocuments 0
  res          <- fst <$> runTestKontra req ctx (folderAPIDelete fidChild)
  deletedAfter <- isNothing <$> dbQuery (FolderGet fidChild)
  assertEqual "Can delete folder with only purged document" 200 $ rsCode res
  assertBool "New child Folder is gone after deletion" deletedAfter

testCannotDeleteHomeFolder :: TestEnv ()
testCannotDeleteHomeFolder = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Arthur"
                                               , lastName       = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               , isCompanyAdmin = True
                                               }
  let fidHome = fromJust $ user ^. #homeFolderID
  ctx         <- mkContextWithUser defaultLang user
  req         <- mkRequest POST []
  res         <- fst <$> runTestKontra req ctx (folderAPIDelete fidHome)
  existsAfter <- isJust <$> dbQuery (FolderGet fidHome)
  assertEqual "Cannot delete folder which is a home folder" 400 $ rsCode res
  assertBool "New child Folder remains after attempted deletion" existsAfter

--
-- local helpers
--

createFolder :: Maybe FolderID -> TestEnv FolderID
createFolder mparentID =
  fmap (view #id) . dbUpdate . FolderCreate $ set #parentID mparentID defaultFolder

folderToFolderWithChildren :: Folder -> FolderWithChildren
folderToFolderWithChildren folder = FolderWithChildren { folder = folder, children = [] }

jsonToFolder :: Aeson.Value -> Folder
jsonToFolder val = let Unjson.Result fdr _ = Unjson.parse unjsonFolderReadAll val in fdr

createChildrenForParentByAPI :: Context -> Folder -> TestEnv [Folder]
createChildrenForParentByAPI ctx fdrParent = do
  forM [fdrParent, fdrParent] $ \fdrp -> do
    let newFolder = set #parentID (Just $ fdrp ^. #id) defaultFolder
        newFolderJSONBS =
          AE.encodingToLazyByteString
            . encodeFolderWithChildren
            $ folderToFolderWithChildren newFolder
        reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
    jsonToFolder <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

-- read _all_ fields of the presumed JSON serialisation of a folder
unjsonFolderReadAll :: Unjson.UnjsonDef Folder
unjsonFolderReadAll =
  objectOf
    $   Folder
    <$> fieldBy "id" (^. #id) "The folder ID" Unjson.unjsonDef
    <*> fieldOptBy "parent_id" (^. #parentID) "Parent folder ID" Unjson.unjsonDef
    <*> field "name" (^. #name) "The folder name"
