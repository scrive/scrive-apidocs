module Folder.FolderTest (folderTests) where

import Control.Monad.Extra (concatForM)
import Data.Aeson hiding ((<?>))
import Data.Aeson.Types hiding ((<?>))
import Data.Foldable (foldlM)
import Data.Int
import Data.Unjson (field, fieldBy, fieldOptBy, objectOf)
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AE
import qualified Data.HashMap.Strict as H
import qualified Data.Unjson as Unjson

import AccessControl.Model
import AccessControl.Types
import Administration.AdministrationControl
import Context
import DB
import Doc.API.V2.AesonTestUtils
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.JSONTest
import Doc.SignatoryLinkID ()
import Folder.API
import Folder.JSON
import Folder.Model
import Partner.API
import TestingUtil
import TestKontra as T
import User.API
import User.Email
import User.Model
import UserGroup
import UserGroupAccounts.UserGroupAccountsControl
import Util.MonadUtils

folderTests :: TestEnvSt -> Test
folderTests env = testGroup
  "Folder"
  [ testThat "Creating groups works in DB"                env testCreateFolders
  , testThat "Get children recursive works" env testFolderGetAllChildrenRecursive
  , testThat "New signup users have home Folders" env testSignuUsersHaveHomeFolders
  , testThat "New partner create users have home Folders" env testPartnerUsersWithFolders
  , testThat "New partner create users have home Folders, only if companies do"
             env
             testPartnerUsersWithoutFolders
  , testThat "Add new user in unsergroup"        env testNewCompanyAccount
  , testThat "Moving a folder works"             env testMoveFolder
  , testThat "Moving a folder cannot form cycle" env testMoveFolderCycleError
  , testThat "Cannot delete a Folder with subfolders"
             env
             testCannotDeleteFolderWithSubfolders
  , testThat "Migration triggers work"                    env testMigrationTriggersWork
  , testThat "Test creating groups works in API "         env testFolderAPICreate
  , testThat "Test updating groups works in API"          env testFolderAPIUpdate
  , testThat "Test reading groups works in API"           env testFolderAPIGet
  , testThat "Test folder deletion endpoint works in API" env testFolderAPIDelete
  , testThat "Test listing documents in a folder works in API" env testFolderAPIListDocs
  ]

testCreateFolders :: TestEnv ()
testCreateFolders = do
  let folderDepth = 5
  void $ createFolderTree folderDepth
  assertCountAllFolders "Created tree of groups" (2 ^ (folderDepth + 1) - 1)

testFolderGetAllChildrenRecursive :: TestEnv ()
testFolderGetAllChildrenRecursive = do
  let folderDepth = 5
  (fRootID, _) <- createFolderTree folderDepth
  allChildren  <- dbQuery $ FolderGetAllChildrenRecursive fRootID
  assertEqual "All children returned"
              (2 ^ (folderDepth + 1) - 2)
              (length $ concatMap fwcToList allChildren)

testSignuUsersHaveHomeFolders :: TestEnv ()
testSignuUsersHaveHomeFolders = do
  -- signup new user
  ctx <- mkContext defaultLang
  req <- mkRequest POST [("email", inText "jabberwocky@scrive.com")]
  void $ runTestKontra req ctx $ apiCallSignup

  -- 1 UserGroup Home Folder, 1 User Home folder
  assertCountAllFolders "Created tree of groups with folders" 2

testPartnerUsersWithFolders :: TestEnv ()
testPartnerUsersWithFolders = do
  (partnerAdminUser, partnerAdminUserGroup) <- addNewRandomPartnerUser
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  partnerUserCreate ctx cid $ ugID partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin, UserGroup and User have Folders" 4

  ug <- guardJustM . dbQuery $ UserGroupGet cid
  assertBool "UserGroup has home Folder" . isJust $ ugHomeFolderID ug

  user <- fmap head . dbQuery $ UserGroupGetUsers cid
  assertBool "User has home Folder" . isJust $ userhomefolderid user
  userFolder <- guardJustM . dbQuery . FolderGet . fromJust $ userhomefolderid user
  assertEqual "User home folder is child of UserGroup home folder"
              (ugHomeFolderID ug)
              (folderParentID userFolder)

testPartnerUsersWithoutFolders :: TestEnv ()
testPartnerUsersWithoutFolders = do
  (partnerAdminUser, partnerAdminUserGroup) <- addNewRandomPartnerUser
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  -- unlink UserGroup from Folder
  ug <- guardJustM . dbQuery $ UserGroupGet cid
  dbUpdate . UserGroupUpdate . set #ugHomeFolderID Nothing $ ug

  partnerUserCreate ctx cid $ ugID partnerAdminUserGroup
  assertCountAllFolders "User havs no Folder" 3

partnerCompanyCreate :: User -> UserGroup -> TestEnv (Context, UserGroupID)
partnerCompanyCreate partnerAdminUser partnerAdminUserGroup = do
  ctx <- (set #maybeUser (Just partnerAdminUser)) <$> mkContext defaultLang
  let partnerUgID = ugID partnerAdminUserGroup
  newCompanyJSON <- readTestFile "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [("json", inTextBS newCompanyJSON)]
      rq_newCompany_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"
  respValue <- runApiJSONTest
    ctx
    POST
    (partnerApiCallV1CompanyCreate $ fromUserGroupID partnerUgID)
    rq_newCompany_params
    201
    rq_newCompany_resp_fp
  let Object respObject      = snd respValue
      Just   (String cidStr) = H.lookup "id" respObject
      cid                    = read cidStr
  return (ctx, cid)

partnerUserCreate :: Context -> UserGroupID -> UserGroupID -> TestEnv ()
partnerUserCreate ctx cid pid = do
  newUserGoodJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [("json", inTextBS newUserGoodJSON)]
      rq_newUserGood_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserCreate (fromUserGroupID pid) cid)
                        rq_newUserGood_params
                        201
                        rq_newUserGood_resp_fp

testNewCompanyAccount :: TestEnv ()
testNewCompanyAccount = do
  (user, ug) <- addNewAdminUserAndUserGroup "Andrzej" "Rybczak" "andrzej@skrivapa.se"
  assertBool "UserGroup has home Folder" . isJust $ ugHomeFolderID ug

  ctx    <- (set #maybeUser (Just user)) <$> mkContext defaultLang

  bobReq <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "bob@blue.com")
    , ("fstname", inText "Bob")
    , ("sndname", inText "Blue")
    ]
  void . runTestKontra bobReq ctx $ handleAddUserGroupAccount
  Just userBob <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertBool "User has home Folder" . isJust $ userhomefolderid userBob

  userFolder <- guardJustM . dbQuery . FolderGet . fromJust $ userhomefolderid userBob
  assertEqual "User home folder is child of UserGroup home folder"
              (ugHomeFolderID ug)
              (folderParentID userFolder)

  -- unlink UserGroup from Folder
  dbUpdate . UserGroupUpdate . set #ugHomeFolderID Nothing $ ug

  -- create another user
  cliffReq <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "cliff@cranberry.com")
    , ("fstname", inText "Cliff")
    , ("sndname", inText "Cranberry")
    ]
  void . runTestKontra cliffReq ctx $ handleAddUserGroupAccount
  Just userCliff <- dbQuery $ GetUserByEmail (Email "cliff@cranberry.com")
  assertBool "User has no home Folder" . not . isJust $ userhomefolderid userCliff

assertCountAllFolders :: String -> Int64 -> TestEnv ()
assertCountAllFolders msg expectedCount =
  assertSQLCount msg expectedCount "SELECT COUNT(*) FROM folders"

createFolderTree :: Int -> TestEnv (FolderID, [FolderID])
createFolderTree folderDepth = do
  -- create a tree of depth 5 with each parent having 2 subgroups
  let createFolder parentID =
        fmap folderID
          . dbUpdate
          . FolderCreate
          . set #folderParentID (Just parentID)
          $ defaultFolder
      createFolders (parentIDs, fids) _lvl = do
        newFids <- concatForM parentIDs
          $ \parentID -> replicateM 2 $ createFolder parentID
        return (newFids, fids ++ newFids)

  fRootID      <- fmap folderID . dbUpdate $ FolderCreate defaultFolder
  (_, allFids) <- foldlM createFolders ([fRootID], [fRootID]) [1 .. folderDepth]
  return (fRootID, allFids)


-- test moving a folder to other part of the tree
--
-- before
--          0
--     1         2
--  3   4     5     6
-- 7 8 9 10 11 12 13 14
--
-- after
--         0
--               2
--            5     6
--          11 12 13 14
--                    1
--                 3   4
--                7 8 9 10

testMoveFolder :: TestEnv ()
testMoveFolder = do
  -- create a 4 level tree, each parent 2 subfolders
  (_, fids)    <- createFolderTree 3
  -- take Root.Left folder and move it to Root.Right....Right folder
  Just folder1 <- dbQuery . FolderGet $ fids !! 1
  void $ dbUpdate . FolderUpdate . set #folderParentID (Just $ fids !! 14) $ folder1
  -- parentpath of new leaf in moved group should be 6 items long
  parentFolders <- dbQuery . FolderGetParents $ fids !! 10
  assertEqual ("Fetched all parents of leaf folder") 6 (length parentFolders)
  -- root still has 14 children
  children <-
    fmap (concatMap fwcToList) . dbQuery . FolderGetAllChildrenRecursive $ fids !! 0
  assertEqual ("Fetched all children of top folder") 14 (length children)

testMoveFolderCycleError :: TestEnv ()
testMoveFolderCycleError = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #folderParentID Nothing $ folderA0

  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <-
    dbUpdate . FolderCreate . set #folderParentID (Just $ folderID folderA) $ folderB0
  -- making A child of B fails
  assertRaisesKontra (\(FoldersFormCycle _) -> True) . dbUpdate . FolderUpdate $ set
    #folderParentID
    (Just $ folderID folderB)
    folderA

testCannotDeleteFolderWithSubfolders :: TestEnv ()
testCannotDeleteFolderWithSubfolders = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #folderParentID Nothing $ folderA0

  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <-
    dbUpdate . FolderCreate . set #folderParentID (Just $ folderID folderA) $ folderB0
  commit -- so that raising exception will not take our data with it
  -- deleting folder A raises exception
  assertRaisesDBException
    .   runSQL_
    $   "DELETE from folders where id = "
    <?> folderID folderA
  commit
  -- deleting folder B works
  runSQL_ $ "DELETE from folders where id = " <?> folderID folderB

testMigrationTriggersWork :: TestEnv ()
testMigrationTriggersWork = do
  (user, _) <- addNewRandomUserWithCompany' False
  void $ addNewRandomUserWithCompany' False
  void $ addNewRandomUserWithCompany' False
  assertCountAllFolders "Created no folders" 0

  void $ addRandomDocumentWithAuthor user

  ctx0 <- mkContext defaultLang
  let ctx =
        set #adminAccounts [useremail $ userinfo user]
          . set #maybeUser (Just user)
          $ ctx0
  req <- mkRequest GET []
  void $ runTestKontra req ctx $ handleTriggerMigrateFolders 1
  assertCountAllFolders "Added Folders for 1 UserGroup with 1 User" 2

  void $ runTestKontra req ctx $ handleTriggerMigrateFolders 2
  assertCountAllFolders "Added the rest of Folders" 6

  Just user' <- dbQuery . GetUserByID $ userid user

  void $ addRandomDocumentWithAuthor user'

  assertSQLCount "All documents" 2 "SELECT COUNT(*) FROM documents"
  -- only 1 document was added after migration
  assertSQLCount "Documents without folders"
                 1
                 "SELECT COUNT(*) FROM documents where folder_id IS NULL"

  void $ runTestKontra req ctx $ handleTriggerMigrateDocuments 1

  assertSQLCount "Documents without folders"
                 0
                 "SELECT COUNT(*) FROM documents where folder_id IS NULL"

-- API tests
testFolderAPICreate :: TestEnv ()
testFolderAPICreate = do
  user      <- addNewRandomUser
  ctx       <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  -- make root of folder struct
  fdrRootID <- folderID <$> do
    dbUpdate . FolderCreate $ defaultFolder

  folderNameTest <- rand 10 arbitraryName
  let fdrTest =
        set #folderName folderNameTest
          . set #folderParentID (Just fdrRootID)
          $ defaultFolder
      newFolderJSONBS  = AE.encodingToLazyByteString $ encodeFolder fdrTest
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  -- should not be able to create a child since the user is not an admin of the
  -- above folder root
  void $ jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 403

  -- make admin of folder
  let folderAdminR = FolderAdminAR fdrRootID
  void . dbUpdate $ AccessControlInsertRoleForUser (userid user) folderAdminR
  -- should now be able to create a child
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

  -- check that API returns what it's supposed to
  Just newFdrFromDB <- dbQuery . FolderGet . folderID $ newFdrFromAPI
  assertEqual ("New folder from API equals the one we sent in")
              newFdrFromAPI
              (set #folderID (folderID newFdrFromAPI) fdrTest)
              -- since id is 0 for default

  assertEqual ("New folder from API equals corresponding one in DB")
              newFdrFromAPI
              newFdrFromDB

testFolderAPIUpdate :: TestEnv ()
testFolderAPIUpdate = do
  grpAdmin <- addNewRandomUser
  user     <- addNewRandomUser
  ctxAdmin <- (set #maybeUser (Just grpAdmin)) <$> mkContext defaultLang
  ctxUser  <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  fdrRoot  <- dbUpdate $ FolderCreate (set #folderName "Folder root" defaultFolder)
  let folderAdminRoot = FolderAdminAR (folderID fdrRoot)
      admid           = userid grpAdmin
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminRoot
  subs@[fdr1        , fdr2         ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     childrenFdr1, _childrenFdr2] <- forM subs $ createChildrenForParentByAPI ctxAdmin

  let folderAdminUserFdr1 = FolderAdminAR (folderID fdr1)
  void . dbUpdate $ AccessControlInsertRoleForUser (userid user) folderAdminUserFdr1

  -- `user` should be able to update fdr1-rooted tree
  forM_ childrenFdr1 (updateTestHelper ctxUser)

  fdr <-
    dbUpdate
    $ FolderCreate
    $ (set #folderParentID (Just $ folderID fdr1) . set #folderName "Folder for move test"
      )
        defaultFolder
  allChildrenFdr1 <-
    concatMap fwcToList <$> (dbQuery $ FolderGetAllChildrenRecursive $ folderID fdr1)
  assertEqual "Children of folder are the same"
              (sortByFolderID allChildrenFdr1)
              (sortByFolderID $ [fdr] <> childrenFdr1)

  let fdrTryMoving = set #folderParentID (Just $ folderID fdr2) fdr
  -- `user` should _not_ be able to change parent of fdrTryMoving to fdr2 before being
  -- an admin of both parents
  void $ fdrAPIUpdate ctxUser fdrTryMoving 403

  -- make `user` admin of `fdr2`
  void . dbUpdate $ AccessControlInsertRoleForUser (userid user)
                                                   (FolderAdminAR (folderID fdr2))

  -- move should now be successful
  movedFdr1'        <- jsonToFolder <$> fdrAPIUpdate ctxUser fdrTryMoving 200

  Just moveddbFdr1' <- dbQuery . FolderGet . folderID $ fdrTryMoving
  assertEqual "Moved folder from API should equal the corresponding one in DB"
              moveddbFdr1'
              movedFdr1'
  assertEqual "Moved folder from API should equal the one specified"
              moveddbFdr1'
              fdrTryMoving

  fromDBchildrenFdr1' <- do
    dbQuery . FolderGetImmediateChildren . folderID $ fdr1
  assertEqual "Children of folder are the same"
              (sortByFolderID fromDBchildrenFdr1')
              (sortByFolderID childrenFdr1)

  parentsOfFdr <- dbQuery . FolderGetParents $ folderID fdr
  assertEqual "Parent path of moved folder is correct" parentsOfFdr [fdr2, fdrRoot]
  where
    sortByFolderID :: [Folder] -> [Folder]
    sortByFolderID = sortBy (\f1 f2 -> compare (folderID f1) (folderID f2))

    fdrUpdateParams :: Folder -> [(Text, Input)]
    fdrUpdateParams fdr =
      let fdrJSON = AE.encodingToLazyByteString $ encodeFolder fdr
      in  [("folder", inTextBS fdrJSON)]

    fdrAPIUpdate :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIUpdate ctx fdr code = do
      let apiCall = folderAPIUpdate $ folderID fdr
      jsonTestRequestHelper ctx POST (fdrUpdateParams fdr) apiCall code

    updateTestHelper :: Context -> Folder -> TestEnv ()
    updateTestHelper ctx fdr = do
      name <- rand 10 arbitraryName
      let fdr' = (set #folderName name fdr)
      updatedFdr <- jsonToFolder <$> fdrAPIUpdate ctx fdr' 200
      assertEqual ("New folder from API should equal the one specified") fdr' updatedFdr
      Just dbFdr <- dbQuery . FolderGet . folderID $ fdr
      assertEqual ("New folder from API should equal the corresponding one in DB")
                  dbFdr
                  updatedFdr
      -- reset for easier testing in calling body
      void $ jsonToFolder <$> fdrAPIUpdate ctx fdr 200

testFolderAPIGet :: TestEnv ()
testFolderAPIGet = do
  grpAdmin <- addNewRandomUser
  ctxAdmin <- (set #maybeUser (Just grpAdmin)) <$> mkContext defaultLang
  fdrRoot  <- dbUpdate $ FolderCreate (set #folderName "Folder root" defaultFolder)
  let folderAdminR = FolderAdminAR (folderID fdrRoot)
      admid        = userid grpAdmin
  void . dbUpdate $ AccessControlInsertRoleForUser admid folderAdminR
  subs@[_fdr1          , _fdr2          ] <- createChildrenForParentByAPI ctxAdmin fdrRoot
  [     _childrenOfFDR1, _childrenOfFDR2] <- do
    forM subs $ createChildrenForParentByAPI ctxAdmin
  fdrwcfuFromAPI <- fdrAPIGet ctxAdmin fdrRoot 200
  fdrChildren    <- dbQuery $ FolderGetImmediateChildren (folderID fdrRoot)
  let fdrwc =
        FolderWithChildren fdrRoot $ map (\c -> FolderWithChildren c []) fdrChildren
      fdrwcbs =
        fromJust . Aeson.decode . AE.encodingToLazyByteString $ encodeFolderWithChildren
          fdrwc
  assertEqual "Folder data output corresponds to what was sent in" fdrwcfuFromAPI fdrwcbs
  where
    fdrAPIGet :: Context -> Folder -> Int -> TestEnv Aeson.Value
    fdrAPIGet ctx fdr code = do
      let apiCall = folderAPIGet . folderID $ fdr
      jsonTestRequestHelper ctx GET [] apiCall code

testFolderAPIDelete :: TestEnv ()
testFolderAPIDelete = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust . userhomefolderid $ user)
  -- I don't want to test deletion of the user home folder, so let's create a subfolder.
  let fdrTestDeleteChild =
        set #folderParentID (Just $ folderID userFolder) $ defaultFolder
      newFolderJSONBS  = AE.encodingToLazyByteString $ encodeFolder fdrTestDeleteChild
      reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
  newFdrFromAPI <- jsonToFolder
    <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200
  void $ jsonTestRequestHelper ctx
                               POST
                               reqNewFolderPrms
                               (folderAPIDelete $ folderID newFdrFromAPI)
                               200

testFolderAPIListDocs :: TestEnv ()
testFolderAPIListDocs = do
  user       <- addNewRandomUser
  ctx        <- (set #maybeUser (Just user)) <$> mkContext defaultLang
  userFolder <- fromJust <$> (dbQuery . FolderGet . fromJust . userhomefolderid $ user)
  let numDocsToStart = 5
      fdrid          = folderID userFolder
  mapM_ testDocApiV2New' $ take numDocsToStart . repeat $ ctx
  -- add a subfolder to org folder tree and start documents there to ensure that we do not
  -- list documents contained in children of a specified folder
  let newFolderBase = set #folderParentID (Just . folderID $ userFolder) defaultFolder
      newFolderBaseJSONBS = AE.encodingToLazyByteString $ encodeFolder newFolderBase
      reqNewFolderPrms = [("folder", inTextBS newFolderBaseJSONBS)]
  newFdr <-
    jsonToFolder <$> (jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200)
  -- HACK to enable user to start documents in another folder
  dbUpdate $ FolderSetUserHomeFolder (userid user) (folderID newFdr)
  user' <- fromJust <$> (dbQuery $ GetUserByID (userid user))
  let ctx'            = set #maybeUser (Just user') ctx
      numDocsToStart' = 3
  mapM_ testDocApiV2New' $ take numDocsToStart' . repeat $ ctx'
  -- switch back to original home folder for user
  dbUpdate $ FolderSetUserHomeFolder (userid user) fdrid
  docsVal <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs fdrid) 200
  assertEqual "Listing by folder id does not retrieve documents in child folders"
              numDocsToStart
              (getTotalReturned docsVal)
  -- list child folder
  docsVal' <- jsonTestRequestHelper ctx GET [] (folderAPIListDocs (folderID newFdr)) 200
  assertEqual "Listing works for child of user home folder"
              numDocsToStart'
              (getTotalReturned docsVal')
  where
    getTotalReturned val =
      fromJust $ parseMaybe (withObject "List call output" (.: "total_matching")) val


--  dbUpdate $ FolderSetUserHome (userid user) (fdrid)

--
-- local helpers
--

jsonToFolder :: Aeson.Value -> Folder
jsonToFolder val = let Unjson.Result fdr _ = Unjson.parse unjsonFolderReadAll val in fdr

-- jsonToFolderWithChildrenForUpdate :: Aeson.Value -> FolderWithChildrenForUpdate
-- jsonToFolderWithChildrenForUpdate val =
--   let Unjson.Result fdrwcfu _ =
--         Unjson.parse unjsonFolderWithChildrenForUpdate val
--   in fdrwcfu

createChildrenForParentByAPI :: Context -> Folder -> TestEnv [Folder]
createChildrenForParentByAPI ctx fdrParent = do
  forM [fdrParent, fdrParent] $ \fdrp -> do
    let newFolder        = set #folderParentID (Just . folderID $ fdrp) defaultFolder
        newFolderJSONBS  = AE.encodingToLazyByteString $ encodeFolder newFolder
        reqNewFolderPrms = [("folder", inTextBS newFolderJSONBS)]
    jsonToFolder <$> jsonTestRequestHelper ctx POST reqNewFolderPrms folderAPICreate 200

-- read _all_ fields of the presumed JSON serialisation of a folder
unjsonFolderReadAll :: Unjson.UnjsonDef Folder
unjsonFolderReadAll =
  objectOf
    $   pure Folder
    <*> (fieldBy "id" folderID "The folder ID" Unjson.unjsonDef)
    <*> (fieldOptBy "parent_id" folderParentID "Parent folder ID" Unjson.unjsonDef)
    <*> (field "name" folderName "The folder name")
