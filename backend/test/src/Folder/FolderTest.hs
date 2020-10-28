module Folder.FolderTest (folderTests) where

import Control.Monad.Extra (concatForM)
import Data.Aeson hiding ((<?>))
import Data.Foldable (foldlM)
import Data.Int
import Happstack.Server
import Test.Framework
import Test.QuickCheck
import qualified Data.HashMap.Strict as H

import AccessControl.Model
import AccessControl.Types
import Context
import DB
import Doc.API.V2.JSONTest
import Doc.SignatoryLinkID ()
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
  , testThat "Cannot move Folder between different UserGroup trees"
             env
             testCannotMoveFolderBetweenDifferentUserGroupTrees
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
  void $ runTestKontra req ctx apiCallSignup

  -- 1 UserGroup Home Folder, 1 User Home folder
  assertCountAllFolders "Created tree of groups with folders" 2

testPartnerUsersWithFolders :: TestEnv ()
testPartnerUsersWithFolders = do
  partnerAdminUserGroup <- instantiateRandomFreeUserGroup
  partnerAdminUser      <- instantiateUser $ randomUserTemplate
    { firstName      = return "Arthur"
    , lastName       = return "Dent"
    , email          = return "arthur.dent@scrive.com"
    , groupID        = return $ partnerAdminUserGroup ^. #id
    , isCompanyAdmin = True
    }
  void
    .  dbUpdate
    .  AccessControlCreateForUser (partnerAdminUser ^. #id)
    .  UserGroupAdminAR
    $  partnerAdminUserGroup
    ^. #id
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  partnerUserCreate ctx cid $ partnerAdminUserGroup ^. #id
  assertCountAllFolders "Partner, Partner admin, UserGroup and User have Folders" 4

  ug <- guardJustM . dbQuery $ UserGroupGet cid
  assertBool "UserGroup has home Folder" . isJust $ ug ^. #homeFolderID

  user <- fmap head . dbQuery $ UserGroupGetUsers cid
  assertBool "User has home Folder" . isJust $ user ^. #homeFolderID
  userFolder <- guardJustM . dbQuery . FolderGet . fromJust $ user ^. #homeFolderID
  assertEqual "User home folder is child of UserGroup home folder"
              (ug ^. #homeFolderID)
              (userFolder ^. #parentID)

testPartnerUsersWithoutFolders :: TestEnv ()
testPartnerUsersWithoutFolders = do
  partnerAdminUserGroup <- instantiateRandomFreeUserGroup
  partnerAdminUser      <- instantiateUser $ randomUserTemplate
    { firstName      = return "Arthur"
    , lastName       = return "Dent"
    , email          = return "arthur.dent@scrive.com"
    , groupID        = return $ partnerAdminUserGroup ^. #id
    , isCompanyAdmin = True
    }
  void
    .  dbUpdate
    .  AccessControlCreateForUser (partnerAdminUser ^. #id)
    .  UserGroupAdminAR
    $  partnerAdminUserGroup
    ^. #id
  assertCountAllFolders "Partner and Partner admin have Folders" 2

  (ctx, cid) <- partnerCompanyCreate partnerAdminUser partnerAdminUserGroup
  assertCountAllFolders "Partner, Partner admin and UserGroup have Folders" 3

  -- unlink UserGroup from Folder
  ug <- guardJustM . dbQuery $ UserGroupGet cid
  dbUpdate . UserGroupUpdate . set #homeFolderID Nothing $ ug

  partnerUserCreate ctx cid $ partnerAdminUserGroup ^. #id
  assertCountAllFolders "User havs no Folder" 3

partnerCompanyCreate :: User -> UserGroup -> TestEnv (Context, UserGroupID)
partnerCompanyCreate partnerAdminUser partnerAdminUserGroup = do
  ctx <- mkContextWithUser defaultLang partnerAdminUser
  let partnerUgID = partnerAdminUserGroup ^. #id
  newCompanyJSON <- readTestFile "json/partner_api_v1/param-partnerCompanyCreate.json"
  let rq_newCompany_params = [("json", inTextBS newCompanyJSON)]
      rq_newCompany_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyCreate.json"
  respValue <- runApiJSONTest ctx
                              POST
                              (partnerApiCallV1CompanyCreate partnerUgID)
                              rq_newCompany_params
                              201
                              rq_newCompany_resp_fp
  let Object respObject      = snd respValue
      Just   (String cidStr) = H.lookup "id" respObject
      cid                    = read cidStr
  return (ctx, cid)

partnerUserCreate :: Context -> UserGroupID -> UserGroupID -> TestEnv ()
partnerUserCreate ctx cid partnerUgID = do
  newUserGoodJSON <- readTestFile
    "json/partner_api_v1/param-partnerCompanyUserNew-good.json"
  let rq_newUserGood_params = [("json", inTextBS newUserGoodJSON)]
      rq_newUserGood_resp_fp =
        inTestDir "json/partner_api_v1/resp-partnerCompanyUserNew-good.json"
  void $ runApiJSONTest ctx
                        POST
                        (partnerApiCallV1UserCreate partnerUgID cid)
                        rq_newUserGood_params
                        201
                        rq_newUserGood_resp_fp

testNewCompanyAccount :: TestEnv ()
testNewCompanyAccount = do
  ug   <- instantiateRandomFreeUserGroup
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Andrzej"
                                               , lastName       = return "Rybczak"
                                               , email = return "andrzej@skrivapa.se"
                                               , groupID        = return $ ug ^. #id
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  assertBool "UserGroup has home Folder" . isJust $ ug ^. #homeFolderID

  ctx    <- mkContextWithUser defaultLang user

  bobReq <- mkRequest
    POST
    [ ("add"    , inText "True")
    , ("email"  , inText "bob@blue.com")
    , ("fstname", inText "Bob")
    , ("sndname", inText "Blue")
    ]
  void . runTestKontra bobReq ctx $ handleAddUserGroupAccount
  Just userBob <- dbQuery $ GetUserByEmail (Email "bob@blue.com")
  assertBool "User has home Folder" . isJust $ userBob ^. #homeFolderID

  userFolder <- guardJustM . dbQuery . FolderGet . fromJust $ userBob ^. #homeFolderID
  assertEqual "User home folder is child of UserGroup home folder"
              (ug ^. #homeFolderID)
              (userFolder ^. #parentID)

  -- unlink UserGroup from Folder
  dbUpdate . UserGroupUpdate . set #homeFolderID Nothing $ ug

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
  assertBool "User has no home Folder" . isNothing $ userCliff ^. #homeFolderID

assertCountAllFolders :: String -> Int64 -> TestEnv ()
assertCountAllFolders msg expectedCount =
  assertSQLCount msg expectedCount "SELECT COUNT(*) FROM folders"

createFolderTree :: Int -> TestEnv (FolderID, [FolderID])
createFolderTree folderDepth = do
  -- create a tree of depth 5 with each parent having 2 subgroups
  let createFolder parentID =
        view #id <$> dbUpdate (FolderCreate $ set #parentID (Just parentID) defaultFolder)
      createFolders (parentIDs, fids) _lvl = do
        newFids <- concatForM parentIDs
          $ \parentID -> replicateM 2 $ createFolder parentID
        return (newFids, fids ++ newFids)

  fRootID      <- view #id <$> dbUpdate (FolderCreate defaultFolder)
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
  -- a folder needs to be able to have root ug associated with it
  -- create a 4 level tree, each parent 2 subfolders
  (rootfdrid, fids) <- createFolderTree 3
  void . dbUpdate . UserGroupCreate $ set #homeFolderID (Just rootfdrid) defaultUserGroup

  -- take Root.Left folder and move it to Root.Right....Right folder
  Just folder1 <- dbQuery . FolderGet $ fids !! 1
  void . dbUpdate . FolderUpdate . set #parentID (Just $ fids !! 14) $ folder1
  -- parentpath of new leaf in moved group should be 6 items long
  parentFolders <- dbQuery . FolderGetParents $ fids !! 10
  assertEqual "Fetched all parents of leaf folder" 6 (length parentFolders)
  -- root still has 14 children
  children <- fmap (concatMap fwcToList) . dbQuery . FolderGetAllChildrenRecursive $ head
    fids
  assertEqual "Fetched all children of top folder" 14 (length children)

testMoveFolderCycleError :: TestEnv ()
testMoveFolderCycleError = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #parentID Nothing $ folderA0
  void . dbUpdate . UserGroupCreate $ set #homeFolderID
                                          (Just (folderA ^. #id))
                                          defaultUserGroup
  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <- dbUpdate . FolderCreate . set #parentID (Just $ folderA ^. #id) $ folderB0
  -- making A child of B fails
  assertRaisesKontra (\(FoldersFormCycle _) -> True) . dbUpdate . FolderUpdate $ set
    #parentID
    (Just $ folderB ^. #id)
    folderA

testCannotMoveFolderBetweenDifferentUserGroupTrees :: TestEnv ()
testCannotMoveFolderBetweenDifferentUserGroupTrees = do
  userA         <- instantiateRandomUser
  userB         <- instantiateRandomUser
  movingFolder0 <- rand 10 arbitrary
  let userAHomeFid = userA ^. #homeFolderID
      userBHomeFid = userB ^. #homeFolderID
  movingFolder1 <- dbUpdate . FolderCreate . set #parentID userAHomeFid $ movingFolder0

  -- try moving Folder to the other user home Folder
  -- random users have separate UserGroup trees by default

  assertRaisesKontra (\FolderMovingBetweenDifferentsUserGroupTrees{} -> True)
    . dbUpdate
    . FolderUpdate
    $ set #parentID userBHomeFid movingFolder1

testCannotDeleteFolderWithSubfolders :: TestEnv ()
testCannotDeleteFolderWithSubfolders = do
  folderA0 <- rand 10 arbitrary
  folderA  <- dbUpdate . FolderCreate . set #parentID Nothing $ folderA0

  -- make B child of A
  folderB0 <- rand 10 arbitrary
  folderB  <- dbUpdate . FolderCreate . set #parentID (Just $ folderA ^. #id) $ folderB0
  commit -- so that raising exception will not take our data with it
  -- deleting folder A raises exception
  assertRaisesDBException . runSQL_ $ "DELETE from folders where id = " <?> folderA ^. #id
  commit
  -- deleting folder B works
  runSQL_ $ "DELETE from folders where id = " <?> folderB ^. #id
