{-# LANGUAGE OverloadedStrings #-}
module UserGroup.UserGroupTest (userGroupTests) where

import Control.Monad.Catch (try)
import Data.Foldable (foldlM)
import Data.Int
import Happstack.Server
import Test.Framework
import Test.QuickCheck

import Administration.AdministrationControl (handleCompanyChange)
import Context
import DB
import Doc.SignatoryLinkID ()
import KontraError
import TestingUtil
import TestKontra as T
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

userGroupTests :: TestEnvSt -> Test
userGroupTests env = testGroup
  "UserGroup"
  [ testThat "Test creating groups"                    env testCreateGroups
  , testThat "Test creating groups with users"         env testCreateGroupsWithUsers
  , testThat "Test fetching all users of group"        env testGetAllUsersOfTopGroup
  , testThat "Test fetching user group"                env testGetUserGroup
  , testThat "Test fetching all groups of user"        env testGetAllGroupsOfUser
  , testThat "Test moving a group"                     env testMoveGroup
  , testThat "Test moving a group cannot form a cycle" env testMoveGroupCycleError
  , testThat "Test find a parent group, where charging happens"
             env
             testFindInheritedPricePlan
  , testThat "Cannot delete a UserGroup with subgroups"
             env
             testCannotDeleteUserGroupWithSubgroups
  , testThat "Test setting user group parents"    env testChangeUserGroupParent
  , testThat "User group root must have invoice" env testUserGroupRootMustHaveInvoice
  , testThat "User group root must have address" env testUserGroupRootMustHaveAddress
  , testThat "User group root must have settings" env testUserGroupRootMustHaveSettings
  , testThat "User group tags work"               env testTags
  ]

testCreateGroups :: TestEnv ()
testCreateGroups = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroups [Nothing] [1 .. 5]
  runQuery_ ("SELECT COUNT(*) FROM user_groups" :: SQL)
  groupcount <- fetchOne runIdentity
  assertEqual ("Created tree of groups") (31 :: Int64) groupcount
  where
    createGroups :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroups mparent_ids level = (concat <$>) . forM mparent_ids $ \mparent_id -> do
      ug0 <- case level of
        1 -> ugFromUGRoot <$> rand 10 arbitrary
        _ -> rand 10 arbitrary
      ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
      return . replicate 2 . Just $ ug1 ^. #id

testCreateGroupsWithUsers :: TestEnv ()
testCreateGroupsWithUsers = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroupsWithUsers [Nothing] [1 .. 5]
  runQuery_ ("SELECT COUNT(*) FROM users" :: SQL)
  usercount <- fetchOne runIdentity
  assertEqual ("Created tree of groups and users") (31 :: Int64) usercount
  where
    createGroupsWithUsers :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroupsWithUsers mparent_ids level =
      (concat <$>) . forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return . replicate 2 . Just $ ug1 ^. #id

testGetAllUsersOfTopGroup :: TestEnv ()
testGetAllUsersOfTopGroup = do
  -- create a 5 level tree, each parent 2 subgroups
  (_, (ugidtop : _)) <- foldlM createGroupsWithUsers ([Nothing], []) [1 .. 5]
  dus                <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ ugidtop
  assertEqual ("Fetched all users of top group") 31 (length dus)
  where
    createGroupsWithUsers
      :: ([Maybe UserGroupID], [UserGroupID])
      -> Int
      -> TestEnv ([Maybe UserGroupID], [UserGroupID])
    createGroupsWithUsers (mparent_ids, ugids) level = do
      new_ugids <- forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return $ ug1 ^. #id
      --      parents for groups in next level , all group ids created
      return (Just <$> (new_ugids ++ new_ugids), ugids ++ new_ugids)

testGetUserGroup :: TestEnv ()
testGetUserGroup = do
  ug0       <- ugFromUGRoot <$> rand 10 arbitrary
  ug1       <- dbUpdate . UserGroupCreate $ ug0
  Just _ug2 <- dbQuery . UserGroupGet $ ug1 ^. #id
  return ()

testGetAllGroupsOfUser :: TestEnv ()
testGetAllGroupsOfUser = do
  -- create a 5 level tree, each parent 2 subgroups
  (lastuid : _) <- (reverse . snd)
    <$> foldlM createGroupsWithUsers ([Nothing], []) [1 .. 5]
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ lastuid
  assertEqual ("Fetched all groups of user") 5 (length $ ugwpToList ugwp)
  where
    createGroupsWithUsers
      :: ([Maybe UserGroupID], [UserID]) -> Int -> TestEnv ([Maybe UserGroupID], [UserID])
    createGroupsWithUsers (mparent_ids, uids) level = do
      (new_ugids, new_uids) <- (unzip <$>) . forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return (ug1 ^. #id, u ^. #id)
      --      parents for groups in next level , all userids created
      return (Just <$> (new_ugids ++ new_ugids), uids ++ new_uids)

-- test moving a group to other part of the tree
--
-- before
--         0
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

testMoveGroup :: TestEnv ()
testMoveGroup = do
  -- create a 4 level tree, each parent 2 subgroups
  (_, ugids) <- foldlM createGroupsWithUsers ([Nothing], []) [1 .. 4]
  -- take Root.Left group and move it to Root.Right....Right group
  Just ug1   <- dbQuery . UserGroupGet $ ugids !! 1
  void $ dbUpdate . UserGroupUpdate . set #parentGroupID (Just $ ugids !! 14) $ ug1
  -- parentpath of new leaf in moved group should be 6 items long
  Just (_ug10, parentugs) <- dbQuery . UserGroupGetWithParents $ ugids !! 10
  assertEqual ("Fetched all parents of leaf group") 6 (length parentugs)
  -- root still has 15 users
  us <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ ugids !! 0
  assertEqual ("Fetched all users of top group") 15 (length us)
  where
    createGroupsWithUsers
      :: ([Maybe UserGroupID], [UserGroupID])
      -> Int
      -> TestEnv ([Maybe UserGroupID], [UserGroupID])
    createGroupsWithUsers (mparent_ids, ugids) level = do
      new_ugids <- forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return $ ug1 ^. #id
      --      parents for groups in next level , all group ids created
      return (Just <$> concatMap (replicate 2) new_ugids, ugids ++ new_ugids)

testMoveGroupCycleError :: TestEnv ()
testMoveGroupCycleError = do
  ugA0 <- ugFromUGRoot <$> rand 10 arbitrary
  ugA  <- dbUpdate . UserGroupCreate $ ugA0
  -- make B child of A
  ugB0 <- rand 10 arbitrary
  ugB  <- dbUpdate . UserGroupCreate . set #parentGroupID (Just $ ugA ^. #id) $ ugB0
  -- making A child of B fails
  assertRaisesKontra (\(UserGroupsFormCycle _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #parentGroupID (Just $ ugB ^. #id)
    $ ugA

testFindInheritedPricePlan :: TestEnv ()
testFindInheritedPricePlan = do
  ugA0 <- ugFromUGRoot <$> rand 10 arbitrary
  ugA  <- dbUpdate . UserGroupCreate $ ugA0
  ugB0 <- rand 10 arbitrary
  ugB  <-
    dbUpdate
    . UserGroupCreate
    . set #parentGroupID (Just $ ugA ^. #id)
    . set #invoicing     None
    $ ugB0
  Just ugwithparents <- dbQuery . UserGroupGetWithParents $ ugB ^. #id
  assertEqual "A is the charging group" (ugPaymentPlan ugA)
    . Just
    . ugwpPaymentPlan
    $ ugwithparents


testCannotDeleteUserGroupWithSubgroups :: TestEnv ()
testCannotDeleteUserGroupWithSubgroups = do
  ugA0 <- ugFromUGRoot <$> rand 10 arbitrary
  ugA  <- dbUpdate . UserGroupCreate $ ugA0
  -- make B child of A
  ugB0 <- rand 10 arbitrary
  ugB  <- dbUpdate . UserGroupCreate . set #parentGroupID (Just $ ugA ^. #id) $ ugB0
  commit -- so that raising exception will not take our data with it
  -- deleting group A raises exception
  assertRaisesDBException . runSQL_ $ "DELETE from user_groups where id = " <?> ugA ^. #id
  commit
  -- deleting group B works
  runSQL_ $ "DELETE from user_groups where id = " <?> ugB ^. #id

testChangeUserGroupParent :: TestEnv ()
testChangeUserGroupParent = do
  usrGrp       <- addNewUserGroup
  parentUsrGrp <- addNewUserGroup
  let usrGrpID       = usrGrp ^. #id
      parentUsrGrpID = parentUsrGrp ^. #id
      usrEmail       = "testuseremail@scrive.com"
  Just user <- addNewUserToUserGroup "Froggie" "Freddie" usrEmail usrGrpID
  ctx       <- (set #maybeUser $ Just user) <$> mkContext defaultLang

  let params1 = [("companyparentid", inText $ showt parentUsrGrpID)]
  req1 <- mkRequest POST params1

  -- user has no admin rights so we should get a `Respond404`
  (eErrRes :: Either KontraError ((), Context)) <- do
    try (runTestKontra req1 ctx $ handleCompanyChange usrGrpID)
  assertLeft eErrRes

  -- -- user gets admin rights
  let ctx' = (set #adminAccounts [Email usrEmail]) ctx
  mUsrGrpParentBefore <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ usrGrpID)
  assertEqual "User group has no parent" mUsrGrpParentBefore Nothing
  void $ runTestKontra req1 ctx' $ handleCompanyChange usrGrpID
  mUsrGrpParentAfter <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ usrGrpID)
  assertEqual "User group parent has been set correctly" mUsrGrpParentAfter
    $ Just parentUsrGrpID

  grandParentUsrGrp <- addNewUserGroup
  let grandParentUsrGrpID = grandParentUsrGrp ^. #id
      params2             = [("companyparentid", inText $ showt grandParentUsrGrpID)]
  req2 <- mkRequest POST params2
  void $ runTestKontra req2 ctx' $ handleCompanyChange parentUsrGrpID
  mUsrGrpGrandParentAfter <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ parentUsrGrpID)
  assertEqual "User group grandparent has been set correctly" mUsrGrpGrandParentAfter
    $ Just grandParentUsrGrpID

  -- Setting a parent that already has a parent should work. We'll reuse
  -- usrGrp since it now has one.
  usrGrp' <- addNewUserGroup
  let usrGrpID' = usrGrp' ^. #id
      params3   = [("companyparentid", inText $ showt usrGrpID)]
  req3 <- mkRequest POST params3
  void $ runTestKontra req3 ctx' $ handleCompanyChange usrGrpID'
  mUsrGrpParentAfter' <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ usrGrpID')
  assertEqual "User group parent that has parent has been set correctly"
              mUsrGrpParentAfter'
    $ Just usrGrpID

  -- removing parent should work
  let params4 = [("companyparentid", inText "")]
  req4                 <- mkRequest POST params4
  mUsrGrpParentBefore' <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ usrGrpID)
  assertEqual "User group parent still set" mUsrGrpParentBefore' $ Just parentUsrGrpID
  void $ runTestKontra req4 ctx' $ handleCompanyChange usrGrpID
  mUsrGrpParentAfter'' <-
    join <$> (view #parentGroupID <$>) <$> (dbQuery . UserGroupGet $ usrGrpID)
  assertEqual "User group parent has been removed" mUsrGrpParentAfter'' Nothing

testUserGroupRootMustHaveInvoice :: TestEnv ()
testUserGroupRootMustHaveInvoice = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root with ugInvoicing other than Invoice fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupCreate
    . set #invoicing None
    $ ug0
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupCreate
    . set #invoicing (BillItem Nothing)
    $ ug0
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupCreate
    . set #invoicing (BillItem (Just FreePlan))
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- updating a root with ugInvoicing other than Invoice fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing None
    $ ug1
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing (BillItem Nothing)
    $ ug1
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing (BillItem (Just FreePlan))
    $ ug1

testUserGroupRootMustHaveAddress :: TestEnv ()
testUserGroupRootMustHaveAddress = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root without address fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupCreate
    . set #address Nothing
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- clearing address from root fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #address Nothing
    $ ug1

testUserGroupRootMustHaveSettings :: TestEnv ()
testUserGroupRootMustHaveSettings = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root without settings fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupCreate
    . set #settings Nothing
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- clearing settings from root fails
  assertRaisesKontra (\(UserGroupIsInvalidAsRoot _) -> True)
    . dbUpdate
    . UserGroupUpdate
    . set #settings Nothing
    $ ug1

testTags :: TestEnv ()
testTags = do
  protoUg <- ugFromUGRoot <$> rand 10 arbitrary
  iTags   <- rand 10 arbitrary
  eTags   <- rand 10 arbitrary
  ug      <-
    dbUpdate
    . UserGroupCreate
    . set #internalTags iTags
    . set #externalTags eTags
    $ protoUg
  let ugid = ug ^. #id
  Just ugRes <- dbQuery $ UserGroupGet ugid
  assertEqual "Internal tags match after create" iTags (ugRes ^. #internalTags)
  assertEqual "External tags match after create" eTags (ugRes ^. #externalTags)

  iTags2 <- rand 10 arbitrary
  eTags2 <- rand 10 arbitrary
  _      <-
    dbUpdate . UserGroupUpdate . set #internalTags iTags2 . set #externalTags eTags2 $ ug
  Just ugRes2 <- dbQuery $ UserGroupGet ugid
  assertEqual "Internal tags match after update" iTags2 (ugRes2 ^. #internalTags)
  assertEqual "External tags match after update" eTags2 (ugRes2 ^. #externalTags)
