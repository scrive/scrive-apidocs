{-# LANGUAGE OverloadedStrings #-}
module UserGroup.UserGroupTest (userGroupTests) where

import Control.Monad.Catch
import Data.Foldable (foldlM)
import Data.Int
import Log
import Test.Framework
import Test.QuickCheck

import BrandedDomain.BrandedDomain
import Company.CompanyUI.Model
import Company.Data
import Company.Model
import DB
import Doc.SignatoryLinkID ()
import Kontra
import Partner.Model
import TestingUtil
import TestKontra as T
import Theme.Model
import User.Model
import UserGroup.Data
import UserGroup.Model

userGroupTests :: TestEnvSt -> Test
userGroupTests env = testGroup "Grouping"
  [ testThat "Test creating groups" env testCreateGroups
  , testThat "Test creating groups with users" env testCreateGroupsWithUsers
  , testThat "Test fetching all users of group" env testGetAllUsersOfTopGroup
  , testThat "Test fetching user group" env testGetUserGroup
  , testThat "Test fetching all groups of user" env testGetAllGroupsOfUser
  , testThat "Test moving a group" env testMoveGroup
  , testThat "Test moving a group cannot form a cycle" env testMoveGroupCycleError
  , testThat "Test find a parent group, where charging happens" env testFindInheritedPricePlan
  , testThat "Can update company without user_group" env testCanUpdateCompanyWithoutUserGroup
  , testThat "Can update partner company without user_group" env testCanUpdatePartnerCompanyWithoutUserGroup
  , testThat "Can update company with user_group" env testCanUpdateCompanyWithUserGroup
  , testThat "Can update partner company with user_group" env testCanUpdatePartnerCompanyWithUserGroup
  , testThat "Update of company does the same to it's group" env testUpdateOfCompanyDoesTheSameToItsGroup
  , testThat "Update of partner company does the same to it's group" env testUpdateOfCompanyWithPartnerDoesTheSameToItsGroup
  , testThat "Cannot delete a UserGroup with subgroups" env testCannotDeleteUserGroupWithSubgroups
  ]

testCreateGroups :: TestEnv ()
testCreateGroups = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroups [Nothing] [1..5]
  runQuery_ ("SELECT COUNT(*) FROM user_groups" :: SQL)
  groupcount <- fetchOne runIdentity
  assertEqual ("Created tree of groups") (31::Int64) groupcount
  where
    createGroups :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroups mparent_ids level = (concat <$>) . forM mparent_ids $ \mparent_id -> do
      ug0 <- case level of
        1 -> unARootUserGroup <$> rand 10 arbitrary
        _ -> rand 10 arbitrary
      ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID mparent_id $ ug0
      return . replicate 2 . Just . get ugID $ ug1

testCreateGroupsWithUsers :: TestEnv ()
testCreateGroupsWithUsers = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroupsWithUsers [Nothing] [1..5]
  runQuery_ ("SELECT COUNT(*) FROM users" :: SQL)
  usercount <- fetchOne runIdentity
  assertEqual ("Created tree of groups and users") (31::Int64) usercount
  where
    createGroupsWithUsers :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroupsWithUsers mparent_ids level = (concat <$>) . forM mparent_ids $ \mparent_id -> do
      ug0 <- case level of
        1 -> unARootUserGroup <$> rand 10 arbitrary
        _ -> rand 10 arbitrary
      ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID mparent_id $ ug0
      u <- addNewRandomUser
      void . dbUpdate $ SetUserGroup (userid u) (Just . get ugID $ ug1)
      return . replicate 2 . Just . get ugID $ ug1

testGetAllUsersOfTopGroup :: TestEnv ()
testGetAllUsersOfTopGroup = do
  -- create a 5 level tree, each parent 2 subgroups
  (_, (ugidtop:_)) <- foldlM createGroupsWithUsers ([Nothing],[]) [1..5]
  dus <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ ugidtop
  assertEqual ("Fetched all users of top group") 31 (length dus)
  where
    createGroupsWithUsers :: ([Maybe UserGroupID], [UserGroupID]) -> Int -> TestEnv ([Maybe UserGroupID], [UserGroupID])
    createGroupsWithUsers (mparent_ids, ugids) level = do
      new_ugids <- forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> unARootUserGroup <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID mparent_id $ ug0
        u <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (userid u) (Just . get ugID $ ug1)
        return . get ugID $ ug1
      --      parents for groups in next level , all group ids created
      return (Just <$> (new_ugids ++ new_ugids), ugids ++ new_ugids)

testGetUserGroup :: TestEnv ()
testGetUserGroup = do
  ug0 <- unARootUserGroup <$> rand 10 arbitrary
  ug1 <- dbUpdate . UserGroupCreate $ ug0
  Just _ug2 <- dbQuery . UserGroupGet . get ugID $ ug1
  return ()

testGetAllGroupsOfUser :: TestEnv ()
testGetAllGroupsOfUser = do
  -- create a 5 level tree, each parent 2 subgroups
  (lastuid:_) <- (reverse . snd) <$> foldlM createGroupsWithUsers ([Nothing],[]) [1..5]
  Just lastu <- dbQuery . GetUserByID $ lastuid
  ugs <- dbQuery . UserGetAllParentGroups $ lastu
  assertEqual ("Fetched all groups of user") 5 (length ugs)
  where
    createGroupsWithUsers :: ([Maybe UserGroupID], [UserID]) -> Int -> TestEnv ([Maybe UserGroupID], [UserID])
    createGroupsWithUsers (mparent_ids, uids) level = do
      (new_ugids, new_uids) <- (unzip <$>) . forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> unARootUserGroup <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate .set ugParentGroupID mparent_id $ ug0
        u <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (userid u) (Just . get ugID $ ug1)
        return (get ugID ug1, userid u)
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
  (_, ugids) <- foldlM createGroupsWithUsers ([Nothing],[]) [1..4]
  -- take Root.Left group and move it to Root.Right....Right group
  Just ug1 <- dbQuery . UserGroupGet $ ugids !! 1
  dbUpdate . UserGroupUpdate . set ugParentGroupID (Just $ ugids !! 14) $ ug1
  -- parentpath of new leaf in moved group should be 6 items long
  Just (_ug10, parentugs) <- dbQuery . UserGroupGetWithParents $ ugids !! 10
  assertEqual ("Fetched all parents of leaf group") 6 (length parentugs)
  -- root still has 15 users
  us <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ ugids !! 0
  assertEqual ("Fetched all users of top group") 15 (length us)
  where
    createGroupsWithUsers :: ([Maybe UserGroupID], [UserGroupID]) -> Int -> TestEnv ([Maybe UserGroupID], [UserGroupID])
    createGroupsWithUsers (mparent_ids, ugids) level = do
      new_ugids <- forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> unARootUserGroup <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID mparent_id $ ug0
        u <- addNewRandomUser
        void . dbUpdate $ SetUserGroup (userid u) (Just . get ugID $ ug1)
        return . get ugID $ ug1
      --      parents for groups in next level , all group ids created
      return (Just <$> concatMap (replicate 2) new_ugids, ugids ++ new_ugids)

testMoveGroupCycleError :: TestEnv ()
testMoveGroupCycleError = do
  ugA0 <- unARootUserGroup <$> rand 10 arbitrary
  ugA <- dbUpdate . UserGroupCreate $ ugA0
  -- make B child of A
  ugB0 <- rand 10 arbitrary
  ugB <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just . get ugID $ ugA) $ ugB0
  -- making A child of B fails
  assertRaisesKontra (\(UserGroupsFormCycle _) -> True) . dbUpdate . UserGroupUpdate . set ugParentGroupID (Just . get ugID $ ugB) $ ugA

testFindInheritedPricePlan :: TestEnv ()
testFindInheritedPricePlan = do
  ugA0 <- unARootUserGroup <$> rand 10 arbitrary
  ugA <- dbUpdate . UserGroupCreate $ ugA0
  ugB0 <- rand 10 arbitrary
  ugB <- dbUpdate . UserGroupCreate
    . set ugParentGroupID (Just . get ugID $ ugA)
    . set ugInvoicing     None
    $ ugB0
  Just ugwithparents <- dbQuery . UserGroupGetWithParents . get ugID $ ugB
  assertEqual "A is the charging group" (ugPaymentPlan ugA) . ugInherited ugPaymentPlan $ ugwithparents

testCanUpdateCompanyWithoutUserGroup :: TestEnv ()
testCanUpdateCompanyWithoutUserGroup = replicateM_ 20 $ do
  c@Company{companyid = cid} <- dbUpdate $ CreateCompanyWithoutUserGroup
  result1 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = companypartnerid . companyinfo $ c }
  assertEqual "SetCompanyInfo result is True" True result1

  result2 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyPaymentPlan result is True" True result2

testCanUpdateCompanyWithUserGroup :: TestEnv ()
testCanUpdateCompanyWithUserGroup = replicateM_ 20 $ do
  -- create company
  c@Company{companyid = cid} <- dbUpdate $ CreateCompanyWithoutUserGroup
  -- convert everything to user_groups
  numberOfUpdates <- migrateToUserGroups 100
  assertEqual "Only the single company was migrated" 1 numberOfUpdates
  -- do not set company partner when randomly updating CompanyInfo
  result1 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = companypartnerid . companyinfo $ c }
  assertEqual "SetCompanyInfo result is True" True result1

  result2 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyPaymentPlan result is True" True result2

testUpdateOfCompanyWithPartnerDoesTheSameToItsGroup :: TestEnv ()
testUpdateOfCompanyWithPartnerDoesTheSameToItsGroup = replicateM_ 20 $ do
  -- create company
  c1@(Company{companyid = cid}) <- dbUpdate $ CreateCompanyWithoutUserGroup
  -- setup some themes
  ctx <- mkContext def
  mailThemeFromDomain <- dbQuery . GetTheme . get (bdMailTheme . ctxbrandeddomain) $ ctx
  mailTheme <- dbUpdate $ InsertNewThemeForCompany cid mailThemeFromDomain
  signviewThemeFromDomain <- dbQuery $ GetTheme (get (bdSignviewTheme . ctxbrandeddomain) ctx)
  signviewTheme <- dbUpdate $ InsertNewThemeForCompany cid signviewThemeFromDomain
  cui <- dbQuery $ GetCompanyUI cid
  result0 <- dbUpdate $ SetCompanyUI cid cui {
      companyMailTheme     = Just $ themeID mailTheme
    , companySignviewTheme = Just $ themeID signviewTheme
    }
  assertEqual "SetCompanyUI result is True" True result0
  -- create partner
  user <- addNewRandomUserAndMaybeUserGroup False
  pid <- dbUpdate $ AddNewPartner "My Favourite Upsales"
  result1 <- dbUpdate $ MakeUserIDAdminForPartnerID (userid user) pid
  assertEqual "MakeUserIDAdminForPartnerID result is True" True result1
  -- set the company partner
  result2 <- dbUpdate$ SetCompanyInfo cid (companyinfo c1) { companypartnerid = pid }
  assertEqual "Set company partner result is True" True result2
  -- convert everything to usergroups
  numberOfUpdates <- migrateToUserGroups 100
  assertEqual "Only the company, its partner and admin's company were migrated" 3 numberOfUpdates
  c2 <- fromJust <$> (dbQuery $ GetCompany cid)
  let ugid = fromJust . companyusergroupid $ c2

  ug0_fromCompany <- userGroupFromCompany cid
  ug0 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company0" ug0 ug0_fromCompany

  -- apply random change (but do not change partner)
  result3 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = companypartnerid . companyinfo $ c2 }
  assertEqual "SetCompanyInfo result is True" True result3

  ug1_fromCompany <- userGroupFromCompany cid
  ug1 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company1" ug1 ug1_fromCompany

  -- apply PaymentPlan change
  result4 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyInfo result is True" True result4

  ug2_fromCompany <- userGroupFromCompany cid
  ug2 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company2" ug2 ug2_fromCompany

  -- apply UI change
  -- this is just change, which remove all themes. But there are some themes set now, so it tests the update.
  serviceThemeFromDomain <- dbQuery $ GetTheme (get (bdServiceTheme . ctxbrandeddomain) ctx)
  serviceTheme <- dbUpdate $ InsertNewThemeForCompany cid serviceThemeFromDomain
  result5 <- randomUpdate $ \cui' -> SetCompanyUI cid cui' {
      companyuicompanyid  = cid
    , companyServiceTheme = Just $ themeID serviceTheme
    }
  assertEqual "SetCompanyUI result is True" True result5

  ug3_fromCompany <- userGroupFromCompany cid
  ug3 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company3" ug3 ug3_fromCompany

testUpdateOfCompanyDoesTheSameToItsGroup :: TestEnv ()
testUpdateOfCompanyDoesTheSameToItsGroup = replicateM_ 20 $ do
  -- create company
  Company{companyid = cid} <- dbUpdate $ CreateCompanyWithoutUserGroup
  -- setup some themes
  ctx <- mkContext def
  mailThemeFromDomain <- dbQuery . GetTheme . get (bdMailTheme . ctxbrandeddomain) $ ctx
  mailTheme <- dbUpdate $ InsertNewThemeForCompany cid mailThemeFromDomain
  signviewThemeFromDomain <- dbQuery $ GetTheme (get (bdSignviewTheme . ctxbrandeddomain) ctx)
  signviewTheme <- dbUpdate $ InsertNewThemeForCompany cid signviewThemeFromDomain
  cui <- dbQuery $ GetCompanyUI cid
  result0 <- dbUpdate $ SetCompanyUI cid cui {
      companyMailTheme     = Just $ themeID mailTheme
    , companySignviewTheme = Just $ themeID signviewTheme
    }
  assertEqual "GetCompanyUI result is True" True result0
  -- convert everything to usergroups
  numberOfUpdates <- migrateToUserGroups 100
  assertEqual "Only the single company was migrated" 1 numberOfUpdates
  c <- fromJust <$> (dbQuery $ GetCompany cid)
  let ugid = fromJust . companyusergroupid $ c

  -- apply random change (but do not change partner)
  result1 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = companypartnerid . companyinfo $ c }
  assertEqual "SetCompanyInfo result is True" True result1

  ug1_fromCompany <- userGroupFromCompany cid
  ug1 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company" ug1 ug1_fromCompany

  -- apply another random change
  result2 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyInfo result is True" True result2

  ug2_fromCompany <- userGroupFromCompany cid
  ug2 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company" ug2 ug2_fromCompany

  -- apply UI change
  serviceThemeFromDomain <- dbQuery $ GetTheme (get (bdServiceTheme . ctxbrandeddomain) ctx)
  serviceTheme <- dbUpdate $ InsertNewThemeForCompany cid serviceThemeFromDomain
  result3 <- randomUpdate $ \cui' -> SetCompanyUI cid cui' {
      companyuicompanyid  = cid
    , companyServiceTheme = Just $ themeID serviceTheme
    }
  assertEqual "SetCompanyUI result is True" True result3

  ug3_fromCompany <- userGroupFromCompany cid
  ug3 <- fromJust <$> (dbQuery $ UserGroupGet ugid)
  assertEqual "UserGroup from DB is the same as UserGroup from company3" ug3 ug3_fromCompany

testCanUpdatePartnerCompanyWithoutUserGroup :: TestEnv ()
testCanUpdatePartnerCompanyWithoutUserGroup = replicateM_ 20 $ do
  -- create company
  c@Company{companyid = cid} <- dbUpdate $ CreateCompanyWithoutUserGroup
  -- create partner
  user <- addNewRandomUserAndMaybeUserGroup False
  pid <- dbUpdate $ AddNewPartner "My Favourite Upsales"
  result1 <- dbUpdate $ MakeUserIDAdminForPartnerID (userid user) pid
  assertEqual "MakeUserIDAdminForPartnerID result is True" True result1
  -- set the company partner
  result2 <- dbUpdate$ SetCompanyInfo cid (companyinfo c) { companypartnerid = pid }
  assertEqual "Set company partner result is True" True result2

  result3 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = pid }
  assertEqual "SetCompanyInfo result is True" True result3

  result4 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyPaymentPlan result is True" True result4

testCanUpdatePartnerCompanyWithUserGroup :: TestEnv ()
testCanUpdatePartnerCompanyWithUserGroup = replicateM_ 20 $ do
  -- create company
  c@Company{companyid = cid} <- dbUpdate $ CreateCompanyWithoutUserGroup
  -- create partner
  user <- addNewRandomUserAndMaybeUserGroup False
  pid <- dbUpdate $ AddNewPartner "My Favourite Upsales"
  result1 <- dbUpdate $ MakeUserIDAdminForPartnerID (userid user) pid
  assertEqual "MakeUserIDAdminForPartnerID result is True" True result1
  -- set the company partner
  result2 <- dbUpdate$ SetCompanyInfo cid (companyinfo c) { companypartnerid = pid }
  assertEqual "Set company partner result is True" True result2
  -- convert everything to user_groups
  numberOfUpdates <- migrateToUserGroups 100
  assertEqual "Only the company, its partner and admin's company were migrated" 3 numberOfUpdates

  result3 <- randomUpdate $ \ci -> SetCompanyInfo cid ci { companypartnerid = pid }
  assertEqual "SetCompanyInfo result is True" True result3

  result4 <- randomUpdate $ \pp -> SetCompanyPaymentPlan cid pp
  assertEqual "SetCompanyPaymentPlan result is True" True result4

testCannotDeleteUserGroupWithSubgroups :: TestEnv ()
testCannotDeleteUserGroupWithSubgroups = do
  ugA0 <- unARootUserGroup <$> rand 10 arbitrary
  ugA <- dbUpdate . UserGroupCreate $ ugA0
  -- make B child of A
  ugB0 <- rand 10 arbitrary
  ugB <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just . get ugID $ ugA) $ ugB0
  commit -- so that raising exception will not take our data with it
  -- deleting group A raises exception
  assertRaisesDBException . runSQL_ $ "DELETE from user_groups where id = " <?> get ugID ugA
  commit
  -- deleting group B works
  runSQL_ $ "DELETE from user_groups where id = " <?> get ugID ugB

userGroupFromCompany :: (MonadDB m, MonadThrow m, MonadLog m) => CompanyID -> m UserGroup
userGroupFromCompany cid = do
  c <- fromJust <$> dbQuery (GetCompany cid)
  cui <- dbQuery $ GetCompanyUI cid
  p <- dbQuery . GetPartnerByID . companypartnerid . companyinfo $ c
  case ptDefaultPartner p of
    True  -> return $ companyToUserGroup c cui Nothing
    False -> return $ companyToUserGroup c cui (ptUserGroupID p)

newtype ARootUserGroup = ARootUserGroup { unARootUserGroup :: UserGroup }

instance Arbitrary ARootUserGroup where
  arbitrary = (\ug pp ui address info -> ARootUserGroup $ ug {
      _ugInvoicing = Invoice pp
    , _ugUI        = ui
    , _ugInfo      = info
    , _ugAddress   = address
    })
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
