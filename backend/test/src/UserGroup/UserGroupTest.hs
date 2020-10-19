module UserGroup.UserGroupTest (userGroupTests) where

import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Writer
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.Int
import Data.Unjson (unjsonDef, unjsonToJSON)
import Happstack.Server hiding (result)
import Test.Framework
import Test.QuickCheck
import qualified Data.Aeson as A

import Administration.AdministrationControl
  ( CompanyCreateResult(groupId, mRootGroupId), handleCompanyChange
  , handleCompanyCreate
  )
import API.V2.Errors
import Context
import DB
import Doc.SignatoryLinkID ()
import TestingUtil
import TestKontra as T
import User.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan

userGroupTests :: TestEnvSt -> Test
userGroupTests env = testGroup
  "UserGroup"
  [ testThat "Creating groups" env testCreateGroups
  , testThat "Creating groups requires admin access" env testGroupCreateRequiresAdmin
  , testGroup "Creating groups using API" $ testCreateGroupsUsingApi env
  , testThat "Creating groups with users"           env testCreateGroupsWithUsers
  , testThat "Fetching all users of group"          env testGetAllUsersOfTopGroup
  , testThat "Fetching user group"                  env testGetUserGroup
  , testThat "Fetching all groups of user"          env testGetAllGroupsOfUser
  , testThat "Updating group requires admin access" env testGroupChangeRequiresAdmin
  , testThat "Moving a group succeeds"              env testMoveGroup
  , testThat "Moving a group cannot form a cycle"   env testMoveGroupCycleError
  , testThat "Moving a group with billable flag is forbidden"
             env
             testMoveBillableGroupForbidden
  , testThat "Find a parent group, where charging happens" env testFindInheritedPricePlan
  , testThat "Cannot delete a UserGroup with subgroups"
             env
             testCannotDeleteUserGroupWithSubgroups
  , testThat "Setting user group parents"         env testChangeUserGroupParent
  , testThat "User group root must have invoice"  env testUserGroupRootMustHaveInvoice
  , testThat "User group root must have address"  env testUserGroupRootMustHaveAddress
  , testThat "User group root must have settings" env testUserGroupRootMustHaveSettings
  , testThat
    "User group invoicing is set to none when moving root without billable flag"
    env
    testUserGroupMoveRootWithoutSubscriptionSetsInvoicing
  , testThat "User group tags" env testTags
  ]


testCreateGroups :: TestEnv ()
testCreateGroups = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroups [Nothing] [1 .. 5]
  runQuery_ ("SELECT COUNT(*) FROM user_groups" :: SQL)
  groupcount <- fetchOne runIdentity
  assertEqual "Created tree of groups" (31 :: Int64) groupcount
  where
    createGroups :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroups mparent_ids level = (concat <$>) . forM mparent_ids $ \mparent_id -> do
      ug0 <- case level of
        1 -> ugFromUGRoot <$> rand 10 arbitrary
        _ -> rand 10 arbitrary
      ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
      return . replicate 2 . Just $ ug1 ^. #id


testGroupCreateRequiresAdmin :: TestEnv ()
testGroupCreateRequiresAdmin = do
  user <- instantiateRandomUser
  let email = user ^. #info % #email
  ctxNoAdmin <- mkContextWithUser defaultLang user
  req        <- mkRequest
    POST
    [ ("user_group_name", inText "I want to see your administrator!")
    , ("payment_plan"   , inText "free")
    ]

  -- User has no admin rights so we should get a `Respond404`.
  assertRaises404 . void $ runTestKontra req ctxNoAdmin handleCompanyCreate

  -- User gets admin rights.
  let ctxAdmin = set #adminAccounts [email] ctxNoAdmin
  (result, _) <- runTestKontra req ctxAdmin handleCompanyCreate

  -- Check that user group exists
  void . getUserGroup $ groupId result


testCreateGroupsUsingApi :: TestEnvSt -> [Test]
testCreateGroupsUsingApi env = execWriter $ do
  testThat' "Create free user group" $ do
    ctx      <- mkAdminContex
    req      <- mkCreateRequest (Just FreePlan) Nothing Nothing
    response <- callCreate req ctx
    assertNothing "No root group was returned for free group" $ mRootGroupId response
    -- Check that user group exists
    ug <- getUserGroup $ groupId response
    assertNothing "Free user group has no parent" $ ug ^. #parentGroupID
    assertBool "Free user group is not billable" $ not (ug ^. #isBillable)
    childrenUgs <- dbQuery . UserGroupGetImmediateChildren $ groupId response
    assertEqual "Free user group has no children" [] childrenUgs
    assertEqual "Free user group invoicing is Invoice with FreePlan"
                (Invoice FreePlan)
                (ug ^. #invoicing)

  withPaidPlans $ \paymentPlan ->
    testThat' ("Create paid user group with " <> show paymentPlan) $ do
      ctx         <- mkAdminContex
      req         <- mkCreateRequest (Just paymentPlan) Nothing Nothing
      response    <- callCreate req ctx
      rootGroupId <- assertJust "Paid group was created with root group"
        $ mRootGroupId response
      -- Check that user groups exists
      ug     <- getUserGroup $ groupId response
      rootUg <- getUserGroup rootGroupId
      assertBool "User group is not billable" $ not (ug ^. #isBillable)
      assertBool "Root user group is billable" (rootUg ^. #isBillable)
      assertEqual "User group has root user group as parent"
                  (ug ^. #parentGroupID)
                  (Just $ rootUg ^. #id)
      childrenUgs <- dbQuery . UserGroupGetImmediateChildren $ groupId response
      assertEqual "User group has no children" childrenUgs []
      rootChildrenUgs <- dbQuery (UserGroupGetImmediateChildren rootGroupId)
      assertEqual "Root user group has user group as child"
                  (view #id <$> rootChildrenUgs)
                  [ug ^. #id]
      assertEqual ("Root user group invoicing is Invoice with " <> show paymentPlan)
                  (Invoice paymentPlan)
                  (rootUg ^. #invoicing)
      assertEqual "User group invoicing is None" None (ug ^. #invoicing)

  testThat'
      "Create user group with no parent_group_id, child_group_id and no payment_plan fails"
    $ do
        ctx <- mkAdminContex
        req <- mkCreateRequest Nothing Nothing Nothing
        assertRaisesApiErrorType RequestFailed $ callCreate req ctx

  withPaidPlans $ \paymentPlan ->
    testThat' ("Upgrade free user group to paid with " <> show paymentPlan) $ do
      ctx      <- mkAdminContex
      freeUg   <- instantiateRandomFreeUserGroup
      req      <- mkCreateRequest (Just paymentPlan) Nothing $ Just (freeUg ^. #id)
      response <- callCreate req ctx
      assertEqual "User group ids are same" (freeUg ^. #id) (groupId response)
      rootGroupId <- assertJust "Group was upgraded with root group"
        $ mRootGroupId response
      -- Check that user groups exists
      ug     <- getUserGroup $ groupId response
      rootUg <- getUserGroup rootGroupId
      assertBool "User group is not billable" $ not (ug ^. #isBillable)
      assertBool "Root user group is billable" (rootUg ^. #isBillable)
      assertEqual "User group has root user group as parent"
                  (Just $ rootUg ^. #id)
                  (ug ^. #parentGroupID)
      childrenUgs <- dbQuery . UserGroupGetImmediateChildren $ groupId response
      assertEqual "User group has no children" [] childrenUgs
      rootChildrenUgs <- dbQuery (UserGroupGetImmediateChildren rootGroupId)
      assertEqual "Root user group has user group as child"
                  [ug ^. #id]
                  (view #id <$> rootChildrenUgs)
      assertEqual "User group invoicing is None" (ug ^. #invoicing) None
      assertEqual ("Root user group invoicing is Invoice with " <> show paymentPlan)
                  (Invoice paymentPlan)
                  (rootUg ^. #invoicing)

  forM_ [Nothing, Just FreePlan] $ \mPaymentPlan ->
    testThat' ("Upgrade user group with fails with " <> show mPaymentPlan) $ do
      ctx      <- mkAdminContex
      parentUg <- instantiateRandomFreeUserGroup
      req      <- mkCreateRequest mPaymentPlan Nothing $ Just (parentUg ^. #id)
      assertRaisesApiErrorType RequestFailed $ callCreate req ctx

  testThat' "Creating child user group for paid group succeeds" $ do
    ctx           <- mkAdminContex
    (parentUg, _) <- instantiateRandomPaidUserGroup
    req           <- mkCreateRequest Nothing (Just $ parentUg ^. #id) Nothing
    response      <- callCreate req ctx
    -- Check that user group exists
    ug            <- getUserGroup $ groupId response
    assertBool "User group is not billable" $ not (ug ^. #isBillable)
    assertEqual "User group has parent user group as parent"
                (Just $ parentUg ^. #id)
                (ug ^. #parentGroupID)
    childrenUgs <- dbQuery . UserGroupGetImmediateChildren $ groupId response
    assertEqual "User group has no children"   []                 childrenUgs
    assertEqual "User group invoicing is None" (ug ^. #invoicing) None

  testThat' "Creating child user group for paid group's child succeeds" $ do
    ctx           <- mkAdminContex
    (_, parentUg) <- instantiateRandomPaidUserGroup
    req           <- mkCreateRequest Nothing (Just $ parentUg ^. #id) Nothing
    response      <- callCreate req ctx
    -- Check that user group exists
    ug            <- getUserGroup $ groupId response
    assertBool "User group is not billable" $ not (ug ^. #isBillable)
    assertEqual "User group has parent user group as parent"
                (Just $ parentUg ^. #id)
                (ug ^. #parentGroupID)
    childrenUgs <- dbQuery . UserGroupGetImmediateChildren $ groupId response
    assertEqual "User group has no children"   []                 childrenUgs
    assertEqual "User group invoicing is None" (ug ^. #invoicing) None

  testThat' "Creating child user group for free group fails" $ do
    ctx      <- mkAdminContex
    parentUg <- instantiateRandomFreeUserGroup
    req      <- mkCreateRequest Nothing (Just $ parentUg ^. #id) Nothing
    assertRaisesApiErrorType RequestFailed $ callCreate req ctx

  forM_ [PaidPlan, TrialPlan, FreePlan] $ \paymentPlan ->
    testThat' ("Creating child user group fails with " <> show paymentPlan) $ do
      ctx           <- mkAdminContex
      (parentUg, _) <- instantiateRandomPaidUserGroup
      req           <- mkCreateRequest (Just paymentPlan) (Just $ parentUg ^. #id) Nothing
      assertRaisesApiErrorType RequestFailed $ callCreate req ctx
  where
    testThat' :: String -> TestEnv () -> WriterT [Test] Identity ()
    testThat' what = tell . (: []) . testThat what env

    mkCreateRequest
      :: Maybe PaymentPlan -> Maybe UserGroupID -> Maybe UserGroupID -> TestEnv Request
    mkCreateRequest mPaymentPlan mParentId mChildId = do
      groupName <- liftBase $ generate arbitraryName
      mkRequest POST $ mapMaybe
        identity
        [ Just $ mkPair "user_group_name" groupName
        , mkPair "payment_plan" . (unString . unjsonToJSON unjsonDef) <$> mPaymentPlan
        , mkPair "user_group_parent_id" . showt <$> mParentId
        , mkPair "user_group_child_id" . showt <$> mChildId
        ]

    withPaidPlans = forM_ [PaidPlan, TrialPlan]

    mkPair name = (name, ) . inText

    unString = \case
      A.String text -> text
      _             -> undefined

    callCreate req ctx = do
      (response, _) <- runTestKontra req ctx handleCompanyCreate
      pure response


testCreateGroupsWithUsers :: TestEnv ()
testCreateGroupsWithUsers = do
  -- create a 5 level tree, each parent 2 subgroups
  void $ foldlM createGroupsWithUsers [Nothing] [1 .. 5]
  runQuery_ ("SELECT COUNT(*) FROM users" :: SQL)
  usercount <- fetchOne runIdentity
  assertEqual "Created tree of groups and users" (31 :: Int64) usercount
  where
    createGroupsWithUsers :: [Maybe UserGroupID] -> Int -> TestEnv [Maybe UserGroupID]
    createGroupsWithUsers mparent_ids level =
      (concat <$>) . forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- instantiateRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return . replicate 2 . Just $ ug1 ^. #id


testGetAllUsersOfTopGroup :: TestEnv ()
testGetAllUsersOfTopGroup = do
  -- create a 5 level tree, each parent 2 subgroups
  (_, ugidtop : _) <- foldlM createGroupsWithUsers ([Nothing], []) [1 .. 5]
  dus              <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ ugidtop
  assertEqual "Fetched all users of top group" 31 (length dus)
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
        u   <- instantiateRandomUser
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
  (lastuid : _) <- reverse . snd <$> foldlM createGroupsWithUsers ([Nothing], []) [1 .. 5]
  ugwp          <- dbQuery . UserGroupGetWithParentsByUserID $ lastuid
  assertEqual "Fetched all groups of user" 5 (length $ ugwpToList ugwp)
  where
    createGroupsWithUsers
      :: ([Maybe UserGroupID], [UserID]) -> Int -> TestEnv ([Maybe UserGroupID], [UserID])
    createGroupsWithUsers (mparent_ids, uids) level = do
      (new_ugids, new_uids) <- (unzip <$>) . forM mparent_ids $ \mparent_id -> do
        ug0 <- case level of
          1 -> ugFromUGRoot <$> rand 10 arbitrary
          _ -> rand 10 arbitrary
        ug1 <- dbUpdate . UserGroupCreate . set #parentGroupID mparent_id $ ug0
        u   <- instantiateRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return (ug1 ^. #id, u ^. #id)
      --      parents for groups in next level , all userids created
      return (Just <$> (new_ugids ++ new_ugids), uids ++ new_uids)


testGroupChangeRequiresAdmin :: TestEnv ()
testGroupChangeRequiresAdmin = do
  user <- instantiateRandomUser
  let groupId = user ^. #groupID
      email   = user ^. #info % #email
  ctxNoAdmin <- mkContextWithUser defaultLang user
  req        <- mkRequest POST []

  -- User has no admin rights so we should get a `Respond404`.
  assertRaises404 . void . runTestKontra req ctxNoAdmin $ handleCompanyChange groupId

  -- User gets admin rights.
  let ctxAdmin = set #adminAccounts [email] ctxNoAdmin
  void . runTestKontra req ctxAdmin $ handleCompanyChange groupId


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
  void . dbUpdate . UserGroupUpdate . set #parentGroupID (Just $ ugids !! 14) $ ug1
  -- parentpath of new leaf in moved group should be 6 items long
  Just (_ug10, parentugs) <- dbQuery . UserGroupGetWithParents $ ugids !! 10
  assertEqual "Fetched all parents of leaf group" 6 (length parentugs)
  -- root still has 15 users
  us <- dbQuery . UserGroupGetAllUsersFromThisAndSubgroups $ head ugids
  assertEqual "Fetched all users of top group" 15 (length us)
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
        u   <- instantiateRandomUser
        void . dbUpdate $ SetUserGroup (u ^. #id) (Just $ ug1 ^. #id)
        return $ ug1 ^. #id
      --      parents for groups in next level , all group ids created
      return (Just <$> concatMap (replicate 2) new_ugids, ugids ++ new_ugids)


testMoveGroupCycleError :: TestEnv ()
testMoveGroupCycleError = do
  (_rootGroup, childGroup) <- instantiateRandomPaidUserGroup
  childGroup'              <- instantiateUserGroup randomUserGroupTemplate
    { parentGroupID = Just $ childGroup ^. #id
    }
  childGroup'' <- instantiateUserGroup randomUserGroupTemplate
    { parentGroupID = Just $ childGroup' ^. #id
    }

  ctx <- mkAdminContex

  -- Creating a loop in group hierarchy is not allowed.
  -- The two commented test would fail with `UserGroupMoveWithSubscriptionForbidden`.
  -- assertLoopFails (ugFromUGRoot _rootGroup) childGroup
  -- assertLoopFails (ugFromUGRoot _rootGroup) childGroup'
  assertLoopFails ctx childGroup  childGroup'
  assertLoopFails ctx childGroup  childGroup''
  assertLoopFails ctx childGroup' childGroup''
  where
    assertLoopFails ctx parentGroup childGroup = do
      req <- mkRequest POST [("companyparentid", inText . showt $ childGroup ^. #id)]
      assertRaisesKontra (\(UserGroupsFormCycle _) -> True)
        . void
        . runTestKontra req ctx
        $ handleCompanyChange (parentGroup ^. #id)
      parentGroup' <- getUserGroup $ parentGroup ^. #id
      assertEqual "Parent of group was not changed"
                  (parentGroup ^. #parentGroupID)
                  (parentGroup' ^. #parentGroupID)


testMoveBillableGroupForbidden :: TestEnv ()
testMoveBillableGroupForbidden = do
  (movedRootGroup, _         ) <- instantiateRandomPaidUserGroup
  (rootGroup     , childGroup) <- instantiateRandomPaidUserGroup
  freeGroup                    <- instantiateRandomFreeUserGroup

  ctx                          <- mkAdminContex

  -- Can't move billable user group into billable group
  testMoveIntoGroup ctx movedRootGroup rootGroup

  -- Can't move billable user group into child group of billable group
  testMoveIntoGroup ctx movedRootGroup childGroup

  -- Can't move billable user group into root group without billable flag
  testMoveIntoGroup ctx movedRootGroup freeGroup

  where
    testMoveIntoGroup ctx movedGroup destinationGroup = do
      req <- mkRequest POST
                       [("companyparentid", inText . showt $ destinationGroup ^. #id)]
      assertRaisesApiErrorType RequestFailed
        . void
        . runTestKontra req ctx
        $ handleCompanyChange (movedGroup ^. #id)
      movedGroup' <- getUserGroup $ movedGroup ^. #id
      assertEqual "Parent of group was not changed"
                  Nothing
                  (movedGroup' ^. #parentGroupID)



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
  ug                     <- instantiateRandomFreeUserGroup
  parentUg               <- instantiateRandomFreeUserGroup
  (_, grandParentUsrGrp) <- instantiateRandomPaidUserGroup
  let ugId            = ug ^. #id
      parentUgId      = parentUg ^. #id
      grandParentUgId = grandParentUsrGrp ^. #id
  ctxAdmin <- mkAdminContex

  -- Group without billable flag (free group) can't be parent.
  do
    req <- mkParentRequest parentUgId
    assertParent "User group has no parent" ugId Nothing
    assertRaisesApiErrorType RequestFailed
      . void
      . runTestKontra req ctxAdmin
      $ handleCompanyChange ugId
    assertParent "Parent user group without valid root was not set" ugId Nothing

  -- But when group without billable flag is part of structure with flagged root
  -- it can become parent.
  do
    req <- mkParentRequest grandParentUgId
    void . runTestKontra req ctxAdmin $ handleCompanyChange parentUgId
    assertParent "User group parent has been set correctly" parentUgId
      $ Just grandParentUgId

  -- Setting a parent that already has a parent should work.
  do
    req <- mkParentRequest parentUgId
    void . runTestKontra req ctxAdmin $ handleCompanyChange ugId
    assertParent "User group parent that has parent has been set correctly" ugId
      $ Just parentUgId

  -- User group can't become root once becoming part of hierarchy
  do
    req <- mkRequest POST [("companyparentid", inText "")]
    assertParent "User group has parent" ugId $ Just parentUgId
    assertRaisesApiErrorType RequestFailed
      . void
      . runTestKontra req ctxAdmin
      $ handleCompanyChange ugId
    assertParent "User group parent has not been removed" ugId $ Just parentUgId
  where
    mkParentRequest :: UserGroupID -> TestEnv Request
    mkParentRequest parentGroupID =
      mkRequest POST [("companyparentid", inText $ showt parentGroupID)]

    assertParent :: String -> UserGroupID -> Maybe UserGroupID -> TestEnv ()
    assertParent assertMsg userGroupId expectedParentId = do
      ugParent <- getUserGroup userGroupId
      assertEqual assertMsg expectedParentId (ugParent ^. #parentGroupID)


testUserGroupRootMustHaveInvoice :: TestEnv ()
testUserGroupRootMustHaveInvoice = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root with ugInvoicing other than Invoice fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupCreate
    . set #invoicing None
    $ ug0
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupCreate
    . set #invoicing (BillItem Nothing)
    $ ug0
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupCreate
    . set #invoicing (BillItem (Just FreePlan))
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- updating a root with ugInvoicing other than Invoice fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing None
    $ ug1
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing (BillItem Nothing)
    $ ug1
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupUpdate
    . set #invoicing (BillItem (Just FreePlan))
    $ ug1


testUserGroupRootMustHaveAddress :: TestEnv ()
testUserGroupRootMustHaveAddress = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root without address fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupCreate
    . set #address Nothing
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- clearing address from root fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupUpdate
    . set #address Nothing
    $ ug1


testUserGroupRootMustHaveSettings :: TestEnv ()
testUserGroupRootMustHaveSettings = do
  ug0 <- ugFromUGRoot <$> rand 10 arbitrary
  -- creating a root without settings fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupCreate
    . set #settings Nothing
    $ ug0

  ug1 <- dbUpdate . UserGroupCreate $ ug0
  -- clearing settings from root fails
  assertRaisesKontra invalidAsRootException
    . dbUpdate
    . UserGroupUpdate
    . set #settings Nothing
    $ ug1

testUserGroupMoveRootWithoutSubscriptionSetsInvoicing :: TestEnv ()
testUserGroupMoveRootWithoutSubscriptionSetsInvoicing = do
  (rootUg, _) <- instantiateRandomPaidUserGroup
  freeUg      <- instantiateRandomFreeUserGroup

  do
    freeUg' <- getUserGroup $ freeUg ^. #id
    assertBool "Free user group is root" . isNothing $ freeUg' ^. #parentGroupID
    assertBool "Free user group invoicing is Invoice" . isInvoice $ freeUg' ^. #invoicing

  ctx <- mkAdminContex
  req <- mkRequest POST [("companyparentid", inText . showt $ rootUg ^. #id)]
  void . runTestKontra req ctx . handleCompanyChange $ freeUg ^. #id

  do
    freeUg' <- getUserGroup $ freeUg ^. #id
    assertEqual "Free user group has been moved"
                (freeUg' ^. #parentGroupID)
                (Just $ rootUg ^. #id)
    assertBool "Free user group invoicing is None" . isNone $ freeUg' ^. #invoicing

  where
    isInvoice = \case
      Invoice _ -> True
      _         -> False

    isNone = \case
      None -> True
      _    -> False


testTags :: TestEnv ()
testTags = do
  ug         <- instantiateRandomFreeUserGroup
  Just ugRes <- dbQuery $ UserGroupGet (ug ^. #id)
  assertEqual "Internal tags match after create"
              (ug ^. #internalTags)
              (ugRes ^. #internalTags)
  assertEqual "External tags match after create"
              (ug ^. #externalTags)
              (ugRes ^. #externalTags)

  iTags2 <- rand 10 arbitrary
  eTags2 <- rand 10 arbitrary
  _      <-
    dbUpdate . UserGroupUpdate . set #internalTags iTags2 . set #externalTags eTags2 $ ug
  Just ugRes2 <- dbQuery $ UserGroupGet (ug ^. #id)
  assertEqual "Internal tags match after update" iTags2 (ugRes2 ^. #internalTags)
  assertEqual "External tags match after update" eTags2 (ugRes2 ^. #externalTags)


mkAdminContex :: TestEnv Context
mkAdminContex = do
  user <- instantiateRandomUser
  mkContextWithUser defaultLang user <&> set #adminAccounts [user ^. #info % #email]


getUserGroup :: UserGroupID -> TestEnv UserGroup
getUserGroup = assertJust' <=< dbQuery . UserGroupGet


invalidAsRootException :: UserGroupInvalidAsRoot -> Bool
invalidAsRootException = \case
  UserGroupInvalidAsRoot _ _ -> True
