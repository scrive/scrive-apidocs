{-# LANGUAGE OverloadedStrings #-}
module AccessControl.RoleTest (accessControlRoleTests) where

import Test.Framework
import Test.QuickCheck

import AccessControl.Model
import AccessControl.Types
import DB
import TestingUtil
import TestKontra
import User.Model
import UserGroup.Model
import UserGroup.Types

accessControlRoleTests :: TestEnvSt -> Test
accessControlRoleTests env = testGroup
  "AccessControlRoles"
  [ testThat "User's roles do not trickle down the user group tree"
             env
             testRolesNotInheritedInUserGroupTree
  ]

testRolesNotInheritedInUserGroupTree :: TestEnv ()
testRolesNotInheritedInUserGroupTree = do
  (Just user) <- addNewUser "Lloyd" "Garmadon" "lloyd.garmadon@scrive.com"
  let uid = userid user
  (root_ug :: UserGroupRoot) <- rand 10 arbitrary
  root_ugid <- (get ugID) <$> (dbUpdate . UserGroupCreate $ ugFromUGRoot root_ug)
  [_ug0, ug1] <- createChildGroups root_ugid
  void $ dbUpdate $ SetUserGroup uid (Just $ get ugID ug1)
  -- user's group changed, need to re-retrieve the user
  (Just user') <- dbQuery $ GetUserByID uid
  let role_trg = UserGroupAdminAR root_ugid
  void $ dbUpdate $ AccessControlCreateForUserGroup root_ugid role_trg
  userRoles1 <- dbQuery $ GetRoles user'
  [grp_role] <- dbQuery $ AccessControlGetRolesByUserGroup root_ugid
  assertBool "The role set on a parent group is not included in user's roles"
             (not $ grp_role `elem` userRoles1)

  void $ dbUpdate $ SetUserGroup (userid user) (Just $ root_ugid)
  -- user's group changed, need to re-retrieve the user
  (Just user'') <- dbQuery $ GetUserByID uid
  userRoles2    <- dbQuery $ GetRoles user''
  assertBool "The role set on the user's group is indeed included in user's roles"
             (grp_role `elem` userRoles2)
  where
    createChildGroups :: UserGroupID -> TestEnv [UserGroup]
    createChildGroups root_ugid' = do
      ugrand0 <- rand 10 arbitrary
      ugrand1 <- rand 10 arbitrary
      ug0 <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just root_ugid') $ ugrand0
      ug1     <-
        dbUpdate . UserGroupCreate . set ugParentGroupID (Just (get ugID ug0)) $ ugrand1
      return $ [ug0, ug1]
