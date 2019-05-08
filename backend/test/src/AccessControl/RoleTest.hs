{-# LANGUAGE OverloadedStrings #-}
module AccessControl.RoleTest (accessControlRoleTests) where

--import Data.Aeson
import Happstack.Server
import Test.Framework
import Test.QuickCheck

--import AccessControl.API
import AccessControl.Model
import AccessControl.Types
import Context
import DB
import TestingUtil
import TestKontra
import User.Model
import UserGroup.Model
import UserGroup.Types

accessControlRoleTests :: TestEnvSt -> Test
accessControlRoleTests env = testGroup "AccessControlRoles"
  [ testThat "User's roles do not trickle down the user group tree"
      env testRolesNotInheritedInUserGroupTree
  ]

testRolesNotInheritedInUserGroupTree :: TestEnv ()
testRolesNotInheritedInUserGroupTree = do
  muser@(Just user) <- addNewUser "Lloyd" "Garmadon" "lloyd.garmadon@scrive.com"
  _ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  _req   <- mkRequest GET []
--  _res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid)
  root_ug <- rand 10 arbitrary
  root_ugid <- (get ugID) <$> (dbUpdate . UserGroupCreate $ root_ug)
  [_ug0, ug1] <- createChildGroups root_ugid
  void $ dbUpdate $ SetUserGroup (userid user) (Just $ get ugID ug1)
  userRoles0 <- dbQuery $ GetRoles user
  let roleTrg = UserGroupAdminAR root_ugid
  void $ dbUpdate $ AccessControlInsertRoleTrgForUserGroup root_ugid roleTrg
  userRoles1 <- dbQuery $ GetRoles user
  -- chk that user does _not_ have the role inherited
  assertEqual "asdf" userRoles0 userRoles1
  where
    createChildGroups :: UserGroupID -> TestEnv [UserGroup]
    createChildGroups root_ugid' = do
      ugrand0 <- rand 10 arbitrary
      ugrand1 <- rand 10 arbitrary
      ug0 <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just root_ugid') $ ugrand0
      ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just (get ugID ug0)) $ ugrand1
      return $ [ug0, ug1]
   
