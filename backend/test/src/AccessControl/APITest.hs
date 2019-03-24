{-# LANGUAGE OverloadedStrings #-}
module AccessControl.APITest (accessControlApiTests) where

import Happstack.Server
import Test.Framework

import AccessControl.API
import AccessControl.Model
import Context
import DB
import TestingUtil
import TestKontra
import User.Model
import UserGroup.Types

accessControlApiTests :: TestEnvSt -> Test
accessControlApiTests env = testGroup "AccessControlAPI"
  [ -- UserGroup getuserroles tests
    testThat "non-admin and non-sales user can't view non-existent User's roles"
      env testNonAdminUserCannotViewRolesForNonExistentUser
  , testThat "non-admin and non-sales user can't view User's roles without permissions"
      env testNonAdminUserCannotViewUserRolesWithoutPermissions
  , testThat "non-admin and non-sales user can view own roles"
      env testNonAdminUserCanViewOwnRoles
  , testThat "usergroup admin can view roles of other usergroup member"
      env testUserGroupAdminCanViewRolesOfOtherUserGroupMember
  -- This actually gives a 200 - Should it?
  -- , testThat "usergroup member cannot view roles of other usergroup members"
  --     env testUserGroupMemberCannotViewRolesOfOtherUserGroupMembers
  ]

-- AccessControl getuserroles tests

testNonAdminUserCannotViewRolesForNonExistentUser :: TestEnv ()
testNonAdminUserCannotViewRolesForNonExistentUser = do
  muser <- addNewUser "Dave" "Lister" "dave.lister@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid)
  assertEqual "non-admin user can't view non-existent user's roles" 403 $ rsCode res
    where
      uid = unsafeUserID 123

testNonAdminUserCannotViewUserRolesWithoutPermissions :: TestEnv ()
testNonAdminUserCannotViewUserRolesWithoutPermissions = do
  muser <- addNewUser "Dave" "Lister" "dave.lister@scrive.com"
  uid2  <- userid . fromJust <$> addNewUser "Arnold" "Rimmer" "arnold.rimmer@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid2)
  assertEqual "non-admin user can't view user's roles without permission" 403 $ rsCode res

testNonAdminUserCanViewOwnRoles :: TestEnv ()
testNonAdminUserCanViewOwnRoles = do
  muser <- addNewUser "The" "Cat" "the.cat@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  let uid = userid $ fromJust muser
  res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid)
  assertEqual "non-admin user can view own roles" 200 $ rsCode res

testUserGroupAdminCanViewRolesOfOtherUserGroupMember :: TestEnv ()
testUserGroupAdminCanViewRolesOfOtherUserGroupMember = do
  (user, ug) <- addNewAdminUserAndUserGroup "Captain" "Hollister" emailAddress
  let uid1 = userid user
      ugid = get ugID ug
  void . dbUpdate $ AccessControlInsertUserGroupAdmin uid1 ugid
  user2 <- fromJust <$> addNewUserToUserGroup "Dwayne" "Dibley" emailAddress2 ugid
  let uid2 = userid user2
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid2)
  assertEqual "" 200 $ rsCode res
    where
      emailAddress  = "captain.hollister@scrive.com"
      emailAddress2 = "dwayne.dibley@scrive.com"

-- This actually gives a 200 - Should it?
-- testUserGroupMemberCannotViewRolesOfOtherUserGroupMembers :: TestEnv ()
-- testUserGroupMemberCannotViewRolesOfOtherUserGroupMembers = do
--   (user, ug) <- addNewAdminUserAndUserGroup "Captain" "Hollister" emailAddress
--   let uid1 = userid user
--       ugid = get ugID ug
--   void . dbUpdate $ AccessControlInsertUserGroupAdmin uid1 ugid
--   muser2 <- addNewUserToUserGroup "Dwayne" "Dibley" emailAddress2 ugid
--   ctx   <- set ctxmaybeuser muser2 <$> mkContext defaultLang
--   req   <- mkRequest GET []
--   res   <- fst <$> runTestKontra req ctx (accessControlAPIV2GetUserRoles uid1)
--   assertEqual "usergroup member cannot view roles of other usergroup member" 403 $ rsCode res
--     where
--       emailAddress  = "captain.hollister@scrive.com"
--       emailAddress2 = "dwayne.dibley@scrive.com"
