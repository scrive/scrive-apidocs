{-# LANGUAGE OverloadedStrings #-}
module UserGroup.APITest (userGroupApiTests) where

import Happstack.Server
import Test.Framework

import AccessControl.Model
import Context
import DB
import TestingUtil
import TestKontra
import User.Email
import User.Model
import UserGroup.API
import UserGroup.Types

userGroupApiTests :: TestEnvSt -> Test
userGroupApiTests env = testGroup "UserGroupAPI"
  [ -- UserGroup GET endpoint tests
    testThat "non-admin and non-sales user can't view non-existent UserGroup"
      env testNonAdminUserCannotViewNonExistentUserGroup
  , testThat "admin user can't view non-existent UserGroup"
      env testAdminUserCannotViewNonExistentUserGroup
  , testThat "sales user can't view non-existent UserGroup"
      env testSalesUserCannotViewNonExistentUserGroup
  , testThat "non-admin and non-sales user can view UserGroup with permissions"
      env testNonAdminUserCanViewUserGroupWithPermissions
  , testThat "admin user can UserGroup with permissions"
      env testAdminUserCanViewUserGroupWithPermissions
  , testThat "sales user can UserGroup with permissions"
      env testSalesUserCanViewUserGroupWithPermissions
  , testThat "admin user can UserGroup without permissions"
      env testAdminUserCanViewUserGroupWithoutPermissions
  , testThat "sales user can UserGroup without permissions"
      env testSalesUserCanViewUserGroupWithoutPermissions
  ]

-- UserGroup GET endpoint tests

testNonAdminUserCannotViewNonExistentUserGroup :: TestEnv ()
testNonAdminUserCannotViewNonExistentUserGroup = do
  muser <- addNewUser "Googleplex" "Starthinker" "googleplex.starthinker@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotViewNonExistentUserGroup :: TestEnv ()
testAdminUserCannotViewNonExistentUserGroup = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroup :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroup = do
  muser <- addNewUser "Hotblack" "Desiato" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testNonAdminUserCanViewUserGroupWithPermissions :: TestEnv ()
testNonAdminUserCanViewUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Googleplex" "Starthinker" emailAddress
  let ugid = get ugID ug
  void . dbUpdate $ AccessControlInsertUserGroupAdmin (userid user) ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "non-admin user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "googleplex.starthinker@scrive.com"

testAdminUserCanViewUserGroupWithPermissions :: TestEnv ()
testAdminUserCanViewUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Grunthos" "the Flatulent" emailAddress
  let ugid = get ugID ug
  void . dbUpdate $ AccessControlInsertUserGroupAdmin (userid user) ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hotblack" "Desiato" emailAddress
  let ugid = get ugID ug
  void . dbUpdate $ AccessControlInsertUserGroupAdmin (userid user) ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testAdminUserCanViewUserGroupWithoutPermissions :: TestEnv ()
testAdminUserCanViewUserGroupWithoutPermissions = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupWithoutPermissions = do
  muser <- addNewUser "Hotblack" "Desiato" emailAddress
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]
