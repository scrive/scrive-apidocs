{-# LANGUAGE OverloadedStrings #-}
module UserGroup.APITest (userGroupApiTests) where

import Happstack.Server
import Test.Framework
import qualified Data.Text as T

import AccessControl.Model
import AccessControl.Types
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
  [ -- UserGroup POST and PUT endpoint tests
    testThat "non-admin and non-sales user can't create root UserGroup"
      env testNonAdminUserCannotCreateRootUserGroup
  , testThat "admin users can create root UserGroup"
      env testAdminUserCanCreateRootUserGroup
  , testThat "sales users can create root UserGroup"
      env testSalesUserCanCreateRootUserGroup
  , testThat "users can't create child UserGroup for a non-existent UserGroup"
      env testNonAdminUserCannotCreateChildUserGroupForNonExistentUserGroup
  , testThat "non-admin users can't create child UserGroup without permissions on parent UserGroup"
      env testNonAdminUserCannotCreateChildUserGroupWithoutUGAdminPermissions
  , testThat "users can create child UserGroup as UG Admin"
      env testUserCanCreateChildUserGroupWithPermissions
  , testThat "admin users can create child UserGroup without permissions"
      env testAdminUserCanCreateChildUserGroupWithoutPermissions
  , testThat "sales users can create child UserGroup without permissions"
      env testSalesUserCanCreateChildUserGroupWithoutPermissions
  , testThat "users can edit root UserGroup as UG Admin"
      env testUserCanEditRootUserGroupWithPermissions
  , testThat "users can edit child UserGroup as UG Admin"
      env testUserCanEditChildUserGroupWithPermissions
  -- UserGroup GET endpoint tests
  , testThat "non-admin and non-sales user can't view non-existent UserGroup"
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
  -- UserGroup DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroup"
      env testUserCannotDeleteNonExistentUserGroup
  , testThat "admin user cannot delete non-existent UserGroup"
      env testAdminUserCannotDeleteNonExistentUserGroup
  , testThat "sales user cannot delete non-existent UserGroup"
      env testSalesUserCannotDeleteNonExistentUserGroup
  , testThat "user can delete child UserGroup as UG Admin"
      env testUserCanDeleteChildUserGroupWithPermissions
  , testThat "admin user can delete child UserGroup without permissions"
      env testAdminUserCanDeleteChildUserGroupWithoutPermissions
  , testThat "sales user can delete child UserGroup without permissions"
      env testSalesUserCanDeleteChildUserGroupWithoutPermissions
  , testThat "user cannot delete root UserGroup as UG Admin"
      env testUserCannotDeleteRootUserGroupWithPermissions
  , testThat "admin user cannot delete root UserGroup"
      env testAdminUserCannotDeleteRootUserGroup
  , testThat "sales user cannot delete root UserGroup"
      env testSalesUserCannotDeleteRootUserGroup
  -- UserGroup Address GET endpoint tests
  , testThat "non-admin and non-sales user can't view non-existent UserGroupAddress"
      env testNonAdminUserCannotViewNonExistentUserGroupAddress
  , testThat "admin user can't view non-existent UserGroupAddress"
      env testAdminUserCannotViewNonExistentUserGroupAddress
  , testThat "sales user can't view non-existent UserGroupAddress"
      env testSalesUserCannotViewNonExistentUserGroupAddress
  , testThat "non-admin and non-sales user can view UserGroupAddress with permissions"
      env testNonAdminUserCanViewUserGroupAddressWithPermissions
  , testThat "admin user can UserGroupAddress with permissions"
      env testAdminUserCanViewUserGroupAddressWithPermissions
  , testThat "sales user can UserGroupAddress with permissions"
      env testSalesUserCanViewUserGroupAddressWithPermissions
  , testThat "admin user can UserGroupAddress without permissions"
      env testAdminUserCanViewUserGroupAddressWithoutPermissions
  , testThat "sales user can UserGroupAddress without permissions"
      env testSalesUserCanViewUserGroupAddressWithoutPermissions
  -- UserGroup Address PUT endpoint tests
  , testThat "non-admin and non-sales edit UserGroupAddress with permissions"
      env testSalesUserCanEditUserGroupAddressWithPermissions
  -- UserGroup Address DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroupAddress"
      env testUserCannotDeleteNonExistentUserGroupAddress
  , testThat "admin user cannot delete non-existent UserGroupAddress"
      env testAdminUserCannotDeleteNonExistentUserGroupAddress
  , testThat "sales user cannot delete non-existent UserGroupAddress"
      env testSalesUserCannotDeleteNonExistentUserGroupAddress
  , testThat "user can delete child UserGroupAddress as UG Admin"
      env testUserCanDeleteChildUserGroupAddressWithPermissions
  , testThat "admin user can delete child UserGroupAddress without permissions"
      env testAdminUserCanDeleteChildUserGroupAddressWithoutPermissions
  , testThat "sales user can delete child UserGroupAddress without permissions"
      env testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions
  , testThat "user cannot delete root UserGroupAddress as UG Admin"
      env testUserCannotDeleteRootUserGroupAddressWithPermissions
  , testThat "admin user cannot delete root UserGroupAddress"
      env testAdminUserCannotDeleteRootUserGroupAddress
  , testThat "sales user cannot delete root UserGroupAddress"
      env testSalesUserCannotDeleteRootUserGroupAddress
  -- UserGroup Settings GET endpoint tests
  , testThat "non-admin and non-sales user can't view non-existent UserGroupSettings"
      env testNonAdminUserCannotViewNonExistentUserGroupSettings
  , testThat "admin user can't view non-existent UserGroupSettings"
      env testAdminUserCannotViewNonExistentUserGroupSettings
  , testThat "sales user can't view non-existent UserGroupSettings"
      env testSalesUserCannotViewNonExistentUserGroupSettings
  , testThat "non-admin and non-sales user can view UserGroupSettings with permissions"
      env testNonAdminUserCanViewUserGroupSettingsWithPermissions
  , testThat "admin user can UserGroupSettings with permissions"
      env testAdminUserCanViewUserGroupSettingsWithPermissions
  , testThat "sales user can UserGroupSettings with permissions"
      env testSalesUserCanViewUserGroupSettingsWithPermissions
  , testThat "admin user can UserGroupSettings without permissions"
      env testAdminUserCanViewUserGroupSettingsWithoutPermissions
  , testThat "sales user can UserGroupSettings without permissions"
      env testSalesUserCanViewUserGroupSettingsWithoutPermissions
  -- UserGroup Settings PUT endpoint tests
  , testThat "non-admin and non-sales edit UserGroupSettings with permissions"
      env testSalesUserCanEditUserGroupSettingsWithPermissions
  -- UserGroup Settings DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroupSettings"
      env testUserCannotDeleteNonExistentUserGroupSettings
  , testThat "admin user cannot delete non-existent UserGroupSettings"
      env testAdminUserCannotDeleteNonExistentUserGroupSettings
  , testThat "sales user cannot delete non-existent UserGroupSettings"
      env testSalesUserCannotDeleteNonExistentUserGroupSettings
  , testThat "user can delete child UserGroupSettings as UG Admin"
      env testUserCanDeleteChildUserGroupSettingsWithPermissions
  , testThat "admin user can delete child UserGroupSettings without permissions"
      env testAdminUserCanDeleteChildUserGroupSettingsWithoutPermissions
  , testThat "sales user can delete child UserGroupSettings without permissions"
      env testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions
  , testThat "user can't delete root UserGroupSettings as UG Admin"
      env testUserCannotDeleteRootUserGroupSettingsWithPermissions
  , testThat "admin user can't delete root UserGroupSettings"
      env testAdminUserCannotDeleteRootUserGroupSettings
  , testThat "sales user can't delete root UserGroupSettings"
      env testSalesUserCannotDeleteRootUserGroupSettings
  -- UserGroup Users GET endpoint tests
  , testThat "non-admin and non-sales can view Users in UserGroup with permissions"
      env testNonAdminUserCanViewUsersInUserGroupWithPermissions
  ]

-- UserGroup POST and PUT endpoint tests

jsonRootUG :: String
jsonRootUG = "{\"name\":\"Test UserGroup blah blah\"}"

jsonWithParentUG :: UserGroupID -> String
jsonWithParentUG ugid = "{\"name\":\"Test UserGroup blah blah\",\"parent_id\":"
  <> "\"" <> (show ugid) <> "\"}"

testNonAdminUserCannotCreateRootUserGroup :: TestEnv ()
testNonAdminUserCannotCreateRootUserGroup = do
  muser <- addNewUser "Arthur" "Dent" "arthur.dent@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack jsonRootUG) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "non-admin/sales user can't create root UserGroup" 403 $ rsCode res

testAdminUserCanCreateRootUserGroup :: TestEnv ()
testAdminUserCanCreateRootUserGroup = do
  muser <- addNewUser "Tricia" "McMillan" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack jsonRootUG) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "admin user can create root UserGroup" 200 $ rsCode res
    where
      emailAddress = "trillian@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanCreateRootUserGroup :: TestEnv ()
testSalesUserCanCreateRootUserGroup = do
  muser <- addNewUser "Deep" "Thought" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack jsonRootUG) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "sales user can create root UserGroup" 200 $ rsCode res
    where
      emailAddress = "deep.thought@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testNonAdminUserCannotCreateChildUserGroupForNonExistentUserGroup :: TestEnv ()
testNonAdminUserCannotCreateChildUserGroupForNonExistentUserGroup = do
  muser <- addNewUser "Gag" "Halfrunt" "gag.halfrunt@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack jsonNonExistentParentUG) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "user can't create child UserGroup for non-existent UserGroup" 403 $ rsCode res
    where
      jsonNonExistentParentUG :: String
      jsonNonExistentParentUG = "{\"name\":\"Test UserGroup blah blah\",\"parent_id\":"
        <> "\"42\",\"settings\":{\"ip_address_mask_list\":[],\"data_retention_policy\":"
        <> "{\"idle_doc_timeout_preparation\":null,\"idle_doc_timeout_closed\":null,"
        <> "\"idle_doc_timeout_canceled\":null,\"idle_doc_timeout_timedout\":null,"
        <> "\"idle_doc_timeout_rejected\":null,\"idle_doc_timeout_error\":null,"
        <> "\"immediate_trash\":false},\"cgi_display_name\":null,\"cgi_service_id\":null,"
        <> "\"sms_provider\":\"default\",\"pad_app_mode\":\"list_view\","
        <> "\"pad_earchive_enabled\":true,\"legal_text\":false},\"invoicing\":"
        <> "{\"invoicing\":\"invoice\",\"payment_plan\":\"free\"},\"address\":"
        <> "{\"company_number\":\"\",\"address\":\"\",\"zip\":\"\",\"city\":\"\","
        <> "\"country\":\"\"},\"ui\":{\"mail_theme\":null,\"signview_theme\":null,"
        <> "\"service_theme\":null,\"browser_title\":null,\"sms_originator\":null,"
        <> "\"favicon\":null}}"

testNonAdminUserCannotCreateChildUserGroupWithoutUGAdminPermissions :: TestEnv ()
testNonAdminUserCannotCreateChildUserGroupWithoutUGAdminPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Prostetnic Vogon" "Jeltz" emailAddress
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack $ jsonWithParentUG $ get ugID ug) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "UserGroup admin can create child UserGroup" 403 $ rsCode res
    where
      emailAddress = "prostetnic.vogon.jeltz@scrive.com"

testUserCanCreateChildUserGroupWithPermissions :: TestEnv ()
testUserCanCreateChildUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Great Green" "Arkleseizure" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack $ jsonWithParentUG $ get ugID ug) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "UserGroup admin can create child UserGroup" 200 $ rsCode res
    where
      emailAddress = "great.green.arkleseizure@scrive.com"

testAdminUserCanCreateChildUserGroupWithoutPermissions :: TestEnv ()
testAdminUserCanCreateChildUserGroupWithoutPermissions = do
  muser <- addNewUser "Oolon" "Colluphid" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  ug    <- addNewUserGroup
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack $ jsonWithParentUG $ get ugID ug) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "admin user can create root UserGroup" 200 $ rsCode res
    where
      emailAddress = "oolon.colluphid@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanCreateChildUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanCreateChildUserGroupWithoutPermissions = do
  muser <- addNewUser "Marvin" "the Paranoid Android" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  ug    <- addNewUserGroup
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack $ jsonWithParentUG $ get ugID ug) ]
  res   <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "sales user can create root UserGroup" 200 $ rsCode res
    where
      emailAddress = "marvin@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCanEditRootUserGroupWithPermissions :: TestEnv ()
testUserCanEditRootUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Great Green" "Arkleseizure" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest POST [ ("usergroup", inText $ T.pack jsonExistingRootUG) ]
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Update ugid)
  assertEqual "users can edit root UserGroup as UG Admin" 200 $ rsCode res
    where
      emailAddress = "great.green.arkleseizure@scrive.com"
      jsonExistingRootUG :: String
      jsonExistingRootUG = "{\"name\":\"New usergroup name\",\"parent_id\":null}"

testUserCanEditChildUserGroupWithPermissions :: TestEnv ()
testUserCanEditChildUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hotblack" "Desiato" emailAddress
  let uid = userid user
      ugidParent = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugidParent
  ugidChild <- get ugID <$> addNewUserGroupWithParent False (Just ugidParent)
  ctx     <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  let field = ("usergroup", inText $ T.pack $ jsonExistingChildUG ugidParent)
  req     <- mkRequest POST [ field ]
  res     <- fst <$> runTestKontra req ctx (userGroupApiV2Update ugidChild)
  assertEqual "users can edit child UserGroup as UG Admin" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      jsonExistingChildUG :: UserGroupID -> String
      jsonExistingChildUG ugidParent = "{\"name\":\"Test UserGroup blah blah\""
        <> ",\"parent_id\":\"" <> (show ugidParent) <> "\"}"

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
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "non-admin user can view UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "googleplex.starthinker@scrive.com"

testAdminUserCanViewUserGroupWithPermissions :: TestEnv ()
testAdminUserCanViewUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Grunthos" "the Flatulent" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
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
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
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

-- UserGroup DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testUserCannotDeleteNonExistentUserGroup = do
  muser <- addNewUser "Almighty" "Bob" "almighty.bob@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testAdminUserCannotDeleteNonExistentUserGroup = do
  muser <- addNewUser "Blart" "Versenwald III" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "blart.versenwald@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroup = do
  muser <- addNewUser "Gogrilla" "Mincefriend" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "gogrilla.mincefriend@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCanDeleteChildUserGroupWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupWithPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let uid   = userid user
      ugid1 = get ugID ug1
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"

testAdminUserCanDeleteChildUserGroupWithoutPermissions :: TestEnv ()
testAdminUserCanDeleteChildUserGroupWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup without permissions" 200 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup without permissions" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hurling" "Frootmig" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup" 400 $ rsCode res
    where
      emailAddress = "hurling.frootmig@scrive.com"

-- Perhaps this should be allowed, currently this checks a prohibition of deleting roots
testAdminUserCannotDeleteRootUserGroup :: TestEnv ()
testAdminUserCannotDeleteRootUserGroup = do
  (user, ug) <- addNewAdminUserAndUserGroup "Humma" "Kavula" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup" 400 $ rsCode res
    where
      emailAddress = "humma.kavula@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

-- Perhaps this should be allowed, currently this checks a prohibition of deleting roots
testSalesUserCannotDeleteRootUserGroup :: TestEnv ()
testSalesUserCannotDeleteRootUserGroup = do
  (user, ug) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup" 400 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

-- UserGroup Address GET endpoint tests

testNonAdminUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testNonAdminUserCannotViewNonExistentUserGroupAddress = do
  muser <- addNewUser "Googleplex" "Starthinker" "googleplex.starthinker@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup Address" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testAdminUserCannotViewNonExistentUserGroupAddress = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup Address" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroupAddress = do
  muser <- addNewUser "Hotblack" "Desiato" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup Address" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testNonAdminUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testNonAdminUserCanViewUserGroupAddressWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Googleplex" "Starthinker" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "non-admin user can view UserGroup Address with permissions" 200 $ rsCode res
    where
      emailAddress = "googleplex.starthinker@scrive.com"

testAdminUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testAdminUserCanViewUserGroupAddressWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Grunthos" "the Flatulent" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can view UserGroup Address with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupAddressWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hotblack" "Desiato" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can view UserGroup Address with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testAdminUserCanViewUserGroupAddressWithoutPermissions :: TestEnv ()
testAdminUserCanViewUserGroupAddressWithoutPermissions = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can view UserGroup Address with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupAddressWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupAddressWithoutPermissions = do
  muser <- addNewUser "Hotblack" "Desiato" "hotblack.desiato@scrive.com"
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can view UserGroup Address with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

-- UserGroup Address Update endpoint tests

testSalesUserCanEditUserGroupAddressWithPermissions :: TestEnv ()
testSalesUserCanEditUserGroupAddressWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Great Green" "Arkleseizure" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest POST [ ("contact_details", inText addressJson) ]
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Update ugid)
  assertEqual "non-admin and non-sales edit UserGroup Contact Details with permissions" 200 $ rsCode res
    where
      emailAddress = "great.green.arkleseizure@scrive.com"
      addressJson = "{\"address\":{\"company_number\":\"0987654321\",\"address\":"
        <> "\"dobra\",\"zip\":\"00-321\",\"city\":\"warsaw\",\"country\":\"PL\"}}"

-- UserGroup Address DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testUserCannotDeleteNonExistentUserGroupAddress = do
  muser <- addNewUser "Almighty" "Bob" "almighty.bob@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Contact Details" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testAdminUserCannotDeleteNonExistentUserGroupAddress = do
  muser <- addNewUser "Blart" "Versenwald III" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Address" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "blart.versenwald@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroupAddress = do
  muser <- addNewUser "Gogrilla" "Mincefriend" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Address" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "gogrilla.mincefriend@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCanDeleteChildUserGroupAddressWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupAddressWithPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let uid   = userid user
      ugid1 = get ugID ug1
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup Address" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"

testAdminUserCanDeleteChildUserGroupAddressWithoutPermissions :: TestEnv ()
testAdminUserCanDeleteChildUserGroupAddressWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup Address without permissions" 200 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup Address without permissions" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupAddressWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupAddressWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hurling" "Frootmig" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup Address" 400 $ rsCode res
    where
      emailAddress = "hurling.frootmig@scrive.com"

testAdminUserCannotDeleteRootUserGroupAddress :: TestEnv ()
testAdminUserCannotDeleteRootUserGroupAddress = do
  (user, ug) <- addNewAdminUserAndUserGroup "Humma" "Kavula" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup Address" 400 $ rsCode res
    where
      emailAddress = "humma.kavula@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotDeleteRootUserGroupAddress :: TestEnv ()
testSalesUserCannotDeleteRootUserGroupAddress = do
  (user, ug) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup Address" 400 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

-- UserGroup Settings GET endpoint tests

testNonAdminUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testNonAdminUserCannotViewNonExistentUserGroupSettings = do
  muser <- addNewUser "Googleplex" "Starthinker" "googleplex.starthinker@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testAdminUserCannotViewNonExistentUserGroupSettings = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroupSettings = do
  muser <- addNewUser "Hotblack" "Desiato" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testNonAdminUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testNonAdminUserCanViewUserGroupSettingsWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Googleplex" "Starthinker" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "non-admin user can view UserGroup Settings with permissions" 200 $ rsCode res
    where
      emailAddress = "googleplex.starthinker@scrive.com"

testAdminUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testAdminUserCanViewUserGroupSettingsWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Grunthos" "the Flatulent" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can view UserGroup Settings with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupSettingsWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hotblack" "Desiato" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can view UserGroup Settings with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testAdminUserCanViewUserGroupSettingsWithoutPermissions :: TestEnv ()
testAdminUserCanViewUserGroupSettingsWithoutPermissions = do
  muser <- addNewUser "Grunthos" "the Flatulent" emailAddress
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can view UserGroup Settings with permissions" 200 $ rsCode res
    where
      emailAddress = "grunthos.the.flatulent@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanViewUserGroupSettingsWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupSettingsWithoutPermissions = do
  muser <- addNewUser "Hotblack" "Desiato" "hotblack.desiato@scrive.com"
  ug <- addNewUserGroup
  let ugid = get ugID ug
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can view UserGroup Settings with permissions" 200 $ rsCode res
    where
      emailAddress = "hotblack.desiato@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

-- UserGroup Settings Update endpoint tests

testSalesUserCanEditUserGroupSettingsWithPermissions :: TestEnv ()
testSalesUserCanEditUserGroupSettingsWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Great Green" "Arkleseizure" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest POST [ ("settings", inText settingsJson) ]
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Update ugid)
  assertEqual "non-admin and non-sales edit UserGroupSettings with permissions" 200 $ rsCode res
    where
      emailAddress = "great.green.arkleseizure@scrive.com"
      settingsJson = "{\"ip_address_mask_list\":[],\"data_retention_policy\":"
        <> "{\"idle_doc_timeout_preparation\":null,\"idle_doc_timeout_closed\":null,"
        <> "\"idle_doc_timeout_canceled\":null,\"idle_doc_timeout_timedout\":null,"
        <> "\"idle_doc_timeout_rejected\":null,\"idle_doc_timeout_error\":null,"
        <> "\"immediate_trash\":false},\"cgi_display_name\":\"a new display name\","
        <> "\"cgi_service_id\":null,\"sms_provider\":\"default\",\"pad_app_mode\":"
        <> "\"list_view\",\"pad_earchive_enabled\":true,\"legal_text\":false}"

-- UserGroup Settings DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testUserCannotDeleteNonExistentUserGroupSettings = do
  muser <- addNewUser "Almighty" "Bob" "almighty.bob@scrive.com"
  ctx   <- set ctxmaybeuser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123

testAdminUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testAdminUserCannotDeleteNonExistentUserGroupSettings = do
  muser <- addNewUser "Blart" "Versenwald III" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "blart.versenwald@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroupSettings = do
  muser <- addNewUser "Gogrilla" "Mincefriend" emailAddress
  ctx   <- setUser muser <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
    where
      ugid = unsafeUserGroupID 123
      emailAddress = "gogrilla.mincefriend@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCanDeleteChildUserGroupSettingsWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupSettingsWithPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let uid   = userid user
      ugid1 = get ugID ug1
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup Settings" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"

testAdminUserCanDeleteChildUserGroupSettingsWithoutPermissions :: TestEnv ()
testAdminUserCanDeleteChildUserGroupSettingsWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup Settings without permissions" 200 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions = do
  (user, ug1) <- addNewAdminUserAndUserGroup "Hig" "Hurtenflurst" emailAddress
  let ugid1 = get ugID ug1
  ugid2 <- get ugID <$> addNewUserGroupWithParent False (Just ugid1)
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup Settings without permissions" 200 $ rsCode res
    where
      emailAddress = "hig.hurtenflurst@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupSettingsWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupSettingsWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Hurling" "Frootmig" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup Settings" 400 $ rsCode res
    where
      emailAddress = "hurling.frootmig@scrive.com"

testAdminUserCannotDeleteRootUserGroupSettings :: TestEnv ()
testAdminUserCannotDeleteRootUserGroupSettings = do
  (user, ug) <- addNewAdminUserAndUserGroup "Humma" "Kavula" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup Settings" 400 $ rsCode res
    where
      emailAddress = "humma.kavula@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxadminaccounts [Email emailAddress]

testSalesUserCannotDeleteRootUserGroupSettings :: TestEnv ()
testSalesUserCannotDeleteRootUserGroupSettings = do
  (user, ug) <- addNewAdminUserAndUserGroup "Max" "Quordlepleen" emailAddress
  let ugid = get ugID ug
  ctx   <- setUser (Just user) <$> mkContext defaultLang
  req   <- mkRequest DELETE []
  res   <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup Settings" 400 $ rsCode res
    where
      emailAddress = "max.quordlepleen@scrive.com"
      setUser muser = set ctxmaybeuser muser . set ctxsalesaccounts [Email emailAddress]

-- UserGroup Users GET endpoint tests

testNonAdminUserCanViewUsersInUserGroupWithPermissions :: TestEnv ()
testNonAdminUserCanViewUsersInUserGroupWithPermissions = do
  (user, ug) <- addNewAdminUserAndUserGroup "Googleplex" "Starthinker" emailAddress
  let uid  = userid user
      ugid = get ugID ug
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx   <- set ctxmaybeuser (Just user) <$> mkContext defaultLang
  req   <- mkRequest GET []
  res   <- fst <$> runTestKontra req ctx (userGroupApiUsersV2Get ugid)
  assertEqual "non-admin user can view Users in UserGroup with permissions" 200 $ rsCode res
    where
      emailAddress = "googleplex.starthinker@scrive.com"
