{-# LANGUAGE OverloadedStrings #-}
module UserGroup.APITest (userGroupApiTests) where

import Data.Aeson (encode)
import Data.Aeson.Types
import Happstack.Server
import Test.Framework
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import AccessControl.Model
import AccessControl.Types
import DB
import Doc.API.V2.AesonTestUtils (jsonTestRequestHelper, lookupObjectArray)
import Tag
import TestingUtil
import TestKontra
import User.Email
import User.Model
import UserGroup.API
import UserGroup.Model
import UserGroup.Types

userGroupApiTests :: TestEnvSt -> Test
userGroupApiTests env = testGroup
  "UserGroupAPI"
  [ -- UserGroup POST and PUT endpoint tests
    testThat "non-god-mode and non-sales user can't create root UserGroup"
             env
             testNonGodModeUserCannotCreateRootUserGroup
  , testThat "god-mode users can create root UserGroup"
             env
             testGodModeUserCanCreateRootUserGroup
  , testThat "sales users can create root UserGroup"
             env
             testSalesUserCanCreateRootUserGroup
  , testThat "users can't create child UserGroup for a non-existent UserGroup"
             env
             testNonGodModeUserCannotCreateChildUserGroupForNonExistentUserGroup
  , testThat
    "non-admin users can't create child UserGroup without permissions on parent UserGroup"
    env
    testNonGodModeUserCannotCreateChildUserGroupWithoutUGAdminPermissions
  , testThat "users can create child UserGroup as UG Admin"
             env
             testUserCanCreateChildUserGroupWithPermissions
  , testThat "god-mode users can create child UserGroup without permissions"
             env
             testGodModeUserCanCreateChildUserGroupWithoutPermissions
  , testThat "sales users can create child UserGroup without permissions"
             env
             testSalesUserCanCreateChildUserGroupWithoutPermissions
  , testThat "users can edit root UserGroup as UG Admin"
             env
             testUserCanEditRootUserGroupWithPermissions
  , testThat "users can edit child UserGroup as UG Admin"
             env
             testUserCanEditChildUserGroupWithPermissions
  -- UserGroup GET endpoint tests
  , testThat "non-god-mode and non-sales user can't view non-existent UserGroup"
             env
             testNonGodModeUserCannotViewNonExistentUserGroup
  , testThat "god-mode user can't view non-existent UserGroup"
             env
             testGodModeUserCannotViewNonExistentUserGroup
  , testThat "sales user can't view non-existent UserGroup"
             env
             testSalesUserCannotViewNonExistentUserGroup
  , testThat "non-god-mode and non-sales user can view UserGroup with permissions"
             env
             testNonGodModeUserCanViewUserGroupWithPermissions
  , testThat "god-mode user can UserGroup with permissions"
             env
             testGodModeUserCanViewUserGroupWithPermissions
  , testThat "sales user can UserGroup with permissions"
             env
             testSalesUserCanViewUserGroupWithPermissions
  , testThat "god-mode user can UserGroup without permissions"
             env
             testGodModeUserCanViewUserGroupWithoutPermissions
  , testThat "sales user can UserGroup without permissions"
             env
             testSalesUserCanViewUserGroupWithoutPermissions
  -- UserGroup DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroup"
             env
             testUserCannotDeleteNonExistentUserGroup
  , testThat "god-mode user cannot delete non-existent UserGroup"
             env
             testGodModeUserCannotDeleteNonExistentUserGroup
  , testThat "sales user cannot delete non-existent UserGroup"
             env
             testSalesUserCannotDeleteNonExistentUserGroup
  , testThat "user can delete child UserGroup as UG Admin"
             env
             testUserCanDeleteChildUserGroupWithPermissions
  , testThat "god-mode user can delete child UserGroup without permissions"
             env
             testGodModeUserCanDeleteChildUserGroupWithoutPermissions
  , testThat "sales user can delete child UserGroup without permissions"
             env
             testSalesUserCanDeleteChildUserGroupWithoutPermissions
  , testThat "user cannot delete root UserGroup as UG Admin"
             env
             testUserCannotDeleteRootUserGroupWithPermissions
  , testThat "god-mode user cannot delete root UserGroup"
             env
             testGodModeUserCannotDeleteRootUserGroup
  , testThat "sales user cannot delete root UserGroup"
             env
             testSalesUserCannotDeleteRootUserGroup
  -- UserGroup Address GET endpoint tests
  , testThat "non-god-mode and non-sales user can't view non-existent UserGroupAddress"
             env
             testNonGodModeUserCannotViewNonExistentUserGroupAddress
  , testThat "god-mode user can't view non-existent UserGroupAddress"
             env
             testGodModeUserCannotViewNonExistentUserGroupAddress
  , testThat "sales user can't view non-existent UserGroupAddress"
             env
             testSalesUserCannotViewNonExistentUserGroupAddress
  , testThat
    "non-god-mode and non-sales user can view UserGroupAddress with permissions"
    env
    testNonGodModeUserCanViewUserGroupAddressWithPermissions
  , testThat "god-mode user can UserGroupAddress with permissions"
             env
             testGodModeUserCanViewUserGroupAddressWithPermissions
  , testThat "sales user can UserGroupAddress with permissions"
             env
             testSalesUserCanViewUserGroupAddressWithPermissions
  , testThat "god-mode user can UserGroupAddress without permissions"
             env
             testGodModeUserCanViewUserGroupAddressWithoutPermissions
  , testThat "sales user can UserGroupAddress without permissions"
             env
             testSalesUserCanViewUserGroupAddressWithoutPermissions
  -- UserGroup Address PUT endpoint tests
  , testThat "non-god-mode and non-sales edit UserGroupAddress with permissions"
             env
             testSalesUserCanEditUserGroupAddressWithPermissions
  -- UserGroup Address DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroupAddress"
             env
             testUserCannotDeleteNonExistentUserGroupAddress
  , testThat "god-mode user cannot delete non-existent UserGroupAddress"
             env
             testGodModeUserCannotDeleteNonExistentUserGroupAddress
  , testThat "sales user cannot delete non-existent UserGroupAddress"
             env
             testSalesUserCannotDeleteNonExistentUserGroupAddress
  , testThat "user can delete child UserGroupAddress as UG Admin"
             env
             testUserCanDeleteChildUserGroupAddressWithPermissions
  , testThat "god-mode user can delete child UserGroupAddress without permissions"
             env
             testGodModeUserCanDeleteChildUserGroupAddressWithoutPermissions
  , testThat "sales user can delete child UserGroupAddress without permissions"
             env
             testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions
  , testThat "user cannot delete root UserGroupAddress as UG Admin"
             env
             testUserCannotDeleteRootUserGroupAddressWithPermissions
  , testThat "god-mode user cannot delete root UserGroupAddress"
             env
             testGodModeUserCannotDeleteRootUserGroupAddress
  , testThat "sales user cannot delete root UserGroupAddress"
             env
             testSalesUserCannotDeleteRootUserGroupAddress
  -- UserGroup Settings GET endpoint tests
  , testThat "non-god-mode and non-sales user can't view non-existent UserGroupSettings"
             env
             testNonGodModeUserCannotViewNonExistentUserGroupSettings
  , testThat "god-mode user can't view non-existent UserGroupSettings"
             env
             testGodModeUserCannotViewNonExistentUserGroupSettings
  , testThat "sales user can't view non-existent UserGroupSettings"
             env
             testSalesUserCannotViewNonExistentUserGroupSettings
  , testThat
    "non-god-mode and non-sales user can view UserGroupSettings with permissions"
    env
    testNonGodModeUserCanViewUserGroupSettingsWithPermissions
  , testThat "god-mode user can UserGroupSettings with permissions"
             env
             testGodModeUserCanViewUserGroupSettingsWithPermissions
  , testThat "sales user can UserGroupSettings with permissions"
             env
             testSalesUserCanViewUserGroupSettingsWithPermissions
  , testThat "god-mode user can UserGroupSettings without permissions"
             env
             testGodModeUserCanViewUserGroupSettingsWithoutPermissions
  , testThat "sales user can UserGroupSettings without permissions"
             env
             testSalesUserCanViewUserGroupSettingsWithoutPermissions
  -- UserGroup Settings PUT endpoint tests
  , testThat "non-god-mode and non-sales edit UserGroupSettings with permissions"
             env
             testSalesUserCanEditUserGroupSettingsWithPermissions
  -- UserGroup Settings DELETE endpoint tests
  , testThat "non-user cannot delete non-existent UserGroupSettings"
             env
             testUserCannotDeleteNonExistentUserGroupSettings
  , testThat "god-mode user cannot delete non-existent UserGroupSettings"
             env
             testGodModeUserCannotDeleteNonExistentUserGroupSettings
  , testThat "sales user cannot delete non-existent UserGroupSettings"
             env
             testSalesUserCannotDeleteNonExistentUserGroupSettings
  , testThat "user can delete child UserGroupSettings as UG Admin"
             env
             testUserCanDeleteChildUserGroupSettingsWithPermissions
  , testThat "god-mode user can delete child UserGroupSettings without permissions"
             env
             testGodModeUserCanDeleteChildUserGroupSettingsWithoutPermissions
  , testThat "sales user can delete child UserGroupSettings without permissions"
             env
             testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions
  , testThat "user can't delete root UserGroupSettings as UG Admin"
             env
             testUserCannotDeleteRootUserGroupSettingsWithPermissions
  , testThat "god-mode user can't delete root UserGroupSettings"
             env
             testGodModeUserCannotDeleteRootUserGroupSettings
  , testThat "sales user can't delete root UserGroupSettings"
             env
             testSalesUserCannotDeleteRootUserGroupSettings
  -- UserGroup Users GET endpoint tests
  , testThat "non-god-mode and non-sales can view Users in UserGroup with permissions"
             env
             testNonGodModeUserCanViewUsersInUserGroupWithPermissions
  , testThat "user can create tags on root user group"
             env
             testUserCanCreateTagsOnRootUserGroup
  , testThat "user can create tags on child user group"
             env
             testUserCanCreateTagsOnChildUserGroup
  , testThat "user can update tags" env testUserCanUpdateTags
  , testThat "user can view tags"   env testUserCanViewTags
  ]

-- UserGroup POST and PUT endpoint tests

jsonRootUG :: String
jsonRootUG = "{\"name\":\"Test UserGroup blah blah\"}"

jsonWithParentUG :: UserGroupID -> String
jsonWithParentUG ugid =
  "{\"name\":\"Test UserGroup blah blah\",\"parent_id\":" <> "\"" <> (show ugid) <> "\"}"

testNonGodModeUserCannotCreateRootUserGroup :: TestEnv ()
testNonGodModeUserCannotCreateRootUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Arthur"
                                               , lastName = return "Dent"
                                               , email = return "arthur.dent@scrive.com"
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack jsonRootUG)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "non-admin/sales user can't create root UserGroup" 403 $ rsCode res

testGodModeUserCanCreateRootUserGroup :: TestEnv ()
testGodModeUserCanCreateRootUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Tricia"
                                               , lastName  = return "McMillan"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack jsonRootUG)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "admin user can create root UserGroup" 200 $ rsCode res
  where
    emailAddress = "trillian@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanCreateRootUserGroup :: TestEnv ()
testSalesUserCanCreateRootUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Deep"
                                               , lastName  = return "Thought"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack jsonRootUG)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "sales user can create root UserGroup" 200 $ rsCode res
  where
    emailAddress = "deep.thought@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testNonGodModeUserCannotCreateChildUserGroupForNonExistentUserGroup :: TestEnv ()
testNonGodModeUserCannotCreateChildUserGroupForNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Gag"
                                               , lastName = return "Halfrunt"
                                               , email = return "gag.halfrunt@scrive.com"
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack jsonNonExistentParentUG)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "user can't create child UserGroup for non-existent UserGroup" 403
    $ rsCode res
  where
    jsonNonExistentParentUG :: String
    jsonNonExistentParentUG =
      "{\"name\":\"Test UserGroup blah blah\",\"parent_id\":"
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

testNonGodModeUserCannotCreateChildUserGroupWithoutUGAdminPermissions :: TestEnv ()
testNonGodModeUserCannotCreateChildUserGroupWithoutUGAdminPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Prostetnic Vogon"
                                               , lastName       = return "Jeltz"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest
    POST
    [("usergroup", inText $ T.pack $ jsonWithParentUG $ user ^. #groupID)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "UserGroup admin can create child UserGroup" 403 $ rsCode res
  where emailAddress = "prostetnic.vogon.jeltz@scrive.com"

testUserCanCreateChildUserGroupWithPermissions :: TestEnv ()
testUserCanCreateChildUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Great Green"
                                               , lastName       = return "Arkleseizure"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack $ jsonWithParentUG $ ugid)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "UserGroup admin can create child UserGroup" 200 $ rsCode res
  where emailAddress = "great.green.arkleseizure@scrive.com"

testGodModeUserCanCreateChildUserGroupWithoutPermissions :: TestEnv ()
testGodModeUserCanCreateChildUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Oolon"
                                               , lastName  = return "Colluphid"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  ug  <- instantiateRandomUserGroup
  req <- mkRequest POST [("usergroup", inText $ T.pack $ jsonWithParentUG $ ug ^. #id)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "admin user can create root UserGroup" 200 $ rsCode res
  where
    emailAddress = "oolon.colluphid@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanCreateChildUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanCreateChildUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Marvin"
                                               , lastName  = return "the Paranoid Android"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  ug  <- instantiateRandomUserGroup
  req <- mkRequest POST [("usergroup", inText $ T.pack $ jsonWithParentUG $ ug ^. #id)]
  res <- fst <$> runTestKontra req ctx userGroupApiV2Create
  assertEqual "sales user can create root UserGroup" 200 $ rsCode res
  where
    emailAddress = "marvin@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCanEditRootUserGroupWithPermissions :: TestEnv ()
testUserCanEditRootUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Great Green"
                                               , lastName       = return "Arkleseizure"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("usergroup", inText $ T.pack jsonExistingRootUG)]
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Update ugid)
  assertEqual "users can edit root UserGroup as UG Admin" 200 $ rsCode res
  where
    emailAddress = "great.green.arkleseizure@scrive.com"
    jsonExistingRootUG :: String
    jsonExistingRootUG = "{\"name\":\"New usergroup name\",\"parent_id\":null}"

testUserCanEditChildUserGroupWithPermissions :: TestEnv ()
testUserCanEditChildUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hotblack"
                                               , lastName       = return "Desiato"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid        = user ^. #id
      ugidParent = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugidParent
  ugidChild <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugidParent
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  let field = ("usergroup", inText $ T.pack $ jsonExistingChildUG ugidParent)
  req <- mkRequest POST [field]
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Update ugidChild)
  assertEqual "users can edit child UserGroup as UG Admin" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    jsonExistingChildUG :: UserGroupID -> String
    jsonExistingChildUG ugidParent =
      "{\"name\":\"Test UserGroup blah blah\""
        <> ",\"parent_id\":\""
        <> (show ugidParent)
        <> "\"}"

-- UserGroup GET endpoint tests

testNonGodModeUserCannotViewNonExistentUserGroup :: TestEnv ()
testNonGodModeUserCannotViewNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Googleplex"
    , lastName  = return "Starthinker"
    , email     = return "googleplex.starthinker@scrive.com"
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotViewNonExistentUserGroup :: TestEnv ()
testGodModeUserCannotViewNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroup :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Hotblack"
                                               , lastName  = return "Desiato"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testNonGodModeUserCanViewUserGroupWithPermissions :: TestEnv ()
testNonGodModeUserCanViewUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Googleplex"
                                               , lastName       = return "Starthinker"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "non-admin user can view UserGroup with permissions" 200 $ rsCode res
  where emailAddress = "googleplex.starthinker@scrive.com"

testGodModeUserCanViewUserGroupWithPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Grunthos"
                                               , lastName       = return "the Flatulent"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can view UserGroup with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hotblack"
                                               , lastName       = return "Desiato"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can view UserGroup with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testGodModeUserCanViewUserGroupWithoutPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "admin user can view UserGroup with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Hotblack"
                                               , lastName  = return "Desiato"
                                               , email     = return emailAddress
                                               }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Get ugid)
  assertEqual "sales user can view UserGroup with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testUserCannotDeleteNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Almighty"
                                               , lastName = return "Bob"
                                               , email = return "almighty.bob@scrive.com"
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testGodModeUserCannotDeleteNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Blart"
                                               , lastName  = return "Versenwald III"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "blart.versenwald@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroup :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Gogrilla"
                                               , lastName  = return "Mincefriend"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "gogrilla.mincefriend@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCanDeleteChildUserGroupWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid   = user ^. #id
      ugid1 = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup" 200 $ rsCode res
  where emailAddress = "hig.hurtenflurst@scrive.com"

testGodModeUserCanDeleteChildUserGroupWithoutPermissions :: TestEnv ()
testGodModeUserCanDeleteChildUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup without permissions" 200 $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup without permissions" 200 $ rsCode res
  where
    emailAddress = "hig.hurtenflurst@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hurling"
                                               , lastName       = return "Frootmig"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup" 400 $ rsCode res
  where emailAddress = "hurling.frootmig@scrive.com"

-- Perhaps this should be allowed, currently this checks a prohibition of deleting roots
testGodModeUserCannotDeleteRootUserGroup :: TestEnv ()
testGodModeUserCannotDeleteRootUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Humma"
                                               , lastName       = return "Kavula"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup" 400 $ rsCode res
  where
    emailAddress = "humma.kavula@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

-- Perhaps this should be allowed, currently this checks a prohibition of deleting roots
testSalesUserCannotDeleteRootUserGroup :: TestEnv ()
testSalesUserCannotDeleteRootUserGroup = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup" 400 $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup Address GET endpoint tests

testNonGodModeUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testNonGodModeUserCannotViewNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Googleplex"
    , lastName  = return "Starthinker"
    , email     = return "googleplex.starthinker@scrive.com"
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup Address" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testGodModeUserCannotViewNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup Address" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroupAddress :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Hotblack"
                                               , lastName  = return "Desiato"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup Address" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testNonGodModeUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testNonGodModeUserCanViewUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Googleplex"
                                               , lastName       = return "Starthinker"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "non-admin user can view UserGroup Address with permissions" 200
    $ rsCode res
  where emailAddress = "googleplex.starthinker@scrive.com"

testGodModeUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Grunthos"
                                               , lastName       = return "the Flatulent"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can view UserGroup Address with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupAddressWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hotblack"
                                               , lastName       = return "Desiato"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can view UserGroup Address with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testGodModeUserCanViewUserGroupAddressWithoutPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupAddressWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "admin user can view UserGroup Address with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupAddressWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupAddressWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Hotblack"
    , lastName  = return "Desiato"
    , email     = return "hotblack.desiato@scrive.com"
    }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Get ugid)
  assertEqual "sales user can view UserGroup Address with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup Address Update endpoint tests

testSalesUserCanEditUserGroupAddressWithPermissions :: TestEnv ()
testSalesUserCanEditUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Great Green"
                                               , lastName       = return "Arkleseizure"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("contact_details", inText addressJson)]
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Update ugid)
  assertEqual "non-admin and non-sales edit UserGroup Contact Details with permissions"
              200
    $ rsCode res
  where
    emailAddress = "great.green.arkleseizure@scrive.com"
    addressJson =
      "{\"address\":{\"company_number\":\"0987654321\",\"address\":"
        <> "\"dobra\",\"zip\":\"00-321\",\"city\":\"warsaw\",\"country\":\"PL\"}}"

-- UserGroup Address DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testUserCannotDeleteNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Almighty"
                                               , lastName = return "Bob"
                                               , email = return "almighty.bob@scrive.com"
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Contact Details" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testGodModeUserCannotDeleteNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Blart"
                                               , lastName  = return "Versenwald III"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Address" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "blart.versenwald@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroupAddress :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Gogrilla"
                                               , lastName  = return "Mincefriend"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Address" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "gogrilla.mincefriend@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCanDeleteChildUserGroupAddressWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid   = user ^. #id
      ugid1 = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup Address" 200 $ rsCode res
  where emailAddress = "hig.hurtenflurst@scrive.com"

testGodModeUserCanDeleteChildUserGroupAddressWithoutPermissions :: TestEnv ()
testGodModeUserCanDeleteChildUserGroupAddressWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup Address without permissions" 200
    $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupAddressWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup Address without permissions" 200
    $ rsCode res
  where
    emailAddress = "hig.hurtenflurst@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupAddressWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupAddressWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hurling"
                                               , lastName       = return "Frootmig"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup Address" 400 $ rsCode res
  where emailAddress = "hurling.frootmig@scrive.com"

testGodModeUserCannotDeleteRootUserGroupAddress :: TestEnv ()
testGodModeUserCannotDeleteRootUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Humma"
                                               , lastName       = return "Kavula"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup Address" 400 $ rsCode res
  where
    emailAddress = "humma.kavula@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotDeleteRootUserGroupAddress :: TestEnv ()
testSalesUserCannotDeleteRootUserGroupAddress = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiContactDetailsV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup Address" 400 $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup Settings GET endpoint tests

testNonGodModeUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testNonGodModeUserCannotViewNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Googleplex"
    , lastName  = return "Starthinker"
    , email     = return "googleplex.starthinker@scrive.com"
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "non-admin user can't view non-existent UserGroup Settings" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testGodModeUserCannotViewNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can't view non-existent UserGroup Settings" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotViewNonExistentUserGroupSettings :: TestEnv ()
testSalesUserCannotViewNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Hotblack"
                                               , lastName  = return "Desiato"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can't view non-existent UserGroup Settings" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testNonGodModeUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testNonGodModeUserCanViewUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Googleplex"
                                               , lastName       = return "Starthinker"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "non-admin user can view UserGroup Settings with permissions" 200
    $ rsCode res
  where emailAddress = "googleplex.starthinker@scrive.com"

testGodModeUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Grunthos"
                                               , lastName       = return "the Flatulent"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can view UserGroup Settings with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupSettingsWithPermissions :: TestEnv ()
testSalesUserCanViewUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hotblack"
                                               , lastName       = return "Desiato"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can view UserGroup Settings with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testGodModeUserCanViewUserGroupSettingsWithoutPermissions :: TestEnv ()
testGodModeUserCanViewUserGroupSettingsWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Grunthos"
                                               , lastName  = return "the Flatulent"
                                               , email     = return emailAddress
                                               }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "admin user can view UserGroup Settings with permissions" 200 $ rsCode res
  where
    emailAddress = "grunthos.the.flatulent@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanViewUserGroupSettingsWithoutPermissions :: TestEnv ()
testSalesUserCanViewUserGroupSettingsWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Hotblack"
    , lastName  = return "Desiato"
    , email     = return "hotblack.desiato@scrive.com"
    }
  ug <- instantiateRandomUserGroup
  let ugid = ug ^. #id
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Get ugid)
  assertEqual "sales user can view UserGroup Settings with permissions" 200 $ rsCode res
  where
    emailAddress = "hotblack.desiato@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup Settings Update endpoint tests

testSalesUserCanEditUserGroupSettingsWithPermissions :: TestEnv ()
testSalesUserCanEditUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Great Green"
                                               , lastName       = return "Arkleseizure"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest POST [("settings", inText settingsJson)]
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Update ugid)
  assertEqual "non-admin and non-sales edit UserGroupSettings with permissions" 200
    $ rsCode res
  where
    emailAddress = "great.green.arkleseizure@scrive.com"
    settingsJson =
      "{\"ip_address_mask_list\":[],\"data_retention_policy\":"
        <> "{\"idle_doc_timeout_preparation\":null,\"idle_doc_timeout_closed\":null,"
        <> "\"idle_doc_timeout_canceled\":null,\"idle_doc_timeout_timedout\":null,"
        <> "\"idle_doc_timeout_rejected\":null,\"idle_doc_timeout_error\":null,"
        <> "\"immediate_trash\":false},\"cgi_display_name\":\"a new display name\","
        <> "\"cgi_service_id\":null,\"sms_provider\":\"default\",\"pad_app_mode\":"
        <> "\"list_view\",\"pad_earchive_enabled\":true,\"legal_text\":false}"

-- UserGroup Settings DELETE endpoint tests

testUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testUserCannotDeleteNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Almighty"
                                               , lastName = return "Bob"
                                               , email = return "almighty.bob@scrive.com"
                                               }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
  where ugid = unsafeUserGroupID 123

testGodModeUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testGodModeUserCannotDeleteNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Blart"
                                               , lastName  = return "Versenwald III"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "blart.versenwald@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotDeleteNonExistentUserGroupSettings :: TestEnv ()
testSalesUserCannotDeleteNonExistentUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName = return "Gogrilla"
                                               , lastName  = return "Mincefriend"
                                               , email     = return emailAddress
                                               }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "user cannot delete non-existent UserGroup Settings" 403 $ rsCode res
  where
    ugid         = unsafeUserGroupID 123
    emailAddress = "gogrilla.mincefriend@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCanDeleteChildUserGroupSettingsWithPermissions :: TestEnv ()
testUserCanDeleteChildUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid   = user ^. #id
      ugid1 = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid1
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "UserGroup admin can delete child UserGroup Settings" 200 $ rsCode res
  where emailAddress = "hig.hurtenflurst@scrive.com"

testGodModeUserCanDeleteChildUserGroupSettingsWithoutPermissions :: TestEnv ()
testGodModeUserCanDeleteChildUserGroupSettingsWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "admin user can delete child UserGroup Settings without permissions" 200
    $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions :: TestEnv ()
testSalesUserCanDeleteChildUserGroupSettingsWithoutPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hig"
                                               , lastName       = return "Hurtenflurst"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid1 = user ^. #groupID
  ugid2 <- fmap (view #id) . instantiateUserGroup $ randomUserGroupTemplate
    { parentGroupID = Just ugid1
    }
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid2)
  assertEqual "sales user can delete child UserGroup Settings without permissions" 200
    $ rsCode res
  where
    emailAddress = "hig.hurtenflurst@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

testUserCannotDeleteRootUserGroupSettingsWithPermissions :: TestEnv ()
testUserCannotDeleteRootUserGroupSettingsWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Hurling"
                                               , lastName       = return "Frootmig"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "UserGroup admin can delete root UserGroup Settings" 400 $ rsCode res
  where emailAddress = "hurling.frootmig@scrive.com"

testGodModeUserCannotDeleteRootUserGroupSettings :: TestEnv ()
testGodModeUserCannotDeleteRootUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Humma"
                                               , lastName       = return "Kavula"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "admin user cannot delete root UserGroup Settings" 400 $ rsCode res
  where
    emailAddress = "humma.kavula@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]

testSalesUserCannotDeleteRootUserGroupSettings :: TestEnv ()
testSalesUserCannotDeleteRootUserGroupSettings = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Max"
                                               , lastName       = return "Quordlepleen"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let ugid = user ^. #groupID
  ctx <- setUser user <$> mkContext defaultLang
  req <- mkRequest DELETE []
  res <- fst <$> runTestKontra req ctx (userGroupApiSettingsV2Delete ugid)
  assertEqual "sales user cannot delete root UserGroup Settings" 400 $ rsCode res
  where
    emailAddress = "max.quordlepleen@scrive.com"
    setUser user = set #maybeUser (Just user) . set #salesAccounts [Email emailAddress]

-- UserGroup Users GET endpoint tests

testNonGodModeUserCanViewUsersInUserGroupWithPermissions :: TestEnv ()
testNonGodModeUserCanViewUsersInUserGroupWithPermissions = do
  user <- instantiateUser $ randomUserTemplate { firstName      = return "Googleplex"
                                               , lastName       = return "Starthinker"
                                               , email          = return emailAddress
                                               , isCompanyAdmin = True
                                               , signupMethod   = CompanyInvitation
                                               }
  let uid  = user ^. #id
      ugid = user ^. #groupID
  void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  req <- mkRequest GET []
  res <- fst <$> runTestKontra req ctx (userGroupApiUsersV2Get ugid)
  assertEqual "non-admin user can view Users in UserGroup with permissions" 200
    $ rsCode res
  where emailAddress = "googleplex.starthinker@scrive.com"

testUserCanCreateTagsOnRootUserGroup :: TestEnv ()
testUserCanCreateTagsOnRootUserGroup = do
  user       <- instantiateUser $ randomUserTemplate { email = return emailAddress }
  ctx        <- setUser user <$> mkContext defaultLang
  val        <- jsonTestRequestHelper ctx POST params userGroupApiV2Create 200
  outputTags <- lookupObjectArray "tags" val
  assertEqual "admin user can create tags" (length inputTags) (length outputTags)
  where
    emailAddress = "trillian@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]
    inputTags = [TagUpdate "foo" (SetTo "bar")]
    ug =
      object ["tags" .= toJSON inputTags, "name" .= toJSON ("testing user group" :: Text)]
    params = [("usergroup", valueToInput ug)]

testUserCanCreateTagsOnChildUserGroup :: TestEnv ()
testUserCanCreateTagsOnChildUserGroup = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { email = return emailAddress }
  let inputUg =
        object
          $ [ "tags" .= toJSON inputTags
            , "name" .= toJSON ("testing user group" :: Text)
            , "parent_id" .= toJSON (ug ^. #id)
            ]
  let params = [("usergroup", valueToInput inputUg)]
  ctx        <- setUser user <$> mkContext defaultLang
  val        <- jsonTestRequestHelper ctx POST params userGroupApiV2Create 200
  outputTags <- lookupObjectArray "tags" val
  assertEqual "admin user can create tags" (length inputTags) (length outputTags)
  where
    emailAddress = "trillian@scrive.com"
    setUser user = set #maybeUser (Just user) . set #adminAccounts [Email emailAddress]
    inputTags = [TagUpdate "foo" (SetTo "bar")]

valueToInput :: Value -> Input
valueToInput = inText . TE.decodeUtf8 . BSL.toStrict . encode

testUserCanUpdateTags :: TestEnv ()
testUserCanUpdateTags = do
  ug <-
    instantiateUserGroup $ randomUserGroupTemplate & #externalTags .~ return initialTags
  user <- instantiateUser $ randomUserTemplate { groupID = return $ ug ^. #id }
  let ugid = ug ^. #id
  void . dbUpdate . AccessControlCreateForUser (user ^. #id) $ UserGroupAdminAR ugid
  void . dbUpdate . UserGroupUpdate $ ug & #externalTags .~ initialTags
  ctx <- set #maybeUser (Just user) <$> mkContext defaultLang
  val <- jsonTestRequestHelper ctx
                               POST
                               [("usergroup", inText tagUpdateJson)]
                               (userGroupApiV2Update ugid)
                               200
  tags <- lookupObjectArray "tags" val
  assertEqual "user can update tags" expectUpdatedTags tags
  where
    tagUpdates =
      [ TagUpdate "legs" (SetTo "six")
      , TagUpdate "size" Delete
      , TagUpdate "eyes" (SetTo "big")
      ]
    tagUpdateJson =
      TE.decodeUtf8 . BSL.toStrict . encode $ object ["tags" .= toJSON tagUpdates]
    initialTags = S.fromList [Tag "legs" "four", Tag "size" "tiny", Tag "color" "black"]
    expectUpdatedTags =
      map toJSON [Tag "color" "black", Tag "eyes" "big", Tag "legs" "six"]

testUserCanViewTags :: TestEnv ()
testUserCanViewTags = do
  ug   <- instantiateRandomUserGroup
  user <- instantiateUser $ randomUserTemplate { groupID = return $ ug ^. #id }
  let ugid = ug ^. #id
  void . dbUpdate . AccessControlCreateForUser (user ^. #id) $ UserGroupAdminAR ugid
  ctx  <- set #maybeUser (Just user) <$> mkContext defaultLang
  val  <- jsonTestRequestHelper ctx GET [] (userGroupApiV2Get ugid) 200
  tags <- lookupObjectArray "tags" val
  assertEqual "user can view tags" (length $ ug ^. #externalTags) (length tags)
