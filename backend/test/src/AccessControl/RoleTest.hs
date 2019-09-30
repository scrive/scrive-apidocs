{-# LANGUAGE OverloadedStrings #-}
module AccessControl.RoleTest (accessControlRoleTests) where

import Test.Framework
import Test.QuickCheck

import AccessControl.Model
import AccessControl.Types
import DB
import Folder.Model
import TestingUtil
import TestKontra
import User.Model
import UserGroup.Model
import UserGroup.Types

accessControlRoleTests :: TestEnvSt -> Test
accessControlRoleTests env = testGroup "AccessControlRoles"
  [ testThat "User's roles do not trickle down the user group tree"
      env testRolesNotInheritedInUserGroupTree
  , testThat "Test inheriting implicit roles work" env testImplicitRoles
  , testThat "Test inheriting explicit roles work" env testExplicitRoles
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
  [grp_role] <- (filter byUserGroup) <$>
    (dbQuery $ AccessControlGetExplicitRoles uid root_ugid)
  assertBool "The role set on a parent group is not included in user's roles"
             (not $ grp_role `elem` userRoles1)

  void $ dbUpdate $ SetUserGroup (userid user) (Just $ root_ugid)
  -- user's group changed, need to re-retrieve the user
  (Just user'') <- dbQuery $ GetUserByID uid
  userRoles2 <- dbQuery $ GetRoles user''
  assertBool "The role set on the user's group is indeed included in user's roles"
             (grp_role `elem` userRoles2)
  where
    byUserGroup :: AccessRole -> Bool
    byUserGroup r =
      case r of
       -- we care only about explicit (persisted) roles
       AccessRoleUserGroup _ _ _       -> True
       _                               -> False

    createChildGroups :: UserGroupID -> TestEnv [UserGroup]
    createChildGroups root_ugid' = do
      ugrand0 <- rand 10 arbitrary
      ugrand1 <- rand 10 arbitrary
      ug0 <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just root_ugid') $ ugrand0
      ug1 <- dbUpdate . UserGroupCreate . set ugParentGroupID (Just (get ugID ug0)) $ ugrand1
      return $ [ug0, ug1]

testImplicitRoles :: TestEnv ()
testImplicitRoles = do
  -- admin user
  admusr <- fromJust <$> addNewUser "Lego" "Flash" "lego.flash@scrive.com"
  let admusrid = userid admusr
      admugid = usergroupid admusr
  admug <- fromJust <$> (dbQuery $ UserGroupGet admugid)
  -- ordinary user
  --              admug              . . . admugfdr
  --               /                          /
  --              /    ( \o/ admusr) . . . admfdr
  --             /
  --            /
  --         usrug                   . . . usrughomefolder
  --                                            /
  --                   ( \o/ usr)    . . . usrfdr

  usrughomefdr <- dbUpdate $ FolderCreate defaultFolder
  usrug <- dbUpdate $ UserGroupCreate $
                        (set ugParentGroupID $ Just admugid) .
                        (set ugHomeFolderID $ Just (get folderID usrughomefdr)) $
                        defaultUserGroup
  usr <- fromJust <$> addNewCompanyUser' DontMakeAdmin "Lego" "Batman"
                                         "lego.batman@scrive.com" (get ugID usrug)

  let admugfdrid = (fromJust $ get ugHomeFolderID admug)
      admfdrid = (fromJust $ userhomefolderid admusr)
      usrid = userid usr
      usrughomefdrid = fromJust $ get ugHomeFolderID usrug
      usrugid = (get ugID usrug)
      usrfdrid = fromJust $ userhomefolderid usr

  -- We need some descendants of the user user group and folder when testing role
  -- inheritance for the user.

  --              admug              . . . admugfdr
  --               /                          /
  --              /      \o/ admusr  . . . admfdr
  --             /
  --            /
  --         usrug                   . . . usrughomefolder
  --            /                                /
  --           /         \o/ usr     . . . usrfdr
  --          /                              /
  --         /                              /
  --      childug                    . . . childfdr

  --
  let childfdr = set folderParentID (Just usrfdrid) defaultFolder
      childug = set ugParentGroupID (Just usrugid) defaultUserGroup
  childfdrid <- get folderID <$> (dbUpdate $ FolderCreate  childfdr)
  childugid <- get ugID <$> (dbUpdate $ UserGroupCreate childug)

  -- let's check inheritance for the admin
  let inheritForUserGroupRoleAdm r = map r [admugid, usrugid, childugid]
      inheritForFolderRoleAdm r = map r [admugfdrid, admfdrid]
      shouldBeInherited = (AccessRoleImplicitUser admusrid) <$>
                            concatMap inheritForUserGroupRoleAdm
                                      [ UserAdminAR ] <>
                            concatMap inheritForFolderRoleAdm
                                      [ FolderAdminAR
                                      , DocumentAfterPreparationAdminAR
                                      , SharedTemplateUserAR ] <>
                            [ FolderUserAR admfdrid
                            , DocumentAdminAR admfdrid
                            , UserAR admusrid ]

  admroles <- nub <$> (dbQuery $ GetRolesIncludingInherited admusr admug)
  assertBool "The implicit roles for company admins are inherited as intended"
              (  [] == admroles \\ shouldBeInherited
              && [] == shouldBeInherited \\ admroles)

  -- now, let's check inheritance for the user
  usrRoles <- nub <$> (dbQuery $ GetRolesIncludingInherited usr usrug)

  let inheritForUserGroupRoleUsr r = map r [ usrugid, childugid ]
      inheritForFolderRoleUsr r = map r [ usrfdrid, childfdrid ]
      shouldBeInheritedUsr = (AccessRoleImplicitUser usrid) <$>
                               concatMap inheritForUserGroupRoleUsr
                                         [ UserGroupMemberAR ] <>
                               concatMap inheritForFolderRoleUsr
                                         [ SharedTemplateUserAR
                                         , DocumentAdminAR
                                         , FolderUserAR ] <>
                               [ SharedTemplateUserAR usrughomefdrid
                               -- ^ special case since code would be ugly otherwise
                               , UserAR usrid]
  assertBool "The implicit roles for company users are inherited as intended"
              (  [] == usrRoles \\ shouldBeInheritedUsr
              && [] == shouldBeInheritedUsr \\ usrRoles)

testExplicitRoles :: TestEnv ()
testExplicitRoles = do
  admusr <- fromJust <$> addNewUser "Lego" "Superman" "lego.superman@scrive.com"
  let admusrid = userid admusr
      admugid = usergroupid admusr
  admug <- fromJust <$> (dbQuery $ UserGroupGet admugid)

  -- the test groups and folders - we test on unrelated structures as per the
  -- illustration:
  --
  --       admug              . . . testfdr
  --                                   /
  --              \o/ admusr  . . . testchildfdr
  --
  --                          . . . testug
  --                                  /
  --                          . . . testchildug
  --

  testugid <- get ugID <$> (dbUpdate $ UserGroupCreate defaultUserGroup)
  let testchildug = set ugParentGroupID (Just testugid) defaultUserGroup
  testchildugid <- get ugID <$> (dbUpdate $ UserGroupCreate testchildug)

  testfdrid <- get folderID <$> (dbUpdate $ FolderCreate defaultFolder)
  let testchildfdr = set folderParentID (Just testfdrid) defaultFolder
  testchildfdrid <- get folderID <$> (dbUpdate $ FolderCreate testchildfdr)

  -- let's set some roles
  let explicitroles = [ FolderAdminAR testfdrid
                      , UserGroupAdminAR testugid ]
  mapM_ (dbUpdate . (AccessControlCreateForUser admusrid)) explicitroles

  -- let's check inheritance for the admin
  let inheritForUserGroupRole r = map r [testugid, testchildugid]
      inheritForFolderRole r    = map r [testfdrid, testchildfdrid]
      explicitShouldBeInherited = (AccessRoleUser (unsafeAccessRoleID 0) admusrid) <$>
                                    concatMap inheritForUserGroupRole
                                              [ UserGroupAdminAR ] <>
                                    concatMap inheritForFolderRole
                                              [ FolderAdminAR ]

  admroles <- dbQuery $ GetRolesIncludingInherited admusr admug
  -- because we get an id in the access roles from the db we set them all to zero so we
  -- can compare easier later.
  let explicitOnlyNormalised = catMaybes $ map normalise admroles
      zeroid = unsafeAccessRoleID 0
      normalise r = case r of
                      AccessRoleUser _ x y      -> Just (AccessRoleUser zeroid x y)
                      AccessRoleUserGroup _ x y -> Just (AccessRoleUserGroup zeroid x y)
                      _                         -> Nothing

  assertBool "The explicit roles are inherited as intended"
              (  [] == explicitShouldBeInherited \\ explicitOnlyNormalised
              && [] == explicitOnlyNormalised \\ explicitShouldBeInherited)
