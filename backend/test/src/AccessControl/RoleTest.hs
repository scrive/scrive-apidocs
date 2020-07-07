{-# LANGUAGE OverloadedStrings #-}
module AccessControl.RoleTest (accessControlRoleTests) where

import Data.List.Extra (nubOrd)
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
accessControlRoleTests env = testGroup
  "AccessControlRoles"
  [ testThat "User's roles do not trickle down the user group tree"
             env
             testRolesNotInheritedInUserGroupTree
  , testThat "Test inheriting implicit roles work" env testImplicitRoles
  , testThat "Test inheriting explicit roles work" env testExplicitRoles
  ]

testRolesNotInheritedInUserGroupTree :: TestEnv ()
testRolesNotInheritedInUserGroupTree = do
  user <- instantiateUser $ randomUserTemplate
    { firstName = return "Lloyd"
    , lastName  = return "Garmadon"
    , email     = return "lloyd.garmadon@scrive.com"
    }
  let uid = user ^. #id
  (root_ug :: UserGroupRoot) <- rand 10 arbitrary
  root_ugid <- view #id <$> (dbUpdate . UserGroupCreate $ ugFromUGRoot root_ug)
  [_ug0, ug1] <- createChildGroups root_ugid
  void . dbUpdate $ SetUserGroup uid (Just $ ug1 ^. #id)
  -- user's group changed, need to re-retrieve the user
  (Just user') <- dbQuery $ GetUserByID uid
  let role_trg = UserGroupAdminAR root_ugid
  void . dbUpdate $ AccessControlCreateForUserGroup root_ugid role_trg
  userRoles1 <- dbQuery $ GetRoles user'
  [grp_role] <- filter byUserGroup
    <$> dbQuery (AccessControlGetExplicitRoles uid root_ugid)
  assertBool "The role set on a parent group is not included in user's roles"
             (grp_role `notElem` userRoles1)

  void . dbUpdate $ SetUserGroup (user ^. #id) (Just root_ugid)
  -- user's group changed, need to re-retrieve the user
  (Just user'') <- dbQuery $ GetUserByID uid
  userRoles2    <- dbQuery $ GetRoles user''
  assertBool "The role set on the user's group is indeed included in user's roles"
             (grp_role `elem` userRoles2)
  where
    byUserGroup :: AccessRole -> Bool
    byUserGroup r = case r of
       -- we care only about explicit (persisted) roles
      AccessRoleUserGroup{} -> True
      _                     -> False

    createChildGroups :: UserGroupID -> TestEnv [UserGroup]
    createChildGroups root_ugid' = do
      ugrand0 <- rand 10 arbitrary
      ugrand1 <- rand 10 arbitrary
      ug0     <- dbUpdate . UserGroupCreate $ set #parentGroupID (Just root_ugid') ugrand0
      ug1 <- dbUpdate . UserGroupCreate $ set #parentGroupID (Just (ug0 ^. #id)) ugrand1
      return [ug0, ug1]

testImplicitRoles :: TestEnv ()
testImplicitRoles = do
  -- admin user
  admusr <- instantiateUser $ randomUserTemplate { firstName      = return "Lego"
                                                 , lastName       = return "Flash"
                                                 , email = return "lego.flash@scrive.com"
                                                 , isCompanyAdmin = True
                                                 }
  let admusrid = admusr ^. #id
      admugid  = admusr ^. #groupID
  admug        <- fromJust <$> dbQuery (UserGroupGet admugid)
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
  usrug        <-
    dbUpdate
    . UserGroupCreate
    . set #parentGroupID (Just admugid)
    . set #homeFolderID  (Just (usrughomefdr ^. #id))
    $ defaultUserGroup
  usr <- instantiateUser $ randomUserTemplate { groupID = return $ usrug ^. #id }

  let admugfdrid     = fromJust $ admug ^. #homeFolderID
      admfdrid       = fromJust $ admusr ^. #homeFolderID
      usrid          = usr ^. #id
      usrughomefdrid = fromJust $ usrug ^. #homeFolderID
      usrugid        = usrug ^. #id
      usrfdrid       = fromJust $ usr ^. #homeFolderID

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
  let childfdr = set #parentID (Just usrfdrid) defaultFolder
      childug  = set #parentGroupID (Just usrugid) defaultUserGroup
  childfdrid <- view #id <$> dbUpdate (FolderCreate childfdr)
  childugid  <- view #id <$> dbUpdate (UserGroupCreate childug)

  -- let's check inheritance for the admin
  let inheritForUserGroupRoleAdm r = map r [admugid, usrugid, childugid]
      inheritForFolderRoleAdm r = map r [admugfdrid, admfdrid]
      shouldBeInherited =
        AccessRoleImplicitUser admusrid
          <$> concatMap inheritForUserGroupRoleAdm [UserAdminAR]
          <>  concatMap inheritForFolderRoleAdm [FolderAdminAR, SharedTemplateUserAR]
          <>  [FolderUserAR admfdrid, UserAR admusrid]

  admroles <- nubOrd <$> dbQuery (GetRolesIncludingInherited admusr admug)
  assertBool
    "The implicit roles for company admins are inherited as intended"
    (null (admroles \\ shouldBeInherited) && null (shouldBeInherited \\ admroles))

  -- now, let's check inheritance for the user
  usrRoles <- nubOrd <$> dbQuery (GetRolesIncludingInherited usr usrug)

  let inheritForUserGroupRoleUsr r = map r [usrugid, childugid]
      inheritForFolderRoleUsr r = map r [usrfdrid, childfdrid]
      shouldBeInheritedUsr =
        AccessRoleImplicitUser usrid
          <$> concatMap inheritForUserGroupRoleUsr [UserGroupMemberAR]
          <>  concatMap inheritForFolderRoleUsr    [SharedTemplateUserAR, FolderUserAR]
          <>  [ SharedTemplateUserAR usrughomefdrid
                               -- special case since code would be ugly otherwise
              , UserAR usrid
              ]
  assertBool
    "The implicit roles for company users are inherited as intended"
    (null (usrRoles \\ shouldBeInheritedUsr) && null (shouldBeInheritedUsr \\ usrRoles))

testExplicitRoles :: TestEnv ()
testExplicitRoles = do
  admusr <- instantiateUser $ randomUserTemplate
    { firstName = return "Lego"
    , lastName  = return "Superman"
    , email     = return "lego.superman@scrive.com"
    }
  let admusrid = admusr ^. #id
      admugid  = admusr ^. #groupID
  admug    <- fromJust <$> dbQuery (UserGroupGet admugid)

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

  testugid <- view #id <$> dbUpdate (UserGroupCreate defaultUserGroup)
  let testchildug = set #parentGroupID (Just testugid) defaultUserGroup
  testchildugid <- view #id <$> dbUpdate (UserGroupCreate testchildug)

  testfdrid     <- view #id <$> dbUpdate (FolderCreate defaultFolder)
  let testchildfdr = set #parentID (Just testfdrid) defaultFolder
  testchildfdrid <- view #id <$> dbUpdate (FolderCreate testchildfdr)

  -- let's set some roles
  let explicitroles = [FolderAdminAR testfdrid, UserGroupAdminAR testugid]
  mapM_ (dbUpdate . AccessControlCreateForUser admusrid) explicitroles

  -- let's check inheritance for the admin
  let inheritForUserGroupRole r = map r [testugid, testchildugid]
      inheritForFolderRole r = map r [testfdrid, testchildfdrid]
      explicitShouldBeInherited =
        AccessRoleUser (unsafeAccessRoleID 0) admusrid
          <$> concatMap inheritForUserGroupRole [UserGroupAdminAR]
          <>  concatMap inheritForFolderRole    [FolderAdminAR]

  admroles <- dbQuery $ GetRolesIncludingInherited admusr admug
  -- because we get an id in the access roles from the db we set them all to zero so we
  -- can compare easier later.
  let explicitOnlyNormalised = mapMaybe normalise admroles
      zeroid                 = unsafeAccessRoleID 0
      normalise r = case r of
        AccessRoleUser _ x y -> Just (AccessRoleUser zeroid x y)
        AccessRoleUserGroup _ x y -> Just (AccessRoleUserGroup zeroid x y)
        _ -> Nothing

  assertBool
    "The explicit roles are inherited as intended"
    (  null (explicitShouldBeInherited \\ explicitOnlyNormalised)
    && null (explicitOnlyNormalised \\ explicitShouldBeInherited)
    )
