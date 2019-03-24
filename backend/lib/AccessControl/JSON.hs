{-# LANGUAGE TemplateHaskell #-}
module AccessControl.JSON (
    unjsonAccessRole
) where

import Data.Unjson

import AccessControl.Types
import Folder.Model
import User.UserID
import UserGroup.Types
import Utils.TH

unjsonAccessRole :: UnjsonDef AccessRole
unjsonAccessRole = unjsonInvmapR
  ((maybe (fail errorMsg) return) . jsonToAccessRole)
  accessRoleToJSON
  unjsonAccessRoleJSON
    where
      errorMsg = "Can't parse AccessRole - Role type doesn't match target type"

unjsonAccessRoleJSON :: UnjsonDef AccessRoleJSON
unjsonAccessRoleJSON = objectOf $ pure AccessRoleJSON
  <*> fieldOpt "id" roleID "AccessRoleJSON role ID"
  <*> fieldOpt "is_generated" isGenerated "AccessRoleJSON is generated role?"
  <*> field "role_type" roleType "AccessRoleJSON role type"
  <*> fieldBy "source" source "AccessRoleJSON source" unjsonAccessRoleSourceJSON
  <*> fieldBy "target" target "AccessRoleJSON target" unjsonAccessRoleTargetJSON

unjsonAccessRoleSourceJSON :: UnjsonDef AccessRoleSourceJSON
unjsonAccessRoleSourceJSON = disjointUnionOf "type" [
    ("user", $(isConstr 'AccessRoleUserSourceJSON),
      pure AccessRoleUserSourceJSON
      <*> field "id" (\(AccessRoleUserSourceJSON uid) -> uid)
          "AccessRoleJSON source user id")
  , ("user_group", $(isConstr 'AccessRoleUserGroupSourceJSON),
      pure AccessRoleUserGroupSourceJSON
      <*> field "id" (\(AccessRoleUserGroupSourceJSON ugid) -> ugid)
          "AccessRoleJSON source user group ID")
  ]

unjsonAccessRoleTargetJSON :: UnjsonDef AccessRoleTargetJSON
unjsonAccessRoleTargetJSON = disjointUnionOf "type" [
    ("user", $(isConstr 'UserTargetJSON), pure UserTargetJSON
      <*> field "id" (\(UserTargetJSON uid) -> uid) "AccessRoleJSON user id")
  , ("user_group", $(isConstr 'UserGroupTargetJSON), pure UserGroupTargetJSON
      <*> field "id" (\(UserGroupTargetJSON ugid) -> ugid)
          "AccessRoleJSON user group ID")
  , ("folder", $(isConstr 'FolderTargetJSON), pure FolderTargetJSON
      <*> field "id" (\(FolderTargetJSON fid) -> fid)
          "AccessRoleJSON folder ID")
  ]

data AccessRoleJSON = AccessRoleJSON {
    roleID :: Maybe AccessRoleID
  , isGenerated :: Maybe Bool
  , roleType :: AccessRoleType
  , source :: AccessRoleSourceJSON
  , target :: AccessRoleTargetJSON
  }

data AccessRoleSourceJSON
  = AccessRoleUserSourceJSON UserID
  | AccessRoleUserGroupSourceJSON UserGroupID

data AccessRoleTargetJSON
  = UserTargetJSON UserID
  | UserGroupTargetJSON UserGroupID
  | FolderTargetJSON FolderID

accessRoleToJSON :: AccessRole -> AccessRoleJSON
accessRoleToJSON (AccessRoleUser rid uid target) = AccessRoleJSON {
    roleID = Just rid
  , isGenerated = Just False
  , roleType = toAccessRoleType target
  , source = AccessRoleUserSourceJSON uid
  , target = accessRoleTargetToJSON target
  }
accessRoleToJSON (AccessRoleUserGroup rid ugid target) = AccessRoleJSON {
    roleID = Just rid
  , isGenerated = Just False
  , roleType = toAccessRoleType target
  , source = AccessRoleUserGroupSourceJSON ugid
  , target = accessRoleTargetToJSON target
  }
accessRoleToJSON (AccessRoleImplicitUser uid target) = AccessRoleJSON {
    roleID = Nothing
  , isGenerated = Just True
  , roleType = toAccessRoleType target
  , source = AccessRoleUserSourceJSON uid
  , target = accessRoleTargetToJSON target
  }
accessRoleToJSON (AccessRoleImplicitUserGroup ugid target) = AccessRoleJSON {
    roleID = Nothing
  , isGenerated = Just True
  , roleType = toAccessRoleType target
  , source = AccessRoleUserGroupSourceJSON ugid
  , target = accessRoleTargetToJSON target
  }

accessRoleTargetToJSON :: AccessRoleTarget -> AccessRoleTargetJSON
accessRoleTargetToJSON (UserAR uid) = UserTargetJSON uid
accessRoleTargetToJSON (UserGroupMemberAR ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (UserAdminAR ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (UserGroupAdminAR ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (DocumentAdminAR fid) = FolderTargetJSON fid

jsonToAccessRole :: AccessRoleJSON -> Maybe AccessRole
jsonToAccessRole roleJson = constructor <$> roleTarget
  where
    constructor = case (roleID roleJson, source roleJson) of
      (Just rid, AccessRoleUserSourceJSON uid) -> AccessRoleUser rid uid
      (Just rid, AccessRoleUserGroupSourceJSON ugid)
        -> AccessRoleUserGroup rid ugid
      (Nothing, AccessRoleUserSourceJSON uid) -> AccessRoleImplicitUser uid
      (Nothing, AccessRoleUserGroupSourceJSON ugid)
        -> AccessRoleImplicitUserGroup ugid
    roleTarget = case target roleJson of
      UserTargetJSON uid -> case roleType roleJson of
        UserART -> Just $ UserAR uid
        _ -> Nothing
      UserGroupTargetJSON ugid -> case roleType roleJson of
        UserAdminART -> Just $ UserGroupMemberAR ugid
        UserGroupAdminART -> Just $ UserAdminAR ugid
        UserGroupMemberART -> Just $ UserGroupAdminAR ugid
        _ -> Nothing
      FolderTargetJSON fid -> case roleType roleJson of
        DocumentAdminART -> Just $ DocumentAdminAR fid
        _ -> Nothing
