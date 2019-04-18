{-# LANGUAGE TemplateHaskell #-}
module UserGroup.JSON (
    userGroupToResponse
  , updateUserGroupFromResponse
  , UserGroupResponseJSON
  -- exports from UserGroup.JSON.Address
  , userGroupAddressToResponse
  , updateUserGroupAddressFromResponse
  -- exports from UserGroup.JSON.Settings
  , userGroupSettingsToResponse
  , updateUserGroupDataRetentionFromResponse
) where

import Data.Aeson
import Data.Text hiding (map)
import Data.Unjson

import UserGroup.JSON.Address
import UserGroup.JSON.Settings
import UserGroup.Types

userGroupToResponse
  :: UserGroup
  -> (Maybe UserGroupID, UserGroupAddress, Maybe (UserGroupID, UserGroupAddress))
  -> (Maybe UserGroupID, UserGroupSettings, Maybe (UserGroupID, UserGroupSettings))
  -> [(UserGroupID, Text)]
  -> (UnjsonDef UserGroupResponseJSON, UserGroupResponseJSON)
userGroupToResponse ug ugAddr ugSett children
  = ( unjsonUserGroupResponseJSON
    , UserGroupResponseJSON {
        resID = get ugID ug
      , resParentID = get ugParentGroupID ug
      , resName = get ugName ug
      , resChildren = children
      , resAddress =
          let (mInheritId, addressObj, mInheritPreview) = ugAddr
          in userGroupAddressToJSON mInheritId addressObj mInheritPreview
      , resSettings =
          let (mInheritId, settingsObj, mInheritPreview) = ugSett
          in userGroupSettingsToJSON mInheritId settingsObj mInheritPreview
      }
    )

updateUserGroupFromResponse :: UserGroup -> Value -> Maybe UserGroup
updateUserGroupFromResponse ug ugChanges =
  let ugReq = userGroupToRequest ug
  in case update ugReq unjsonUserGroupRequestJSON ugChanges of
    (Result ugUpdated []) -> Just $ userGroupFromRequest ug ugUpdated
    (Result _ _) -> Nothing

userGroupToRequest :: UserGroup -> UserGroupRequestJSON
userGroupToRequest ug = UserGroupRequestJSON {
    reqParentID = get ugParentGroupID ug
  , reqName = get ugName ug
  }

userGroupFromRequest :: UserGroup -> UserGroupRequestJSON -> UserGroup
userGroupFromRequest baseUg req = baseUg {
    _ugParentGroupID = reqParentID req
  , _ugName = reqName req
  }

unjsonUserGroupResponseJSON :: UnjsonDef UserGroupResponseJSON
unjsonUserGroupResponseJSON = objectOf $ pure UserGroupResponseJSON
  <*> field "id" resID "User Group ID"
  <*> fieldOpt "parent_id" resParentID "User Group Parent ID"
  <*> field "name" resName "User Group Name"
  <*> fieldBy "children" resChildren "User Group Children"
    (arrayOf unjsonUserGroupChild)
  <*> fieldBy "address" resAddress "User Group Address"
    unjsonUserGroupAddressResponseJSON
  <*> fieldBy "settings" resSettings "User Group Settings"
    unjsonDataRetentionPolicyResponseJSON

unjsonUserGroupRequestJSON :: UnjsonDef UserGroupRequestJSON
unjsonUserGroupRequestJSON = objectOf $ pure UserGroupRequestJSON
  <*> fieldOpt "parent_id" reqParentID "User Group ID"
  <*> field "name" reqName "User Group Name"

unjsonUserGroupChild :: UnjsonDef (UserGroupID, Text)
unjsonUserGroupChild = objectOf $ pure (,)
  <*> field "id" fst "User Group Child ID"
  <*> field "name" snd "User Group Child Name"

data UserGroupResponseJSON = UserGroupResponseJSON {
    resID          :: UserGroupID
  , resParentID    :: Maybe UserGroupID
  , resName        :: Text
  , resChildren    :: [(UserGroupID, Text)]
  , resAddress     :: UserGroupAddressResponseJSON
  , resSettings    :: DataRetentionPolicyResponseJSON
  }

data UserGroupRequestJSON = UserGroupRequestJSON {
    reqParentID    :: Maybe UserGroupID
  , reqName        :: Text
  }
