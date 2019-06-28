{-# LANGUAGE TemplateHaskell #-}
module UserGroup.JSON (
    userGroupToResponse
  , userGroupWithInheritableToResponse
  , updateUserGroupFromRequest
  , UserGroupResponseJSON
  , userGroupContactDetailsToResponse
  , userGroupContactDetailsWithInheritableToResponse
  , updateUserGroupContactDetailsFromRequest
  , userGroupSettingsToResponse
  , userGroupSettingsWithInheritableToResponse
  , updateUserGroupDataRetentionFromRequest
) where

import Data.Aeson
import Data.Text hiding (map)
import Data.Unjson

import InputValidation
import UserGroup.JSON.ContactDetails
import UserGroup.JSON.Settings
import UserGroup.Types

userGroupToResponse
  :: UserGroup
  -> (Maybe UserGroupID, UserGroupAddress)
  -> (Maybe UserGroupID, UserGroupSettings)
  -> [(UserGroupID, Text)]
  -> (UnjsonDef UserGroupResponseJSON, UserGroupResponseJSON)
userGroupToResponse ug ugAddr ugSett children
  = ( unjsonUserGroupResponseJSON
    , UserGroupResponseJSON {
        resID = get ugID ug
      , resParentID = get ugParentGroupID ug
      , resName = get ugName ug
      , resChildren = children
      , resContactDetails =
          let (mInheritId, addressObj) = ugAddr
          in userGroupContactDetailsToJSON mInheritId addressObj
      , resSettings =
          let (mInheritId, settingsObj) = ugSett
          in userGroupSettingsToJSON mInheritId settingsObj
      }
    )

userGroupWithInheritableToResponse
  :: UserGroup
  -> (Maybe UserGroupID, UserGroupAddress, Maybe (UserGroupID, UserGroupAddress))
  -> (Maybe UserGroupID, UserGroupSettings, Maybe (UserGroupID, UserGroupSettings))
  -> [(UserGroupID, Text)]
  -> (UnjsonDef UserGroupResponseJSON, UserGroupResponseJSON)
userGroupWithInheritableToResponse ug ugAddr ugSett children
  = ( unjsonUserGroupWithInheritableResponseJSON
    , UserGroupResponseJSON {
        resID = get ugID ug
      , resParentID = get ugParentGroupID ug
      , resName = get ugName ug
      , resChildren = children
      , resContactDetails =
          let (mInheritId, addressObj, mInheritPreview) = ugAddr
          in userGroupContactDetailsWithInheritableToJSON mInheritId addressObj mInheritPreview
      , resSettings =
          let (mInheritId, settingsObj, mInheritPreview) = ugSett
          in userGroupSettingsWithInheritableToJSON mInheritId settingsObj mInheritPreview
      }
    )

updateUserGroupFromRequest :: UserGroup -> Value -> Maybe UserGroup
updateUserGroupFromRequest ug ugChanges =
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
  <*> fieldBy "name" resName "User Group Name"
      (unjsonWithValidationOrEmptyText asValidCompanyName)
  <*> fieldBy "children" resChildren "User Group Children"
      (arrayOf unjsonUserGroupChild)
  <*> fieldBy "contact_details" resContactDetails "User Group Contact Details"
      unjsonUserGroupContactDetailsResponseJSON
  <*> fieldBy "settings" resSettings "User Group Settings"
      unjsonDataRetentionPolicyResponseJSON

unjsonUserGroupWithInheritableResponseJSON :: UnjsonDef UserGroupResponseJSON
unjsonUserGroupWithInheritableResponseJSON = objectOf $ pure UserGroupResponseJSON
  <*> field "id" resID "User Group ID"
  <*> fieldOpt "parent_id" resParentID "User Group Parent ID"
  <*> field "name" resName "User Group Name"
  <*> fieldBy "children" resChildren "User Group Children"
      (arrayOf unjsonUserGroupChild)
  <*> fieldBy "contact_details" resContactDetails "User Group Contact Details"
      unjsonUserGroupContactDetailsResponseWithInheritableJSON
  <*> fieldBy "settings" resSettings "User Group Settings"
      unjsonDataRetentionPolicyWithInheritableResponseJSON

unjsonUserGroupRequestJSON :: UnjsonDef UserGroupRequestJSON
unjsonUserGroupRequestJSON = objectOf $ pure UserGroupRequestJSON
  <*> fieldOpt "parent_id" reqParentID "User Group ID"
  <*> field "name" reqName "User Group Name"

unjsonUserGroupChild :: UnjsonDef (UserGroupID, Text)
unjsonUserGroupChild = objectOf $ pure (,)
  <*> field "id" fst "User Group Child ID"
  <*> field "name" snd "User Group Child Name"

data UserGroupResponseJSON = UserGroupResponseJSON {
    resID             :: UserGroupID
  , resParentID       :: Maybe UserGroupID
  , resName           :: Text
  , resChildren       :: [(UserGroupID, Text)]
  , resContactDetails :: UserGroupContactDetailsResponseJSON
  , resSettings       :: DataRetentionPolicyResponseJSON
  }

data UserGroupRequestJSON = UserGroupRequestJSON {
    reqParentID    :: Maybe UserGroupID
  , reqName        :: Text
  }
