module UserGroup.JSON.Settings (
    userGroupSettingsToResponse
  , userGroupSettingsToJSON
  , updateUserGroupDataRetentionFromResponse
  , DataRetentionPolicyResponseJSON
  , DataRetentionPolicyRequestJSON
  , unjsonDataRetentionPolicyResponseJSON
) where

import Data.Aeson
import Data.Unjson

import DataRetentionPolicy (DataRetentionPolicy, unjsonDataRetentionPolicy)
import UserGroup.Types

userGroupSettingsToResponse
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> Maybe (UserGroupID, UserGroupSettings)
  -> (UnjsonDef DataRetentionPolicyResponseJSON, DataRetentionPolicyResponseJSON)
userGroupSettingsToResponse mInheritId settingsObj mInheritPreview
  = ( unjsonDataRetentionPolicyResponseJSON
    , userGroupSettingsToJSON mInheritId settingsObj mInheritPreview
    )

userGroupSettingsToJSON
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> Maybe (UserGroupID, UserGroupSettings)
  -> DataRetentionPolicyResponseJSON
userGroupSettingsToJSON mInheritId settingsObj mInheritPreview
  = DataRetentionPolicyResponseJSON {
    resInheritedFrom = mInheritId
  , resDataRetentionPolicyObject = get ugsDataRetentionPolicy settingsObj
  , resInheritPreview = case mInheritPreview of
      Nothing -> Nothing
      Just (ugid, settings) -> Just (ugid, get ugsDataRetentionPolicy settings)
  }

updateUserGroupDataRetentionFromResponse
  :: DataRetentionPolicy
  -> Value
  -> Maybe DataRetentionPolicy
updateUserGroupDataRetentionFromResponse ugSett settingsChanges =
  let ugSettReq = dataRetentionPolicyToRequest ugSett
  in case update ugSettReq unjsonDataRetentionPolicyRequestJSON settingsChanges of
    (Result ugSettUpdated []) -> Just $ dataRetentionPolicyFromRequest ugSettUpdated
    (Result _ _) -> Nothing

dataRetentionPolicyToRequest :: DataRetentionPolicy -> DataRetentionPolicyRequestJSON
dataRetentionPolicyToRequest dataRetention = DataRetentionPolicyRequestJSON {
    reqDataRetentionPolicyObject = dataRetention
  }

dataRetentionPolicyFromRequest :: DataRetentionPolicyRequestJSON -> DataRetentionPolicy
dataRetentionPolicyFromRequest = reqDataRetentionPolicyObject

unjsonDataRetentionPolicyResponseJSON :: UnjsonDef DataRetentionPolicyResponseJSON
unjsonDataRetentionPolicyResponseJSON = objectOf $ pure DataRetentionPolicyResponseJSON
  <*> fieldOpt "inherited_from" resInheritedFrom
    "User Group Settings Response Inherited From ID"
  <*  fieldReadonly "can_inherit" canInherit "User Group Settings Response Can Inherit"
  <*> fieldBy "data_retention_policy" resDataRetentionPolicyObject
    "User Group Settings Data Retention Policy" unjsonDataRetentionPolicy
  <*> fieldOptBy "inherit_preview" resInheritPreview
    "User Group Settings Response Inherit Preview"
    unjsonDataRetentionPolicyInheritPreviewJSON
      where
        canInherit = isJust . resInheritedFrom || isJust . resInheritPreview

unjsonDataRetentionPolicyRequestJSON :: UnjsonDef DataRetentionPolicyRequestJSON
unjsonDataRetentionPolicyRequestJSON = objectOf $ pure DataRetentionPolicyRequestJSON
  <*> fieldBy "data_retention_policy" reqDataRetentionPolicyObject desc unjsonDataRetentionPolicy
    where
      desc = "User Group Settings Data Retention Policy"

unjsonDataRetentionPolicyInheritPreviewJSON :: UnjsonDef (UserGroupID, DataRetentionPolicy)
unjsonDataRetentionPolicyInheritPreviewJSON = objectOf $ pure (,)
  <*> field "inherited_from" fst
    "User Group Settings Response Inherit Preview Inherit From ID"
  <*> fieldBy "data_retention_policy" snd
    "User Group Settings Response Inherit Preview Data Retention Policy"
    unjsonDataRetentionPolicy

data DataRetentionPolicyRequestJSON = DataRetentionPolicyRequestJSON {
    reqDataRetentionPolicyObject :: DataRetentionPolicy
  }

data DataRetentionPolicyResponseJSON = DataRetentionPolicyResponseJSON {
    resInheritedFrom         :: Maybe UserGroupID
  , resDataRetentionPolicyObject :: DataRetentionPolicy
  , resInheritPreview        :: Maybe (UserGroupID, DataRetentionPolicy)
  }
