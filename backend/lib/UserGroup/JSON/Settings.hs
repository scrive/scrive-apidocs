module UserGroup.JSON.Settings (
    userGroupSettingsToResponse
  , userGroupSettingsWithInheritableToResponse
  , userGroupSettingsToJSON
  , userGroupSettingsWithInheritableToJSON
  , updateUserGroupDataRetentionFromRequest
  , DataRetentionPolicyResponseJSON
  , DataRetentionPolicyRequestJSON
  , unjsonDataRetentionPolicyResponseJSON
  , unjsonDataRetentionPolicyWithInheritableResponseJSON
) where

import Data.Aeson
import Data.Unjson

import DataRetentionPolicy (DataRetentionPolicy, unjsonDataRetentionPolicy)
import UserGroup.Types

userGroupSettingsToResponse
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> (UnjsonDef DataRetentionPolicyResponseJSON, DataRetentionPolicyResponseJSON)
userGroupSettingsToResponse mInheritId settingsObj
  = ( unjsonDataRetentionPolicyResponseJSON
    , userGroupSettingsToJSON mInheritId settingsObj
    )

userGroupSettingsWithInheritableToResponse
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> Maybe (UserGroupID, UserGroupSettings)
  -> (UnjsonDef DataRetentionPolicyResponseJSON, DataRetentionPolicyResponseJSON)
userGroupSettingsWithInheritableToResponse mInheritId settingsObj mInheritPreview
  = ( unjsonDataRetentionPolicyWithInheritableResponseJSON
    , userGroupSettingsWithInheritableToJSON mInheritId settingsObj mInheritPreview
    )

userGroupSettingsToJSON
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> DataRetentionPolicyResponseJSON
userGroupSettingsToJSON mInheritId settingsObj
  = DataRetentionPolicyResponseJSON {
    resInheritedFrom = mInheritId
  , resDataRetentionPolicyObject = get ugsDataRetentionPolicy settingsObj
  , resInheritPreview = Nothing
  }

userGroupSettingsWithInheritableToJSON
  :: Maybe UserGroupID
  -> UserGroupSettings
  -> Maybe (UserGroupID, UserGroupSettings)
  -> DataRetentionPolicyResponseJSON
userGroupSettingsWithInheritableToJSON mInheritId settingsObj mInheritPreview
  = DataRetentionPolicyResponseJSON {
    resInheritedFrom = mInheritId
  , resDataRetentionPolicyObject = get ugsDataRetentionPolicy settingsObj
  , resInheritPreview = case mInheritPreview of
      Nothing -> Nothing
      Just (ugid, settings) -> Just (ugid, get ugsDataRetentionPolicy settings)
  }

updateUserGroupDataRetentionFromRequest
  :: DataRetentionPolicy
  -> Value
  -> Maybe DataRetentionPolicy
updateUserGroupDataRetentionFromRequest ugSett settingsChanges =
  let ugSettReq = dataRetentionPolicyToRequest ugSett
  in case update ugSettReq unjsonDataRetentionPolicyRequestJSON settingsChanges of
    (Result ugSettUpdated []) -> Just $ reqDataRetentionPolicyObject ugSettUpdated
    (Result _ _) -> Nothing

dataRetentionPolicyToRequest :: DataRetentionPolicy -> DataRetentionPolicyRequestJSON
dataRetentionPolicyToRequest dataRetention = DataRetentionPolicyRequestJSON {
    reqDataRetentionPolicyObject = dataRetention
  }

unjsonDataRetentionPolicyResponseJSON :: UnjsonDef DataRetentionPolicyResponseJSON
unjsonDataRetentionPolicyResponseJSON = objectOf $ pure constructor
  <*> fieldOpt "inherited_from" resInheritedFrom
    "User Group Settings Response Inherited From ID"
  <*> fieldBy "data_retention_policy" resDataRetentionPolicyObject
    "User Group Settings Data Retention Policy" unjsonDataRetentionPolicy
      where
        constructor inheritedFrom drp = DataRetentionPolicyResponseJSON {
            resInheritedFrom = inheritedFrom
          , resDataRetentionPolicyObject = drp
          , resInheritPreview = Nothing
          }

unjsonDataRetentionPolicyWithInheritableResponseJSON
  :: UnjsonDef DataRetentionPolicyResponseJSON
unjsonDataRetentionPolicyWithInheritableResponseJSON
  = objectOf $ pure DataRetentionPolicyResponseJSON
    <*> fieldOpt "inherited_from" resInheritedFrom
        "User Group Settings Response Inherited From ID"
    <*> fieldBy "data_retention_policy" resDataRetentionPolicyObject
        "User Group Settings Data Retention Policy" unjsonDataRetentionPolicy
    <*> fieldOptBy "inheritable_preview" resInheritPreview
        "User Group Settings Response Inherit Preview"
        unjsonDataRetentionPolicyInheritPreviewJSON

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
    resInheritedFrom             :: Maybe UserGroupID
  , resDataRetentionPolicyObject :: DataRetentionPolicy
  , resInheritPreview            :: Maybe (UserGroupID, DataRetentionPolicy)
  }
