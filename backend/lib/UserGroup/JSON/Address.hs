module UserGroup.JSON.Address (
    userGroupAddressToResponse
  , userGroupAddressToJSON
  , updateUserGroupAddressFromResponse
  , UserGroupAddressResponseJSON
  , UserGroupAddressRequestJSON
  , unjsonUserGroupAddressResponseJSON
) where

import Data.Aeson
import Data.Text (Text)
import Data.Unjson

import UserGroup.Types

userGroupAddressToResponse
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> Maybe (UserGroupID, UserGroupAddress)
  -> (UnjsonDef UserGroupAddressResponseJSON, UserGroupAddressResponseJSON)
userGroupAddressToResponse mInheritId addressObj mInheritPreview
  = ( unjsonUserGroupAddressResponseJSON
    , userGroupAddressToJSON mInheritId addressObj mInheritPreview
    )

userGroupAddressToJSON
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> Maybe (UserGroupID, UserGroupAddress)
  -> UserGroupAddressResponseJSON
userGroupAddressToJSON mInheritId addressObj mInheritPreview
  = UserGroupAddressResponseJSON {
    resInheritedFrom = mInheritId
  , resAddressObject = userGroupAddressToJSON' addressObj
  , resInheritPreview = case mInheritPreview of
      Nothing -> Nothing
      Just (ugid, address) -> Just (ugid, userGroupAddressToJSON' address)
  }

updateUserGroupAddressFromResponse :: UserGroupAddress -> Value -> Maybe UserGroupAddress
updateUserGroupAddressFromResponse ugAddr addressChanges =
  let ugAddrReq = userGroupAddressToRequest ugAddr
  in case update ugAddrReq unjsonUserGroupAddressRequestJSON addressChanges of
    (Result ugAddrUpdated []) -> Just $ userGroupAddressFromRequest ugAddrUpdated
    (Result _ _) -> Nothing

userGroupAddressToRequest :: UserGroupAddress -> UserGroupAddressRequestJSON
userGroupAddressToRequest address = UserGroupAddressRequestJSON {
    reqAddressObject = userGroupAddressToJSON' address
  }

userGroupAddressFromRequest :: UserGroupAddressRequestJSON -> UserGroupAddress
userGroupAddressFromRequest UserGroupAddressRequestJSON{reqAddressObject = addressObj} =
  UserGroupAddress {
    _ugaCompanyNumber = companyNumber addressObj
  , _ugaAddress = address addressObj
  , _ugaZip = zipCode addressObj
  , _ugaCity = city addressObj
  , _ugaCountry = country addressObj
  }

userGroupAddressToJSON' :: UserGroupAddress -> UserGroupAddressJSON
userGroupAddressToJSON' addressObj = UserGroupAddressJSON {
    companyNumber = _ugaCompanyNumber addressObj
  , address = _ugaAddress addressObj
  , zipCode = _ugaZip addressObj
  , city = _ugaCity addressObj
  , country = _ugaCountry addressObj
  }

unjsonUserGroupAddressResponseJSON :: UnjsonDef UserGroupAddressResponseJSON
unjsonUserGroupAddressResponseJSON = objectOf $ pure UserGroupAddressResponseJSON
  <*> fieldOpt "inherited_from" resInheritedFrom
    "User Group Address Response Inherited From ID"
  <*  fieldReadonly "can_inherit" canInherit
    "User Group Address Response Can Inherit"
  <*> fieldBy "address" resAddressObject
    "User Group Address Response Address"
    unjsonUserGroupAddressJSON
  <*> fieldOptBy "inherit_preview" resInheritPreview
    "User Group Address Response Inherited Preview"
    unjsonUserGroupAddressInheritPreviewJSON
      where
        canInherit = isJust . resInheritedFrom || isJust . resInheritPreview

unjsonUserGroupAddressRequestJSON :: UnjsonDef UserGroupAddressRequestJSON
unjsonUserGroupAddressRequestJSON = objectOf $ pure UserGroupAddressRequestJSON
  <*> fieldBy "address" reqAddressObject
    "User Group address JSON Address"
    unjsonUserGroupAddressJSON

unjsonUserGroupAddressJSON :: UnjsonDef UserGroupAddressJSON
unjsonUserGroupAddressJSON = objectOf $ pure UserGroupAddressJSON
  <*> field "company_number" companyNumber "User Group Address JSON Company Number"
  <*> field "address" address "User Group Address JSON Address"
  <*> field "zip" zipCode "User Group Address JSON Zip Code"
  <*> field "city" city "User Group Address JSON City"
  <*> field "country" country "User Group Address JSON Country"

unjsonUserGroupAddressInheritPreviewJSON :: UnjsonDef (UserGroupID, UserGroupAddressJSON)
unjsonUserGroupAddressInheritPreviewJSON = objectOf $ pure (,)
  <*> field "inherited_from" fst ""
  <*> fieldBy "address" snd "" unjsonUserGroupAddressJSON

data UserGroupAddressRequestJSON = UserGroupAddressRequestJSON {
    reqAddressObject :: UserGroupAddressJSON
  }

data UserGroupAddressResponseJSON = UserGroupAddressResponseJSON {
    resInheritedFrom  :: Maybe UserGroupID
  , resAddressObject      :: UserGroupAddressJSON
  , resInheritPreview :: Maybe (UserGroupID, UserGroupAddressJSON)
  }

data UserGroupAddressJSON = UserGroupAddressJSON {
    companyNumber :: Text
  , address       :: Text
  , zipCode       :: Text
  , city          :: Text
  , country       :: Text
  }
