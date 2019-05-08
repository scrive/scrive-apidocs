module UserGroup.JSON.ContactDetails (
    userGroupContactDetailsToResponse
  , userGroupContactDetailsWithInheritableToResponse
  , userGroupContactDetailsToJSON
  , userGroupContactDetailsWithInheritableToJSON
  , updateUserGroupContactDetailsFromRequest
  , UserGroupContactDetailsResponseJSON
  , UserGroupContactDetailsRequestJSON
  , unjsonUserGroupContactDetailsResponseJSON
  , unjsonUserGroupContactDetailsResponseWithInheritableJSON
) where

import Data.Aeson
import Data.Text (Text)
import Data.Unjson

import UserGroup.Types

userGroupContactDetailsToResponse
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> (UnjsonDef UserGroupContactDetailsResponseJSON, UserGroupContactDetailsResponseJSON)
userGroupContactDetailsToResponse mInheritId contactDetailsObj
  = ( unjsonUserGroupContactDetailsResponseJSON
    , userGroupContactDetailsToJSON mInheritId contactDetailsObj
    )

userGroupContactDetailsWithInheritableToResponse
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> Maybe (UserGroupID, UserGroupAddress)
  -> (UnjsonDef UserGroupContactDetailsResponseJSON, UserGroupContactDetailsResponseJSON)
userGroupContactDetailsWithInheritableToResponse mInheritId contactDetailsObj mInheritPreview
  = ( unjsonUserGroupContactDetailsResponseWithInheritableJSON
    , userGroupContactDetailsWithInheritableToJSON mInheritId contactDetailsObj mInheritPreview
    )

userGroupContactDetailsToJSON
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> UserGroupContactDetailsResponseJSON
userGroupContactDetailsToJSON mInheritId contactDetailsObj
  = UserGroupContactDetailsResponseJSON {
    resInheritedFrom = mInheritId
  , resContactDetailsObject = userGroupContactDetailsToJSON' contactDetailsObj
  , resInheritPreview = Nothing
  }

userGroupContactDetailsWithInheritableToJSON
  :: Maybe UserGroupID
  -> UserGroupAddress
  -> Maybe (UserGroupID, UserGroupAddress)
  -> UserGroupContactDetailsResponseJSON
userGroupContactDetailsWithInheritableToJSON mInheritId contactDetailsObj mInheritPreview
  = UserGroupContactDetailsResponseJSON {
    resInheritedFrom = mInheritId
  , resContactDetailsObject = userGroupContactDetailsToJSON' contactDetailsObj
  , resInheritPreview = case mInheritPreview of
      Nothing -> Nothing
      Just (ugid, contactDetails) -> Just (ugid, userGroupContactDetailsToJSON' contactDetails)
  }

updateUserGroupContactDetailsFromRequest :: UserGroupAddress -> Value -> Maybe UserGroupAddress
updateUserGroupContactDetailsFromRequest ugAddr contactDetailsChanges =
  let ugAddrReq = userGroupContactDetailsToRequest ugAddr
  in case update ugAddrReq unjsonUserGroupContactDetailsRequestJSON contactDetailsChanges of
    (Result ugAddrUpdated []) -> Just $ userGroupContactDetailsFromRequest ugAddrUpdated
    (Result _ _) -> Nothing

userGroupContactDetailsToRequest :: UserGroupAddress -> UserGroupContactDetailsRequestJSON
userGroupContactDetailsToRequest contactDetails = UserGroupContactDetailsRequestJSON {
    reqContactDetailsObject = userGroupContactDetailsToJSON' contactDetails
  }

userGroupContactDetailsFromRequest :: UserGroupContactDetailsRequestJSON -> UserGroupAddress
userGroupContactDetailsFromRequest UserGroupContactDetailsRequestJSON{reqContactDetailsObject = contactDetailsObj} =
  UserGroupAddress {
    _ugaCompanyNumber = companyNumber contactDetailsObj
  , _ugaAddress = address contactDetailsObj
  , _ugaZip = zipCode contactDetailsObj
  , _ugaCity = city contactDetailsObj
  , _ugaCountry = country contactDetailsObj
  }

userGroupContactDetailsToJSON' :: UserGroupAddress -> UserGroupContactDetailsJSON
userGroupContactDetailsToJSON' contactDetailsObj = UserGroupContactDetailsJSON {
    companyNumber = _ugaCompanyNumber contactDetailsObj
  , address = _ugaAddress contactDetailsObj
  , zipCode = _ugaZip contactDetailsObj
  , city = _ugaCity contactDetailsObj
  , country = _ugaCountry contactDetailsObj
  }


unjsonUserGroupContactDetailsResponseJSON :: UnjsonDef UserGroupContactDetailsResponseJSON
unjsonUserGroupContactDetailsResponseJSON = objectOf $ pure constructor
  <*> fieldOpt "inherited_from" resInheritedFrom
    "User Group ContactDetails Response Inherited From ID"
  <*> fieldBy "address" resContactDetailsObject
    "User Group address Response ContactDetails"
    unjsonUserGroupContactDetailsJSON
      where
        constructor inheritedFrom cdo = UserGroupContactDetailsResponseJSON {
            resInheritedFrom = inheritedFrom
          , resContactDetailsObject = cdo
          , resInheritPreview = Nothing
          }

unjsonUserGroupContactDetailsResponseWithInheritableJSON
  :: UnjsonDef UserGroupContactDetailsResponseJSON
unjsonUserGroupContactDetailsResponseWithInheritableJSON
  = objectOf $ pure UserGroupContactDetailsResponseJSON
    <*> fieldOpt "inherited_from" resInheritedFrom
      "User Group ContactDetails Response Inherited From ID"
    <*> fieldBy "address" resContactDetailsObject
      "User Group address Response ContactDetails"
      unjsonUserGroupContactDetailsJSON
    <*> fieldOptBy "inheritable_preview" resInheritPreview
      "User Group ContactDetails Response Inherited Preview"
      unjsonUserGroupContactDetailsInheritPreviewJSON

unjsonUserGroupContactDetailsRequestJSON :: UnjsonDef UserGroupContactDetailsRequestJSON
unjsonUserGroupContactDetailsRequestJSON = objectOf $ pure UserGroupContactDetailsRequestJSON
  <*> fieldBy "address" reqContactDetailsObject
    "User Group ContactDetails JSON ContactDetails"
    unjsonUserGroupContactDetailsJSON

unjsonUserGroupContactDetailsJSON :: UnjsonDef UserGroupContactDetailsJSON
unjsonUserGroupContactDetailsJSON = objectOf $ pure UserGroupContactDetailsJSON
  <*> field "company_number" companyNumber "User Group ContactDetails JSON Company Number"
  <*> field "address" address "User Group ContactDetails JSON ContactDetails"
  <*> field "zip" zipCode "User Group ContactDetails JSON Zip Code"
  <*> field "city" city "User Group ContactDetails JSON City"
  <*> field "country" country "User Group ContactDetails JSON Country"

unjsonUserGroupContactDetailsInheritPreviewJSON :: UnjsonDef (UserGroupID, UserGroupContactDetailsJSON)
unjsonUserGroupContactDetailsInheritPreviewJSON = objectOf $ pure (,)
  <*> field "inherited_from" fst ""
  <*> fieldBy "address" snd "" unjsonUserGroupContactDetailsJSON

data UserGroupContactDetailsRequestJSON = UserGroupContactDetailsRequestJSON {
    reqContactDetailsObject :: UserGroupContactDetailsJSON
  }

data UserGroupContactDetailsResponseJSON = UserGroupContactDetailsResponseJSON {
    resInheritedFrom        :: Maybe UserGroupID
  , resContactDetailsObject :: UserGroupContactDetailsJSON
  , resInheritPreview       :: Maybe (UserGroupID, UserGroupContactDetailsJSON)
  }

data UserGroupContactDetailsJSON = UserGroupContactDetailsJSON {
    companyNumber :: Text
  , address       :: Text
  , zipCode       :: Text
  , city          :: Text
  , country       :: Text
  }
