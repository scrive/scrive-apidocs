module UserGroup.JSON (
    encodeUserGroup
  , updateUserGroupFromRequest
  , encodeUserGroupContactDetails
  , updateUserGroupContactDetailsFromRequest
  , encodeUserGroupSettings
  , updateUserGroupDataRetentionFromRequest
) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Unjson
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as L

import DataRetentionPolicy
import InputValidation
import UserGroup.Types

encodeUserGroup :: Bool -> UserGroupWithParents -> [UserGroup] -> Encoding
encodeUserGroup inheritable ugwp children = pairs $
     "id"        .= get ugID ug
  <> "parent_id" .= get ugParentGroupID ug
  <> "name"      .= get ugName ug
  <> pair "children"        childrenEncoding
  <> pair "contact_details" (encodeUserGroupContactDetails inheritable ugwp)
  <> pair "settings"        (encodeUserGroupSettings       inheritable ugwp)
    where
      ug = ugwpUG ugwp
      childrenEncoding = flip list children $ \child ->
        pairs $ "id"   .= get ugID   child
             <> "name" .= get ugName child

updateUserGroupFromRequest :: UserGroup -> Value -> Maybe UserGroup
updateUserGroupFromRequest ug ugChanges = do
  let
    ugReq = UserGroupRequestJSON
      { reqParentID = get ugParentGroupID ug
      , reqName     = get ugName ug
      }
  case update ugReq unjsonUserGroupRequestJSON ugChanges of
    (Result ugUpdated []) -> Just $ ug
      { _ugParentGroupID = reqParentID ugUpdated
      , _ugName          = reqName ugUpdated
      }
    (Result _ _) -> Nothing

unjsonUserGroupRequestJSON :: UnjsonDef UserGroupRequestJSON
unjsonUserGroupRequestJSON = objectOf $ pure UserGroupRequestJSON
  <*> fieldOpt "parent_id" reqParentID "User Group ID"
  <*> field "name" reqName "User Group Name"

data UserGroupRequestJSON = UserGroupRequestJSON {
    reqParentID    :: Maybe UserGroupID
  , reqName        :: Text
  }

newtype UGAddrJSON = UGAddrJSON UserGroupAddress

instance ToJSON UGAddrJSON where
  toJSON _ = Null -- Redundant - Only needed to avoid `deriving Generic`
  toEncoding (UGAddrJSON addr) = pairs $
       "company_number" .= get ugaCompanyNumber addr
    <> "address"        .= get ugaAddress       addr
    <> "zip"            .= get ugaZip           addr
    <> "city"           .= get ugaCity          addr
    <> "country"        .= get ugaCountry       addr

encodeUserGroupContactDetails :: Bool -> UserGroupWithParents -> Encoding
encodeUserGroupContactDetails inheritable ugwp =
  pairs $ makeAddressJson inheritedFrom address <> inheritPreview
    where
      makeAddressJson mugid addr =
           "inherited_from" .= mugid
        <> "address"        .= fmap UGAddrJSON addr
      mugAddr = get ugAddress $ ugwpUG ugwp
      minherited = ugwpAddressWithID <$> ugwpOnlyParents ugwp
      (inheritedFrom, address) = if isJust mugAddr
        then (Nothing, mugAddr) -- UG has own Address
        else L.unzip minherited -- UG has inherited Address
      inheritPreview = if inheritable
        then pair "inheritable_preview" $ case minherited of
          Nothing           -> null_ -- UG is root
          Just (ugid, addr) -> pairs $ makeAddressJson (Just ugid) (Just addr)
        else mempty

updateUserGroupContactDetailsFromRequest
  :: UserGroupAddress
  -> Value
  -> Maybe UserGroupAddress
updateUserGroupContactDetailsFromRequest ugAddr contactDetailsChanges =
  case contactDetailsChanges of
    Object obj -> do
      address <- HM.lookup "address" obj
      case update ugAddr unjsonUserGroupAddress address of
        (Result addressUpdated []) -> Just addressUpdated
        (Result _ _) -> Nothing
    _ -> Nothing

-- You must also update ToJSON UGAddrJSON above
unjsonUserGroupAddress :: UnjsonDef UserGroupAddress
unjsonUserGroupAddress = objectOf $ pure UserGroupAddress
  <*> fieldBy "company_number" _ugaCompanyNumber
      "User Group Address Company Number"
      (unjsonWithValidationOrEmptyText asValidCompanyNumber)
  <*> fieldBy "address" _ugaAddress
      "User Group Address Address"
      (unjsonWithValidationOrEmptyText asValidAddress)
  <*> fieldBy "zip" _ugaZip
      "User Group Address Zip Code"
      (unjsonWithValidationOrEmptyText asValidZip)
  <*> fieldBy "city" _ugaCity
      "User Group Address City"
      (unjsonWithValidationOrEmptyText asValidCity)
  <*> fieldBy "country" _ugaCountry
      "User Group Address Country"
      (unjsonWithValidationOrEmptyText asValidCountry)

newtype UGDRPJSON = UGDRPJSON DataRetentionPolicy

instance ToJSON UGDRPJSON where
  toJSON _ = Null -- Redundant - Only needed to avoid `deriving Generic`
  toEncoding (UGDRPJSON drp) = pairs $
       "idle_doc_timeout_preparation" .= get drpIdleDocTimeoutPreparation drp
    <> "idle_doc_timeout_closed"      .= get drpIdleDocTimeoutClosed      drp
    <> "idle_doc_timeout_canceled"    .= get drpIdleDocTimeoutCanceled    drp
    <> "idle_doc_timeout_timedout"    .= get drpIdleDocTimeoutTimedout    drp
    <> "idle_doc_timeout_rejected"    .= get drpIdleDocTimeoutRejected    drp
    <> "idle_doc_timeout_error"       .= get drpIdleDocTimeoutError       drp
    <> "immediate_trash"              .= get drpImmediateTrash            drp

encodeUserGroupSettings :: Bool -> UserGroupWithParents -> Encoding
encodeUserGroupSettings inheritable ugwp =
  pairs $ makeDRPJson inheritedFrom msettings <> inheritPreview
    where
      makeDRPJson mugid msett =
        let drp = UGDRPJSON . get ugsDataRetentionPolicy <$> msett
        in "inherited_from" .= mugid <> "data_retention_policy" .= drp
      mugSettings = get ugSettings $ ugwpUG ugwp
      minherited = ugwpSettingsWithID <$> ugwpOnlyParents ugwp
      (inheritedFrom, msettings) =
        if isJust mugSettings
          then (Nothing, mugSettings) -- UG has own Settings
          else L.unzip minherited     -- UG has inherited Settings
      inheritPreview = if inheritable
        then pair "inheritable_preview" $ case minherited of
          Nothing           -> null_ -- UG is root
          Just (ugid, sett) -> pairs $ makeDRPJson (Just ugid) (Just sett)
        else mempty

updateUserGroupDataRetentionFromRequest
  :: DataRetentionPolicy
  -> Value
  -> Maybe DataRetentionPolicy
updateUserGroupDataRetentionFromRequest ugSett settingsChanges =
  case settingsChanges of
    Object obj -> do
      dataRetention <- HM.lookup "data_retention_policy" obj
      case update ugSett unjsonDataRetentionPolicy dataRetention of
        (Result dataRetentionUpdated []) -> Just dataRetentionUpdated
        (Result _ _) -> Nothing
    _ -> Nothing
