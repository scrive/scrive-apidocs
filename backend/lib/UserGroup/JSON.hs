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
import qualified UserGroup.Internal as I

encodeUserGroup :: Bool -> UserGroupWithParents -> [UserGroup] -> Encoding
encodeUserGroup inheritable ugwp children =
  pairs
    $  "id"
    .= (ug ^. #id)
    <> "parent_id"
    .= (ug ^. #parentGroupID)
    <> "name"
    .= (ug ^. #name)
    <> pair "children"        childrenEncoding
    <> pair "contact_details" (encodeUserGroupContactDetails inheritable ugwp)
    <> pair "settings" (encodeUserGroupSettings inheritable ugwp)
  where
    ug = ugwpUG ugwp
    childrenEncoding =
      flip list children $ \child -> pairs $ "id" .= (child ^. #id) <> "name" .= (child ^. #name)

updateUserGroupFromRequest :: UserGroup -> Value -> Maybe UserGroup
updateUserGroupFromRequest ug ugChanges = do
  let ugReq =
        UserGroupRequestJSON { reqParentID = ug ^. #parentGroupID, reqName = ug ^. #name }
  case update ugReq unjsonUserGroupRequestJSON ugChanges of
    (Result ugUpdated []) -> Just $ ug
      & (#parentGroupID .~ reqParentID ugUpdated)
      & (#name          .~ reqName ugUpdated)
    (Result _ _) -> Nothing

unjsonUserGroupRequestJSON :: UnjsonDef UserGroupRequestJSON
unjsonUserGroupRequestJSON =
  objectOf
    $   pure UserGroupRequestJSON
    <*> fieldOpt "parent_id" reqParentID "User Group ID"
    <*> field "name" reqName "User Group Name"

data UserGroupRequestJSON = UserGroupRequestJSON {
    reqParentID    :: Maybe UserGroupID
  , reqName        :: Text
  }

newtype UGAddrJSON = UGAddrJSON UserGroupAddress

instance ToJSON UGAddrJSON where
  toJSON _ = Null -- Redundant - Only needed to avoid `deriving Generic`
  toEncoding (UGAddrJSON addr) =
    pairs
      $  "company_number"
      .= (addr ^. #ugaCompanyNumber)
      <> "entity_name"
      .= (addr ^. #ugaEntityName)
      <> "address"
      .= (addr ^. #ugaAddress)
      <> "zip"
      .= (addr ^. #ugaZip)
      <> "city"
      .= (addr ^. #ugaCity)
      <> "country"
      .= (addr ^. #ugaCountry)

encodeUserGroupContactDetails :: Bool -> UserGroupWithParents -> Encoding
encodeUserGroupContactDetails inheritable ugwp =
  pairs $ makeAddressJson inheritedFrom address <> inheritPreview
  where
    makeAddressJson mugid addr =
      "inherited_from" .= mugid <> "address" .= fmap UGAddrJSON addr
    mugAddr                  = ugwpUG ugwp ^. #address
    minherited               = ugwpAddressWithID <$> ugwpOnlyParents ugwp
    (inheritedFrom, address) = if isJust mugAddr
      then (Nothing, mugAddr) -- UG has own Address
      else L.unzip minherited -- UG has inherited Address
    inheritPreview = if inheritable
      then pair "inheritable_preview" $ case minherited of
        Nothing           -> null_ -- UG is root
        Just (ugid, addr) -> pairs $ makeAddressJson (Just ugid) (Just addr)
      else mempty

updateUserGroupContactDetailsFromRequest
  :: UserGroupAddress -> Value -> Maybe UserGroupAddress
updateUserGroupContactDetailsFromRequest ugAddr contactDetailsChanges =
  case contactDetailsChanges of
    Object obj -> do
      address <- HM.lookup "address" obj
      case update ugAddr unjsonUserGroupAddress address of
        (Result addressUpdated []) -> Just addressUpdated
        (Result _              _ ) -> Nothing
    _ -> Nothing

-- You must also update ToJSON UGAddrJSON above
unjsonUserGroupAddress :: UnjsonDef UserGroupAddress
unjsonUserGroupAddress =
  objectOf
    $   pure I.UserGroupAddress
    <*> fieldBy "company_number"
                (^. #ugaCompanyNumber)
                "User Group Address Company Number"
                (unjsonWithValidationOrEmptyText asValidCompanyNumber)
    <*> fieldBy "entity_name"
                (^. #ugaEntityName)
                "User Group Address Entity Name"
                (unjsonWithValidationOrEmptyText asValidCompanyName)
    <*> fieldBy "address"
                (^. #ugaAddress)
                "User Group Address Address"
                (unjsonWithValidationOrEmptyText asValidAddress)
    <*> fieldBy "zip"
                (^. #ugaZip)
                "User Group Address Zip Code"
                (unjsonWithValidationOrEmptyText asValidZip)
    <*> fieldBy "city"
                (^. #ugaCity)
                "User Group Address City"
                (unjsonWithValidationOrEmptyText asValidCity)
    <*> fieldBy "country"
                (^. #ugaCountry)
                "User Group Address Country"
                (unjsonWithValidationOrEmptyText asValidCountry)

newtype UGDRPJSON = UGDRPJSON DataRetentionPolicy

instance ToJSON UGDRPJSON where
  toJSON _ = Null -- Redundant - Only needed to avoid `deriving Generic`
  toEncoding (UGDRPJSON drp) =
    pairs
      $  "idle_doc_timeout_preparation"
      .= drpIdleDocTimeoutPreparation drp
      <> "idle_doc_timeout_closed"
      .= drpIdleDocTimeoutClosed drp
      <> "idle_doc_timeout_canceled"
      .= drpIdleDocTimeoutCanceled drp
      <> "idle_doc_timeout_timedout"
      .= drpIdleDocTimeoutTimedout drp
      <> "idle_doc_timeout_rejected"
      .= drpIdleDocTimeoutRejected drp
      <> "idle_doc_timeout_error"
      .= drpIdleDocTimeoutError drp
      <> "immediate_trash"
      .= drpImmediateTrash drp

encodeUserGroupSettings :: Bool -> UserGroupWithParents -> Encoding
encodeUserGroupSettings inheritable ugwp =
  pairs $ makeDRPJson inheritedFrom msettings <> inheritPreview
  where
    makeDRPJson mugid msett =
      let drp = UGDRPJSON . view #ugsDataRetentionPolicy <$> msett
      in  "inherited_from" .= mugid <> "data_retention_policy" .= drp
    mugSettings                = ugwpUG ugwp ^. #settings
    minherited                 = ugwpSettingsWithID <$> ugwpOnlyParents ugwp
    (inheritedFrom, msettings) = if isJust mugSettings
      then (Nothing, mugSettings) -- UG has own Settings
      else L.unzip minherited     -- UG has inherited Settings
    inheritPreview = if inheritable
      then pair "inheritable_preview" $ case minherited of
        Nothing           -> null_ -- UG is root
        Just (ugid, sett) -> pairs $ makeDRPJson (Just ugid) (Just sett)
      else mempty

updateUserGroupDataRetentionFromRequest
  :: DataRetentionPolicy -> Value -> Maybe DataRetentionPolicy
updateUserGroupDataRetentionFromRequest ugSett settingsChanges = case settingsChanges of
  Object obj -> do
    dataRetention <- HM.lookup "data_retention_policy" obj
    case update ugSett unjsonDataRetentionPolicy dataRetention of
      (Result dataRetentionUpdated []) -> Just dataRetentionUpdated
      (Result _                    _ ) -> Nothing
  _ -> Nothing
