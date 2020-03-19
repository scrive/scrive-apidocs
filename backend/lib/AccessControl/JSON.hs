module AccessControl.JSON (
  getApiRoleParameter
, encodeAccessRole
, encodeAccessRoles
) where

import Data.Aeson
import Data.Bifunctor
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import API.V2
import API.V2.Parameters
import Folder.Model
import Kontra
import User.UserID
import UserGroup.Types

data AccessRoleJSON = AccessRoleJSON {
    roleID         :: Maybe AccessRoleID
  , isGenerated    :: Maybe Bool
  , roleType       :: AccessRoleType
  , source         :: AccessRoleSourceJSON
  , target         :: AccessRoleTargetJSON
  , allowedActions :: AccessRoleAllowedActions
  }
  deriving Generic

instance ToJSON AccessRoleJSON where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = nameMap }
    where
      nameMap :: String -> String
      nameMap "roleID" = "id"
      nameMap s        = camelTo2 '_' s

instance FromJSON AccessRoleJSON where
  parseJSON = withObject "role" $ \o -> do
    roleType <- o .: "role_type"
    source   <- o .: "source"
    target   <- o .: "target"
    forbidField o "id"
    forbidField o "is_generated"
    forbidField o "allowed_actions"
    let roleID         = Nothing
    let isGenerated    = Nothing
    let allowedActions = AccessRoleAllowedActions []
    return AccessRoleJSON { .. }
    where
      forbidField o name = o .:? name >>= \case
        Just (_ :: Value) ->
          fail $ "Cannot parse role: " ++ T.unpack name ++ " must not be set"
        Nothing -> return ()

data AccessRoleSourceJSON
  = AccessRoleUserSourceJSON UserID
  | AccessRoleUserGroupSourceJSON UserGroupID
  deriving Generic

instance ToJSON AccessRoleSourceJSON where
  toEncoding (AccessRoleUserSourceJSON uid) =
    pairs $ "type" .= ("user" :: T.Text) <> "id" .= uid
  toEncoding (AccessRoleUserGroupSourceJSON ugid) =
    pairs $ "type" .= ("user_group" :: T.Text) <> "id" .= ugid

instance FromJSON AccessRoleSourceJSON where
  parseJSON = withObject "source" $ \o -> do
    (sourceType :: T.Text) <- o .: "type"
    case sourceType of
      "user"       -> AccessRoleUserSourceJSON <$> o .: "id"
      "user_group" -> AccessRoleUserGroupSourceJSON <$> o .: "id"
      _            -> fail "Cannot parse role: Invalid source type"

data AccessRoleTargetJSON
  = UserTargetJSON UserID
  | UserGroupTargetJSON UserGroupID
  | FolderTargetJSON FolderID
  deriving Generic

instance ToJSON AccessRoleTargetJSON where
  toEncoding (UserTargetJSON uid) = pairs $ "type" .= ("user" :: T.Text) <> "id" .= uid
  toEncoding (UserGroupTargetJSON ugid) =
    pairs $ "type" .= ("user_group" :: T.Text) <> "id" .= ugid
  toEncoding (FolderTargetJSON fid) =
    pairs $ "type" .= ("folder" :: T.Text) <> "id" .= fid

instance FromJSON AccessRoleTargetJSON where
  parseJSON = withObject "target" $ \o -> do
    (targetType :: T.Text) <- o .: "type"
    case targetType of
      "user"       -> UserTargetJSON <$> o .: "id"
      "user_group" -> UserGroupTargetJSON <$> o .: "id"
      "folder"     -> FolderTargetJSON <$> o .: "id"
      _            -> fail "Cannot parse role: Invalid target type"

accessRoleToJSON :: AccessRole -> AccessRoleJSON
accessRoleToJSON (AccessRoleUser rid uid target) = AccessRoleJSON
  { roleID         = Just rid
  , isGenerated    = Just False
  , roleType       = toAccessRoleType target
  , source         = AccessRoleUserSourceJSON uid
  , target         = accessRoleTargetToJSON target
  , allowedActions = accessRoleTargetToAllowedActions target
  }
accessRoleToJSON (AccessRoleUserGroup rid ugid target) = AccessRoleJSON
  { roleID         = Just rid
  , isGenerated    = Just False
  , roleType       = toAccessRoleType target
  , source         = AccessRoleUserGroupSourceJSON ugid
  , target         = accessRoleTargetToJSON target
  , allowedActions = accessRoleTargetToAllowedActions target
  }
accessRoleToJSON (AccessRoleImplicitUser uid target) = AccessRoleJSON
  { roleID         = Nothing
  , isGenerated    = Just True
  , roleType       = toAccessRoleType target
  , source         = AccessRoleUserSourceJSON uid
  , target         = accessRoleTargetToJSON target
  , allowedActions = accessRoleTargetToAllowedActions target
  }
accessRoleToJSON (AccessRoleImplicitUserGroup ugid target) = AccessRoleJSON
  { roleID         = Nothing
  , isGenerated    = Just True
  , roleType       = toAccessRoleType target
  , source         = AccessRoleUserGroupSourceJSON ugid
  , target         = accessRoleTargetToJSON target
  , allowedActions = accessRoleTargetToAllowedActions target
  }

newtype AccessRoleAllowedActions = AccessRoleAllowedActions [(T.Text, [T.Text])]
  deriving Generic

instance ToJSON AccessRoleAllowedActions where
  toEncoding (AccessRoleAllowedActions resources) =
    pairs . mconcat $ map (uncurry (.=)) resources

accessRoleTargetToAllowedActions :: AccessRoleTarget -> AccessRoleAllowedActions
accessRoleTargetToAllowedActions target =
  AccessRoleAllowedActions . groupActions $ hasPermissions target
  where
    mkPair (Permission _ act res) = (showt res, [showt act])
    groupActions = sortAll . HM.toList . HM.fromListWith (++) . map mkPair
    sortAll      = sort . map (second sort)

accessRoleTargetToJSON :: AccessRoleTarget -> AccessRoleTargetJSON
accessRoleTargetToJSON (UserAR               uid ) = UserTargetJSON uid
accessRoleTargetToJSON (UserGroupMemberAR    ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (UserAdminAR          ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (UserGroupAdminAR     ugid) = UserGroupTargetJSON ugid
accessRoleTargetToJSON (FolderAdminAR        fid ) = FolderTargetJSON fid
accessRoleTargetToJSON (FolderUserAR         fid ) = FolderTargetJSON fid
accessRoleTargetToJSON (SharedTemplateUserAR fid ) = FolderTargetJSON fid
accessRoleTargetToJSON (EidImpersonatorAR    ugid) = UserGroupTargetJSON ugid

jsonToAccessRole :: AccessRoleJSON -> Either String AccessRole
jsonToAccessRole roleJson = constructor =<< jsonToAccessRoleTarget roleJson
  where
    constructor target = case source roleJson of
      AccessRoleUserSourceJSON uid -> return $ AccessRoleImplicitUser uid target
      AccessRoleUserGroupSourceJSON ugid ->
        return $ AccessRoleImplicitUserGroup ugid target

jsonToAccessRoleTarget :: AccessRoleJSON -> Either String AccessRoleTarget
jsonToAccessRoleTarget roleJson = case target roleJson of
  UserTargetJSON uid -> case roleType roleJson of
    -- valid
    UserART               -> return $ UserAR uid
    -- invalid
    UserGroupMemberART    -> Left invalidTargetErr
    UserAdminART          -> Left invalidTargetErr
    UserGroupAdminART     -> Left invalidTargetErr
    FolderAdminART        -> Left invalidTargetErr
    FolderUserART         -> Left invalidTargetErr
    SharedTemplateUserART -> Left invalidTargetErr
    EidImpersonatorART    -> Left invalidTargetErr
  UserGroupTargetJSON ugid -> case roleType roleJson of
    -- valid
    UserAdminART          -> return $ UserAdminAR ugid
    UserGroupAdminART     -> return $ UserGroupAdminAR ugid
    UserGroupMemberART    -> return $ UserGroupMemberAR ugid
    EidImpersonatorART    -> return $ EidImpersonatorAR ugid
    -- invalid
    UserART               -> Left invalidTargetErr
    FolderAdminART        -> Left invalidTargetErr
    FolderUserART         -> Left invalidTargetErr
    SharedTemplateUserART -> Left invalidTargetErr
  FolderTargetJSON fid -> case roleType roleJson of
    -- valid
    FolderAdminART        -> return $ FolderAdminAR fid
    FolderUserART         -> return $ FolderUserAR fid
    -- invalid
    UserART               -> Left invalidTargetErr
    UserGroupMemberART    -> Left invalidTargetErr
    UserAdminART          -> Left invalidTargetErr
    UserGroupAdminART     -> Left invalidTargetErr
    SharedTemplateUserART -> Left invalidTargetErr
    EidImpersonatorART    -> Left invalidTargetErr
  where invalidTargetErr = "Can't parse AccessRole - Role type doesn't match target type"

encodeAccessRole :: AccessRole -> Encoding
encodeAccessRole = toEncoding . accessRoleToJSON

encodeAccessRoles :: [AccessRole] -> Encoding
encodeAccessRoles = toEncoding . map accessRoleToJSON

getApiRoleParameter :: Kontrakcja m => m AccessRole
getApiRoleParameter = do
  let paramName = "role"
  roleJson <- apiV2ParameterObligatory $ ApiV2ParameterAeson paramName
  case jsonToAccessRole roleJson of
    Left errMsg ->
      let errMsg' = T.pack $ " - " ++ errMsg
      in  apiError $ requestParameterParseError paramName errMsg'
    Right role -> return role
