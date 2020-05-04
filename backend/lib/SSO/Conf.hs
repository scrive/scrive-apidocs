module SSO.Conf
    ( SSOConf(..)
    ) where

import Data.Text as T
import Data.Unjson
import Network.URI
import qualified Data.Aeson as Aeson

import UserGroup.Internal (UserGroupSSOConfiguration(..))

data SSOConf = SSOConf {
   scSamlEntityBaseURI :: !URI,
   scSamlIdps :: [UserGroupSSOConfiguration]
} deriving (Show, Eq)

unjsonSSOConf :: UnjsonDef SSOConf
unjsonSSOConf = objectOf
  (   SSOConf
  <$> fieldBy
        "saml_entity_base_uri"
        scSamlEntityBaseURI
        "base URI of Scrive SAML entity (most of the time base uri of server)  - eg. \"https://features-testbed.scrive.com/\""
        unjsonURI
  <*> fieldDefBy "idps"
                 []
                 scSamlIdps
                 " a list of idps"
                 unjsonUserGroupSSOConfigurationsFromConfig
  )

unjsonUserGroupSSOConfigurationsFromConfig :: UnjsonDef [UserGroupSSOConfiguration]
unjsonUserGroupSSOConfigurationsFromConfig = arrayOf $ objectOf
  (   UserGroupSSOConfiguration
  <$> field "idp_id"         idpID              "ID of idp"
  <*> field "idp_public_key" publicKey          "public key of idp"
  <*> field "idp_init_ug"    userInitialGroupID "initial user group id"
  <*> fieldDef "idp_put_name_id_in_company_position"
               False
               putNameIDInCompanyPosition
               "Put name id into company position field"
  )

instance Unjson SSOConf where
  unjsonDef = unjsonSSOConf

unjsonURI :: UnjsonDef URI
unjsonURI = SimpleUnjsonDef "Network.URI" jsonToUri uriToJson
  where
    jsonToUri :: Aeson.Value -> Result URI
    jsonToUri (Aeson.String uri) = case parseURI $ T.unpack uri of
      Nothing -> Result undefined [Anchored (Path [PathElemKey "."]) "Non parsable URI"]
      Just parsedUri -> Result parsedUri []
    jsonToUri _ = Result
      undefined
      [Anchored (Path [PathElemKey "."]) "URI's can only be represented as String"]
    uriToJson :: URI -> Aeson.Value
    uriToJson = Aeson.String . T.pack . show
