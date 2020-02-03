{-# OPTIONS_GHC -fno-warn-orphans #-}
module SSO.Conf
    ( SSOConf(..)
    , IDPConf(..)
    ) where

import Data.Text as T
import Data.Text.Encoding as T
import Data.Unjson
import Network.URI
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Crypto.Store.X509 as X509Store
import qualified Data.Aeson as Aeson
import qualified Data.X509 as X509

import UserGroup.Types

data SSOConf = SSOConf {
    scIdpConfs :: ![IDPConf]
  , scSamlEntityBaseURI :: !URI
} deriving (Show, Eq)

unjsonSSOConf :: UnjsonDef SSOConf
unjsonSSOConf =
  objectOf
    $   pure SSOConf
    <*> fieldBy "idps" scIdpConfs "The configured IdPs" (arrayOf unjsonIDPConf)
    <*> field
          "saml_entity_base_uri"
          scSamlEntityBaseURI
          "base URI of Scrive SAML entity (most of the time base uri of server)  - eg. \"https://features-testbed.scrive.com/\""

instance Unjson SSOConf where
  unjsonDef = unjsonSSOConf

data IDPConf = IDPConf {
  icID                 :: !T.Text,
  icPublicKey          :: !RSA.PublicKey,
  icUserInitialGroupID :: !UserGroupID,
  icPutNameIDInCompanyPosition :: !Bool
} deriving (Show, Eq)

unjsonIDPConf :: UnjsonDef IDPConf
unjsonIDPConf =
  objectOf
    $   pure IDPConf
    <*> field "idp_id"         icID                 "Entity name of IdP"
    <*> field "idp_public_key" icPublicKey          "IdP public RSA key"
    <*> field "idp_init_ug"    icUserInitialGroupID "Group used for user provisioning"
    <*> fieldDef "idp_put_name_id_in_company_position"
                 False
                 icPutNameIDInCompanyPosition
                 "Put NameID in company position field of the user"

unjsonRSAPublicKey :: UnjsonDef RSA.PublicKey
unjsonRSAPublicKey = SimpleUnjsonDef "Crypto.PubKey.RSA.PublicKey"
                                     jsonToPubKey
                                     pubKeyToJson
  where
    pubKeyToJson :: RSA.PublicKey -> Aeson.Value
    pubKeyToJson publicKey =
      Aeson.String
        . T.decodeUtf8
        . X509Store.writePubKeyFileToMemory
        $ [X509.PubKeyRSA publicKey]
    jsonToPubKey :: Aeson.Value -> Result RSA.PublicKey
    jsonToPubKey (Aeson.String s) =
      case listToMaybe . X509Store.readPubKeyFileFromMemory . encodeUtf8 $ s of
        (Just (X509.PubKeyRSA pubKey)) -> Result pubKey []
        (Just _) -> Result
          undefined
          [Anchored (Path [PathElemKey "."]) "Only RSA keys are supported"]
        Nothing -> Result
          undefined
          [ Anchored
              (Path [PathElemKey "."])
              "Unable to read the public key from config - make sure it's properly formatted"
          ]
    jsonToPubKey _ = Result
      undefined
      [Anchored (Path [PathElemKey "."]) "RSA key can only be represented as String"]

instance Unjson RSA.PublicKey where
  unjsonDef = unjsonRSAPublicKey

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

instance Unjson URI where
  unjsonDef = unjsonURI
