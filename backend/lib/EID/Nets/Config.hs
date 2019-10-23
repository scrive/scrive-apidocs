module EID.Nets.Config where

import Data.Unjson

data NetsConfig = NetsConfig
  { netsMerchantIdentifier     :: Text
  , netsMerchantPassword       :: Text
  , netsIdentifyUrl            :: Text
  , netsAssertionUrl           :: Text
  , netsTrustedDomain          :: Text
  } deriving (Eq, Ord, Show)

instance Unjson NetsConfig where
  unjsonDef =
    objectOf
      $   NetsConfig
      <$> field "mid"          netsMerchantIdentifier "Nets merchant identifier"
      <*> field "password"     netsMerchantPassword   "Nets merchant password"
      <*> field "identifyUrl"  netsIdentifyUrl        "Nets identify url"
      <*> field "assertionUrl" netsAssertionUrl       "Nets assertion url"
      <*> field "trustedDomain"
                netsTrustedDomain
                "Trusted domain registed for this nets merchant"


data NetsSignConfig = NetsSignConfig
  { netssignCertFile    :: FilePath
  , netssignPrivKeyFile :: FilePath
  , netssignAPIUrl      :: Text
  , netssignMerchantID  :: Text
  , netssignSignURLBase :: Text
  } deriving (Eq, Ord, Show)

instance Unjson NetsSignConfig where
  unjsonDef =
    objectOf
      $   NetsSignConfig
      <$> field "user_cert_file" netssignCertFile "Path to the user certificate file"
      <*> field "user_privkey_file"
                netssignPrivKeyFile
                "Path to the user private key file"
      <*> field "sign_xml_url"  netssignAPIUrl      "Url to Nets TrustSign interface"
      <*> field "merchant_id"   netssignMerchantID  "Nets merchant ID"
      <*> field "sign_url_base" netssignSignURLBase "Nets signing URL base"
