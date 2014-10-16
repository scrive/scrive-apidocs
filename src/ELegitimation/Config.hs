module ELegitimation.Config(LogicaConfig(..)) where

import Control.Applicative
import Data.Unjson

data LogicaConfig = LogicaConfig { logicaEndpoint      :: String,  -- ^ URL to Logica
                                   logicaServiceID     :: String,  -- ^ ServiceID from Logica
                                   logicaCertFile      :: String,  -- ^ Path to certificate file
                                   logicaMBIDisplayName:: String,  -- ^ Display Name for Mobile Bank ID (must match display name registered with Logica)
                                   logicaMBIEndpoint   :: String   -- ^ URL for MobileBankID at Logica
                                 }
                  deriving (Show, Read, Ord, Eq)

unjsonLogicaConfig :: UnjsonDef LogicaConfig
unjsonLogicaConfig = objectOf $ pure LogicaConfig
  <*> field "endpoint"
      logicaEndpoint
      "URL"
  <*> field "service_id"
      logicaServiceID
      "Service ID"
  <*> field "cert_file"
      logicaCertFile
      "Path to certification file"
  <*> field "mbi_display_name"
      logicaMBIDisplayName
      "Mobile BankiD display name"
  <*> field "mbi_endpoint"
      logicaMBIEndpoint
      "Mobile BankiD endpoint"

instance Unjson LogicaConfig where
  unjsonDef = unjsonLogicaConfig
