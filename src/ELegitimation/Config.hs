module ELegitimation.Config(LogicaConfig(..)) where

data LogicaConfig = LogicaConfig { logicaEndpoint      :: String,  -- ^ URL to Logica
                                   logicaServiceID     :: String,  -- ^ ServiceID from Logica
                                   logicaCertFile      :: String,  -- ^ Path to certificate file
                                   logicaMBIDisplayName:: String,  -- ^ Display Name for Mobile Bank ID (must match display name registered with Logica)
                                   logicaMBIEndpoint   :: String   -- ^ URL for MobileBankID at Logica
                                 }
                  deriving (Show, Read, Ord, Eq)
