module ELegitimation.Config(LogicaConfig(..)) where

data LogicaConfig = LogicaConfig { logicaEndpoint      :: String,  -- ^ URL to Logica
                                   logicaServiceID     :: String,  -- ^ ServiceID from Logica
                                   logicaCertFile      :: String,  -- ^ Path to certificate file
                                   logicaMBIDisplayName:: String,  -- ^ Display Name for Mobile Bank ID (must match display name registered with Logica)
                                   logicaMBIEndpoint   :: String   -- ^ URL for MobileBankID at Logica
                                 }
                  deriving (Show, Read, Ord, Eq)

{- Please delete this on August 1, 2012
-- JSON - just enough to get things working

endpoint :: String
endpoint = "https://eid.funktionstjanster.se:8890/osif" -- production
--endpoint = "https://eidt.funktionstjanster.se:18898/osif" -- test

serviceid :: String
serviceid = "skrivapa9421" -- production
--serviceid = "logtest004" -- test

certfile :: String
certfile = "certs/steria3.pem"

-}