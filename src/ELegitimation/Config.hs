module ELegitimation.Config(
          certfile       
        , serviceid
        , endpoint) where

-- JSON - just enough to get things working

endpoint :: String
endpoint = "https://eid.funktionstjanster.se:8890/osif" -- production
--endpoint = "https://eidt.funktionstjanster.se:18898/osif" -- test

serviceid :: String
serviceid = "skrivapa9421" -- production
--serviceid = "logtest004" -- test

certfile :: String
certfile = "certs/steria3.pem"

