module HubSpot.Conf ( HubSpotConf(..), unjsonHubSpotConf ) where

import Data.Unjson
import qualified Data.Map as Map
import qualified Text.JSON.Gen as J

-- | A datatype for HubSpot configuration data.
data HubSpotConf = HubSpotConf
  { hubspotHubId :: String
  , hubspotFormIds  :: Map.Map String String
  } deriving (Show, Eq, Ord)

unjsonHubSpotConf :: UnjsonDef HubSpotConf
unjsonHubSpotConf =
  objectOf
    $   pure HubSpotConf
    <*> fieldDef "hub_id" ""        hubspotHubId   "HubSpot Hub Id"
    <*> fieldDef "forms"  Map.empty hubspotFormIds "HubSpot form ID:s by name"

instance Unjson HubSpotConf where
  unjsonDef = unjsonHubSpotConf

instance J.ToJSValue HubSpotConf where
  toJSValue HubSpotConf {..} = J.runJSONGen $ do
    J.value "hub_id" $ hubspotHubId
    J.value "forms" $ J.toJSValue hubspotFormIds
