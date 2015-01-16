module HubSpot.Conf ( HubSpotConf(..) ) where

import Control.Applicative
import Data.Unjson
import qualified Data.Map as Map

--
-- @tmpnote(fredrik): CHECK THIS!
--
--   * should **only** `Data.Map.Strict.empty` and the data type be imported?
--
--   * since the map is small relatively speaking, I wanted to use
--   `Data.Map.Strict`. However, the Unjson instance is for the lazy
--   counterpart, so this is why the lazy map is used.
--
--   * I used all `String`:s below since that is used in e.g. SalesforceConf.
--

data HubSpotConf = HubSpotConf {
      hubspotHubId :: String
    , hubspotFormIds  :: Map.Map String String
} deriving (Show, Read, Eq, Ord)

unjsonHubSpotConf :: UnjsonDef HubSpotConf
unjsonHubSpotConf = objectOf $ pure HubSpotConf
  <*> fieldDef "hubId"
      ""
      hubspotHubId
      "HubSpot Hub Id"
  <*> fieldDef "formIds"
      Map.empty
      hubspotFormIds
      "HubSpot form ID:s by name"

instance Unjson HubSpotConf where
  unjsonDef = unjsonHubSpotConf

