module Monitor.API (
    monitorAPI
  , apiCallMonitorStatusGet
  ) where

import Data.Aeson
import Database.PostgreSQL.PQTypes.SQL.Builder
import Happstack.Server.Types
import Happstack.StaticRouting

import DB
import Kontra
import KontraPrelude
import Routing
import Util.MonadUtils
import qualified API.V2 as V2

monitorAPI :: Route (Kontra Response)
monitorAPI = dir "api" $ dir "v2" $ dir "monitor" $ choice [
    dir "status" $ hGet $ toK0 $ apiCallMonitorStatusGet
  ]

apiCallMonitorStatusGet :: Kontrakcja m => m Response
apiCallMonitorStatusGet  = V2.api $ do
  -- Do something useless to see, whether database works.
  -- Should crash with HTTP 500, when database is not available
  runQuery_ . sqlSelect "table_versions" $ do
    sqlResult "name"
    sqlLimit 1
  _ :: String <- guardJustM $ fetchMaybe runIdentity
  return $ V2.Ok $ object [ "status" .= ("ok"::String) ]
