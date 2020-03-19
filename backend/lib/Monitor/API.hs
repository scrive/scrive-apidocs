module Monitor.API (
    monitorAPI
  , apiCallMonitorStatusGet
  ) where

import Data.Aeson
import Happstack.Server.Types
import Happstack.StaticRouting

import DB
import Kontra
import Routing
import Util.MonadUtils
import qualified API.V2 as V2

monitorAPI :: Route (Kontra Response)
monitorAPI = dir "api" . dir "v2" $ dir
  "monitor"
  (choice [(dir "status" . hGet . toK0) apiCallMonitorStatusGet])

apiCallMonitorStatusGet :: Kontrakcja m => m Response
apiCallMonitorStatusGet = V2.api $ do
  -- Do something useless to see, whether database works.
  -- Should crash with HTTP 500, when database is not available
  runQuery_ ("SELECT TRUE" :: SQL)
  (_true :: Bool) <- guardJustM $ fetchMaybe runIdentity
  return . V2.Ok $ object ["status" .= ("ok" :: String)]
