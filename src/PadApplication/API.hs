module PadApplication.API (
    padApplicationAPI
  ) where


import Happstack.StaticRouting
import KontraMonad
import Happstack.Server.Types
import Routing
import API.APIVersion (APIVersion(..))
import Kontra
import API.Monad
import Control.Monad.Error
import Happstack.Fields
import Text.JSON.Gen
import Data.Maybe
import Text.JSON.Types (JSValue(JSNull))

padApplicationAPI :: Route (KontraPlus Response)
padApplicationAPI = dir "api" $ choice
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]

versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [
  dir "checkclient"     $ hPostNoXTokenHttp $ toK0 $ apiCallCheckClient
  ]


apiCallCheckClient :: Kontrakcja m => m Response
apiCallCheckClient = api $ do
    mclient  <- lift $ getField "client"
    when (isNothing mclient) $ do
      throwError $ serverError "No client description"
    runJSONGenT $ do
      value "valid" True
      value "valid_until" JSNull

