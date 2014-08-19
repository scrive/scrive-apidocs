module PadApplication.API (
    padApplicationAPI
  ) where

import DB.SQL
import Happstack.StaticRouting
import KontraMonad
import Happstack.Server.Types
import Routing
import Kontra
import API.Monad
import Control.Exception.Lifted
import Control.Monad
import Happstack.Fields
import Text.JSON.Gen
import Data.Maybe
import Text.JSON.Types (JSValue(JSNull))

padApplicationAPI :: Route (KontraPlus Response)
padApplicationAPI = dir "api" $ choice
  [ dir "frontend" $ padApplicationAPI'
  , padApplicationAPI' -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ padApplicationAPI'
  , dir "v2" $ padApplicationAPI'
  ]

padApplicationAPI' :: Route (KontraPlus Response)
padApplicationAPI' = choice [
  dir "checkclient"     $ hPostNoXTokenHttp $ toK0 $ apiCallCheckClient
  ]


apiCallCheckClient :: Kontrakcja m => m Response
apiCallCheckClient = api $ do
    mclient  <- getField "client"
    when (isNothing mclient) $ do
      throwIO . SomeKontraException $ serverError "No client description"
    runJSONGenT $ do
      value "valid" True
      value "upgrade_warning" False
      value "valid_until" JSNull
      value "upgrade_url" JSNull
      value "message" JSNull
