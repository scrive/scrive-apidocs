module PadApplication.API (
    padApplicationAPI
  ) where

import Control.Exception.Lifted
import Happstack.Server.Types
import Happstack.StaticRouting
import Text.JSON.Gen
import Text.JSON.Types (JSValue(JSNull))

import API.Monad
import DB.SQL
import Happstack.Fields
import Kontra
import KontraPrelude
import Routing

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
