module Doc.API (
    documentAPI
  ) where

import Happstack.Server.Types
import Happstack.StaticRouting

import Doc.API.V1.Calls
import Doc.API.V2
import Kontra

documentAPI :: Route (Kontra Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" documentAPIV2
  , documentAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" documentAPIV1
  , dir "v2" documentAPIV2 -- Only 'list' and 'history' calls. Replace with documentAPIV2 when frontend is ready
  ]
