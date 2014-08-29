module Doc.API (
    documentAPI
  ) where

import Happstack.StaticRouting
import Happstack.Server.Types
import Kontra
import Doc.API.V1.Calls
import Doc.API.V2.Calls

documentAPI :: Route (KontraPlus Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" $ documentAPIV1
  , documentAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ documentAPIV1
  , dir "v2" $ documentAPIV2
  ]