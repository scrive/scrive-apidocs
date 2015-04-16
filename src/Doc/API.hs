module Doc.API (
    documentAPI
  ) where

import Happstack.Server.Types
import Happstack.StaticRouting

import Doc.API.V1.Calls
import Doc.API.V2.Calls
import Kontra
import KontraPrelude

documentAPI :: Route (Kontra Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" $ documentAPIV1
  , documentAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ documentAPIV1
  , dir "v2" $ documentAPIV2
  ]
