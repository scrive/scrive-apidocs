module Doc.API (
    documentAPI
  ) where

import Happstack.Server.Types
import Happstack.StaticRouting

import Doc.API.V1.Calls
import Doc.API.V2() -- API V2 is disabled now. But import here will force compilation
import Kontra
import KontraPrelude

documentAPI :: Route (Kontra Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" $ documentAPIV1
  , documentAPIV1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ documentAPIV1
  -- API V2 is disabled now.
  --, dir "v2" $ documentAPIV2
  ]
