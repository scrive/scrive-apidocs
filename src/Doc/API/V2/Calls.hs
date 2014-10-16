module Doc.API.V2.Calls (
    documentAPIV2
  ) where


import Happstack.Server.Types
import Happstack.StaticRouting

import Kontra

documentAPIV2 ::  Route (KontraPlus Response)
documentAPIV2  = choice [

  ]
