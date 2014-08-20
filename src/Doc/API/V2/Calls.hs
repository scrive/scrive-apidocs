module Doc.API.V2.Calls (
    documentAPIV2
  ) where


import Happstack.StaticRouting
import Happstack.Server.Types
import Kontra

documentAPIV2 ::  Route (KontraPlus Response)
documentAPIV2  = choice [

  ]