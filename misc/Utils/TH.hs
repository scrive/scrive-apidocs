module Utils.TH where

import Language.Haskell.TH
import Prelude

isConstr :: Name -> Q Exp
isConstr _con = [|
    \t -> case t of
      $(recP _con []) -> True
      _               -> False
  |]
