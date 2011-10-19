{- |
   Stores the top-level application state
 -}
module AppState
    ( AppState
    ) where

import Doc.DocState
import Happstack.Data
import Happstack.State
import Payments.PaymentsState
import Session
import ActionSchedulerState
import Data.Data
import Misc
import API.Service.ServiceState
import Company.CompanyState
import User.UserState
import File.State

-- |top-level application state
data AppState = AppState
                deriving (Eq, Ord, Show, Data)

instance Typeable AppState where typeOf _ = mkTypeOf "AppState"

$(deriveSerialize ''AppState)
instance Version AppState

-- |top-level application component
instance Component AppState where
  type Dependencies AppState = Files :+: Documents :+: Sessions :+: Users :+: PaymentAccountModels :+: Actions :+: Companies :+: Services :+: End
  initialValue = AppState


-- create types for event serialization
$(mkMethods ''AppState [])
