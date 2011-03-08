{-# OPTIONS_GHC -Wall #-}
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
import Kontra
import ActionQueueState
import Data.Data
import Misc

-- |top-level application state
data AppState = AppState 
                deriving (Eq, Ord, Show, Data)
   
instance Typeable AppState where typeOf _ = mkTypeOf "AppState"

$(deriveSerialize ''AppState)
instance Version AppState

-- |top-level application component
instance Component AppState where
  type Dependencies AppState = Documents :+: Sessions :+: Users :+:PaymentAccountModels :+: Actions :+: End
  initialValue = AppState


-- create types for event serialization
$(mkMethods ''AppState [])
