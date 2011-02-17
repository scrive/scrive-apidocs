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

-- |top-level application state
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      data AppState = AppState 
   |])

$(deriveSerialize ''AppState)
instance Version AppState

-- |top-level application component
instance Component AppState where
  type Dependencies AppState = Documents :+: Sessions :+: Users :+:PaymentAccountModels :+: Actions :+: End
  initialValue = defaultValue


-- create types for event serialization
$(mkMethods ''AppState [])
