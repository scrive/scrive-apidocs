{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, ScopedTypeVariables
    #-}

{- |
   Sores the top-level application state
 -}
module AppState 
    ( AppState
    ) where
import Happstack.Data
import Happstack.State
import User
import Session
import Data.Data
import qualified Happstack.Data.IxSet as IxSet (empty,size)
import DocState
import Payments.PaymentsState

-- |top-level application state
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      data AppState = AppState 
   |])

$(deriveSerialize ''AppState)
instance Version AppState

-- |top-level application component
-- we depend on the GuestBook component
instance Component AppState where
  type Dependencies AppState = Documents :+: Sessions :+: Users :+:PaymentAccountModels :+: End
  initialValue = defaultValue


-- create types for event serialization
$(mkMethods ''AppState [])

