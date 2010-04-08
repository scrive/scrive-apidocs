{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving
    #-}
module AppState where
import Happstack.Data
import Happstack.State
import GuestBook
import User
import Session
import Data.Data
import qualified Happstack.Data.IxSet as IxSet (empty)
import DocState


-- |top-level application state
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      data AppState = AppState 
   |])

$(deriveSerialize ''AppState)
instance Version AppState

                     
instance Component (Sessions UserID) where
  type Dependencies (Sessions UserID) = End
  initialValue = IxSet.empty

-- |top-level application component
-- we depend on the GuestBook component
instance Component AppState where
  type Dependencies AppState = Documents :+: Sessions UserID :+: Users :+: End
  initialValue = defaultValue
  
-- create types for event serialization
$(mkMethods ''AppState [])
