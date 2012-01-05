{-# LANGUAGE EmptyDataDecls #-}
module Undefined where

import Data.Data

-- | Data type that can't have any values except bottom.
-- Useful for doing some magic with types or enforcing
-- certain behaviors within type system.

data Undefined
deriving instance Data Undefined
deriving instance Eq Undefined
deriving instance Ord Undefined
deriving instance Show Undefined
deriving instance Typeable Undefined
