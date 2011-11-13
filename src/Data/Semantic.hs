-- | Special, semantic pairs.

module Data.Semantic
       (
        And(..), 
        Or(..),
        Not(..)
       )
       
       where

data And a b = And a b

data Or a b = Or a b

data Not a = Not a
