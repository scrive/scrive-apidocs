-- | Special, semantic pairs.

module Data.Pairs 
       (
        And(..), 
        Or(..)
       )
       
       where

data And a b = And a b

data Or a b = Or a b
