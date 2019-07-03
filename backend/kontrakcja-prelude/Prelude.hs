-- | Slightly customized replacement of Prelude.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports       #-}
module Prelude (
    module Control.Applicative
  , module Control.Monad
  , module Data.Algebra.Boolean
  , module Data.Foldable
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Monoid.Utils
  , module P
  , (!!)
  , (.)
  , id
  , get
  , set
  , copy
  , for
  , maybeRead
  , head
  , last
  , maximum
  , minimum
  , read
  , tail
  , fromJust
  , whenJust
  , error
  , unexpectedError
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Extra
import Data.Algebra.Boolean
import Data.Foldable (foldMap)
import Data.Label ((:->), get, set)
import Data.List hiding
  ( (!!), all, and, any, head, last, maximum, minimum, or, tail )

import Data.Maybe hiding (fromJust)
import Data.Monoid
import Data.Monoid.Utils
import GHC.Stack (HasCallStack, withFrozenCallStack)
import "base" Prelude hiding
  ( (!!), (&&), (.), (||), all, and, any, error, head, id, last, maximum
  , minimum, not, or, read, tail )

import qualified "base" Prelude as P

-- | Boolean algebra of functions.
instance Boolean (a -> Bool) where
  true   = const True
  false  = const False
  not f  = not . f
  (&&)   = liftA2 (&&)
  (||)   = liftA2 (||)
  xor    = liftA2 xor
  (-->)  = liftA2 (-->)
  (<-->) = liftA2 (<-->)

----------------------------------------
-- Additional fclabels utilities.

-- | Copy the field value from an object of the same type.
copy :: (f :-> o) -> f -> f -> f
copy x fromThis toThat = set x (get x fromThis) $ toThat

----------------------------------------

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Read a value and return 'Nothing' if an error occurs during parsing.
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(v, "")] -> Just v
  _         -> Nothing

----------------------------------------

-- | Replacement for 'P.!!' that provides useful information on failure.
(!!) :: HasCallStack => [a] -> Int -> a
xs !! n
  | n < 0     = negativeIndexError "!!"
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (indexOutOfBoundsError "!!") xs n

-- | Replacement for 'P.head' that provides useful information on failure.
head :: HasCallStack => [a] -> a
head = emptyList P.head $ emptyListError "head"

-- | Replacement for 'P.tail' that provides useful information on failure.
tail :: HasCallStack => [a] -> [a]
tail = emptyList P.tail $ emptyListError "tail"

-- | Replacement for 'P.last' that provides useful information on failure.
last :: HasCallStack => [a] -> a
last = emptyList P.last $ emptyListError "last"

-- | Replacement for 'P.maximum' that provides useful information on failure.
maximum :: (HasCallStack, Ord a) => [a] -> a
maximum = emptyList P.maximum $ emptyListError "maximum"

-- | Replacement for 'P.minimum' that provides useful information on failure.
minimum :: (HasCallStack, Ord a) => [a] -> a
minimum = emptyList P.minimum $ emptyListError "minimum"

-- | Replacement for 'P.read' that provides useful information on failure.
read :: (HasCallStack, Read a, Show a) => String -> a
read s =
  let parsedS = reads s
  in  fromMaybe (unexpectedError $ "reading failed (input was '"
                  ++ s ++ "', reads returned '" ++ show parsedS ++ "')") $ do
        [(v, "")] <- return parsedS
        return v

-- | Replacement for 'Data.Maybe.fromJust' that provides useful
-- information on failure.
fromJust :: HasCallStack => Maybe a -> a
fromJust Nothing  = unexpectedError "fromJust received Nothing"
fromJust (Just x) = x

{-# WARNING error "Use 'unexpectedError' instead." #-}
error :: HasCallStack => String -> a
error errMsg = withFrozenCallStack $ P.error errMsg

-- | Like 'error', but with a more conspicous name.
unexpectedError :: HasCallStack => String -> a
unexpectedError errMsg = withFrozenCallStack $ P.error errMsg

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: HasCallStack => String -> a
emptyListError fname = unexpectedError $ fname ++ " received an empty list"

indexOutOfBoundsError :: HasCallStack => String -> a
indexOutOfBoundsError fname = unexpectedError $
                              fname ++ " received an out-of-bounds index"

negativeIndexError :: HasCallStack => String -> a
negativeIndexError fname = unexpectedError $
                           fname ++ " received a negative index"
