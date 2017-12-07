-- | Slightly customized replacement of Prelude.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module KontraPrelude (
    module Control.Applicative
  , module Control.Monad
  , module Data.Algebra.Boolean
  , module Data.Foldable
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Monoid.Utils
  , module Prelude
  , (!!)
  , (.)
  , id
  , get
  , set
  , for
  , maybeRead
  , head
  , last
  , maximum
  , minimum
  , read
  , tail
  , fromJust
  , UnexpectedError(..)
  , unexpectedError
  , unexpectedErrorM
  ) where

import Control.Applicative
import Control.Category
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Catch
import Data.Algebra.Boolean
import Data.Foldable (foldMap)
import Data.Label (get, set)
import Data.List hiding ((!!), all, and, any, head, last, maximum, minimum, or, tail)
import Data.Maybe hiding (fromJust)
import Data.Monoid
import Data.Monoid.Utils
import Data.Typeable
import GHC.Stack (HasCallStack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((!!), (&&), (.), (||), all, and, any, error, head, id, last, maximum, minimum, not, or, read, tail)
import qualified Prelude as P

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

-- | Unexpected error message along with source code location.
data UnexpectedError = UnexpectedError {
  ueMessage  :: !String
, ueModule   :: !String
, ueLine     :: !Int
, uePosition :: !Int
} deriving (Eq, Ord, Typeable)

instance Show UnexpectedError where
  showsPrec _ UnexpectedError{..} = (ueModule ++)
    . (":" ++) . (show ueLine ++)
    . (":" ++) . (show uePosition ++)
    . (": " ++) . (ueMessage ++)

instance Exception UnexpectedError

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
  in  fromMaybe (P.error $ "reading failed (input was '" ++ s ++ "', reads returned '" ++ show parsedS ++ "')") $ do
        [(v, "")] <- return parsedS
        return v

-- | Replacement for 'Data.Maybe.fromJust'
-- that provides useful information on failure.
fromJust :: HasCallStack => Maybe a -> a
fromJust Nothing  = P.error "fromJust received Nothing"
fromJust (Just x) = x

-- | Throw 'UnexpectedError' exception.
unexpectedError :: Q Exp
unexpectedError = [|
  \msg -> let (modname, line, position) = $srcLocation
          in throw UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }
  |]

-- | Throw 'UnexpectedError' exception in a monadic context (requires 'MonadThrow').
unexpectedErrorM :: Q Exp
unexpectedErrorM = [|
  \msg -> let (modname, line, position) = $srcLocation
          in throwM UnexpectedError {
            ueMessage = msg
          , ueModule = modname
          , ueLine = line
          , uePosition = position
          }
  |]

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: HasCallStack => String -> a
emptyListError fname = P.error $ fname ++ " received an empty list"

indexOutOfBoundsError :: HasCallStack => String -> a
indexOutOfBoundsError fname = P.error $ fname ++ " received an out-of-bounds index"

negativeIndexError :: HasCallStack => String -> a
negativeIndexError fname = P.error $ fname ++ " received a negative index"

srcLocation :: Q Exp
srcLocation = do
  Loc{..} <- qLocation
  let (line, position) = loc_start
  runQ [| (loc_module, line, position) |]
