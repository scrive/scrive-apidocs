-- | Slightly customized replacement of Prelude.
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PackageImports #-}
module Prelude (
    module Control.Applicative
  , module Control.Monad
  , module Data.Algebra.Boolean
  , module Data.Foldable
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Monoid.Utils
  , module O
  , module P
  , Text
  , TextShow (..)
  , MonadFail (..)
  , (!!)
  -- optics
  , (&), (%), (?~)
  , view, (^.)
  , preview, (^?)
  , toListOf, (^..)
  , over, (%~)
  , set, (.~)
  , copy
  -- prelude
  , identity
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
  , whenNothing
  , error
  , unexpectedError
  ) where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Extra hiding (fail)
import Control.Monad.Fail (MonadFail(..))
import Data.Algebra.Boolean
import Data.Foldable (foldMap)
import Data.List hiding
  ( (!!), all, and, any, head, last, maximum, minimum, or, tail
  )
import Data.Maybe hiding (fromJust)
import Data.Monoid
import Data.Monoid.Utils
import Data.Text (Text)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Optics
import Text.JSON.FromJSValue
import Text.JSON.ToJSValue
import TextShow
import "base" Prelude hiding
  ( (!!), (&&), (||), all, and, any, error, fail, head, id, last, maximum
  , minimum, not, or, read, tail
  )
import qualified Data.Either.Optics as O
import qualified Data.Maybe.Optics as O
import qualified Data.Text as T
import qualified Data.Tuple.Optics as O
import qualified Optics as O
  ( A_Fold, A_Getter, A_Lens, A_Prism, A_ReversedLens, A_ReversedPrism, A_Review
  , A_Setter, A_Traversal, AffineFold, AffineTraversal, AffineTraversal'
  , An_AffineFold, An_AffineTraversal, An_Iso, Fold, Getter, Iso, Iso'
  , IxAffineFold, IxAffineTraversal, IxAffineTraversal', IxFold, IxGetter
  , IxLens, IxLens', IxSetter, IxSetter', IxTraversal, IxTraversal', Lens, Lens'
  , NoIx, Optic, Optic', Prism, Prism', ReversedLens, ReversedLens'
  , ReversedPrism, ReversedPrism', Review, Setter, Setter', Traversal
  , Traversal', WithIx
  )
import qualified "base" Prelude as P hiding (fail)

-- | Boolean algebra of functions.
instance Boolean (a -> Bool) where
  true  = const True
  false = const False
  not f = not . f
  (&&)   = liftA2 (&&)
  (||)   = liftA2 (||)
  xor    = liftA2 xor
  (-->)  = liftA2 (-->)
  (<-->) = liftA2 (<-->)

-- Define a default instance of TextShow from show.
-- This is a workaround until we move completely to
-- GHC 8.6 to be able to use DerivingVia to derive
-- TextShow directly.
instance {-# OVERLAPPABLE #-}
  (Show a)
  => TextShow a
   where
  showb = fromString . show

instance FromJSValue Text where
  fromJSValue = fmap T.pack . fromJSValue

instance ToJSValue Text where
  toJSValue = toJSValue . T.unpack

----------------------------------------
-- Additional optics utilities.

{-# ANN copy ("HLint: ignore Eta reduce" :: String) #-}
-- | Copy the field value from an object of the same type.
copy :: (Is k A_Setter, Is k A_Getter) => Optic k is s s a a -> s -> s -> s
copy x fromThis toThat = set x (view x fromThis) toThat

----------------------------------------

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Read a value and return 'Nothing' if an error occurs during parsing.
maybeRead :: Read a => Text -> Maybe a
maybeRead s = case reads (T.unpack s) of
  [(v, "")] -> Just v
  _         -> Nothing

----------------------------------------

-- | Replacement for 'id' that doesn't collide with "identifier".
identity :: a -> a
identity = P.id

-- | Replacement for 'P.!!' that provides useful information on failure.
(!!) :: HasCallStack => [a] -> Int -> a
xs !! n
  | n < 0 = negativeIndexError "!!"
  | otherwise = foldr
    (\x r k -> case k of
      0 -> x
      _ -> r (k - 1)
    )
    (indexOutOfBoundsError "!!")
    xs
    n

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
read :: (HasCallStack, Read a, TextShow a) => Text -> a
read s =
  let parsedS = reads $ T.unpack s
  in  fromMaybe
          (  unexpectedError
          $  "reading failed (input was '"
          <> s
          <> "', reads returned '"
          <> showt parsedS
          <> "')"
          )
        $ do
            [(v, "")] <- return parsedS
            return v

-- | Replacement for 'Data.Maybe.fromJust' that provides useful
-- information on failure.
fromJust :: HasCallStack => Maybe a -> a
fromJust Nothing  = unexpectedError "fromJust received Nothing"
fromJust (Just x) = x

-- | whenNothing mA fa = maybe fa return mA
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m

{-# WARNING error "Use 'unexpectedError' instead." #-}
error :: HasCallStack => Text -> a
error errMsg = withFrozenCallStack . P.error $ T.unpack errMsg

-- | Like 'error', but with a more conspicous name.
unexpectedError :: HasCallStack => Text -> a
unexpectedError errMsg = withFrozenCallStack . P.error $ T.unpack errMsg

---- internal stuff below ----

emptyList :: ([a] -> t) -> t -> [a] -> t
emptyList f err v = if null v then err else f v

emptyListError :: HasCallStack => Text -> a
emptyListError fname = unexpectedError $ fname <> " received an empty list"

indexOutOfBoundsError :: HasCallStack => Text -> a
indexOutOfBoundsError fname =
  unexpectedError $ fname <> " received an out-of-bounds index"

negativeIndexError :: HasCallStack => Text -> a
negativeIndexError fname = unexpectedError $ fname <> " received a negative index"
