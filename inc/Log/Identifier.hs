module Log.Identifier (
    Identifier(..)
  , identifier
  , identifier_
  , identifiers
  , (.=)
  ) where

import Data.Aeson.Types
import Data.Functor.Identity
import Data.Monoid
import Data.Text (Text)

import KontraPrelude

class ToJSON b => Identifier t b | t -> b where
  gidentifier :: forall f. (Functor f, ToJSON (f b))
              => (Text -> Text) -> f t -> Pair

identifier :: Identifier t b => (Text -> Text) -> t -> Pair
identifier f = gidentifier f . Identity

identifier_ :: Identifier t b => t -> Pair
identifier_ = gidentifier id . Identity

identifiers :: Identifier t b => [t] -> Pair
identifiers = gidentifier (<> "s")
