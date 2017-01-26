module Log.Identifier (
    Identifier(..)
  , identifier
  , identifier_
  , identifiers
  , (.=)
  , LogObject(..)
  , logPair
  , logPair_
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

class LogObject a where
  -- | Type can be converted for logging purposes by encoding (parts of)
  --   it to Aeson 'object'. This is not the same as ToJSON, because we don't have
  --   to encode all information. This conversion is not meant to be reversible.
  --   Instead of implementing ToJSON, we decided to explicitely mark the intent,
  --   that this conversion is only for logging purposes.
  logObject :: a -> Value
  -- | When structuring data for logging, we want to be consistent in naming
  --   data being logged. This way each type can have a default label.
  logDefaultLabel :: a -> Text

-- | Convert datatype to Aeson Pair for logging purposes. Allows adjusting or
--   replacing of the default label
logPair :: (LogObject a) => (Text -> Text) -> a -> Pair
logPair f a = (f $ logDefaultLabel a, logObject a)

-- | Convert datatype to Aeson Pair for logging purposes. Default label will
--   be used.
logPair_ :: (LogObject a) => a -> Pair
logPair_ = logPair id
