module Log.Identifier (
    Identifier(..)
  , identifier
  , identifier_
  , identifiers
  , (.=)
  , LogObject(..)
  , LogDefaultLabel(..)
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
  logObject :: a -> Value

class LogDefaultLabel a where
  logDefaultLabel :: a -> Text

--TODO using aeson >= 1.0 ??
--instance (LogPair a, Functor f, ToJSON1 f) => LogPair (f a) where
--  logObject f fa = f .= toJSON fa

logPair :: (LogObject a, LogDefaultLabel a) => (Text -> Text) -> a -> Pair
logPair f a = (f $ logDefaultLabel a, logObject a)

logPair_ :: (LogObject a, LogDefaultLabel a) => a -> Pair
logPair_ = logPair id
