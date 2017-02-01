module Log.Identifier (
    Identifier(..)
  , identifier
  , identifier_
  , (.=)
  , Loggable(..)
  , logPair
  , logPair_
  , logObject
  , logObject_
  ) where

import Data.Aeson.Types
import Data.Monoid
import Data.Text (Text)
import GHC.Exts (fromList)

import KontraPrelude

class Identifier t b | t -> b where
  idDefaultLabel :: t -> Text
  idValue        :: t -> Value

identifier :: Identifier t b => (Text -> Text) -> t -> Pair
identifier f n = f (idDefaultLabel n) .= idValue n

identifier_ :: Identifier t b => t -> Pair
identifier_ = identifier id

instance Identifier t b => Identifier [t] b where
  idDefaultLabel _ = idDefaultLabel (undefined::t) <> "s"
  idValue ns       = Array $ fromList $ fmap idValue ns

instance Identifier t b => Identifier (Maybe t) b where
  idDefaultLabel _ = idDefaultLabel (undefined::t)
  idValue Nothing  = Null
  idValue (Just n) = idValue n

class Loggable a where
  -- | Type can be converted for logging purposes by encoding (parts of)
  --   it to Aeson 'object'. This is not the same as ToJSON, because we don't have
  --   to encode all information. This conversion is not meant to be reversible.
  --   Instead of implementing ToJSON, we decided to explicitely mark the intent,
  --   that this conversion is only for logging purposes.
  logValue :: a -> Value
  -- | When structuring data for logging, we want to be consistent in naming
  --   data being logged. This way each type can have a default label.
  logDefaultLabel :: a -> Text

-- | Log datatype as a top level log structure
logObject :: (Loggable a) => (Text -> Text) -> a -> Value
logObject f a = object [ logPair f a ]

logObject_ :: (Loggable a) => a -> Value
logObject_ = logObject id

-- | Convert datatype to Aeson Pair for logging purposes. Allows adjusting or
--   replacing of the default label
logPair :: (Loggable a) => (Text -> Text) -> a -> Pair
logPair f a = (f $ logDefaultLabel a, logValue a)

-- | Convert datatype to Aeson Pair for logging purposes. Default label will
--   be used.
logPair_ :: (Loggable a) => a -> Pair
logPair_ = logPair id
