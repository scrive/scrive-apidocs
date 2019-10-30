{-# LANGUAGE AllowAmbiguousTypes #-}
module Log.Identifier (
    Identifier(..)
  , IdentifierValue
  , stringIdentifier
  , int64AsStringIdentifier
  , intIdentifier
  , identifier
  , identifierMapLabel
  , (.=)
  , Loggable(..)
  , logPair
  , logPair_
  , logObject
  , logObject_
  ) where

import Data.Aeson.Types as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Exts (fromList)

-- | We use String identifiers for everything to workaround issues
-- JavaScript has with big numbers, and Ints in rare cases when we
-- know that the value will fit into 52 bits.
data IdentifierValue = IdentifierInt    !Int
                     | IdentifierString !String
                     | IdentifierJSON   !Value
                       -- ^ Used only for implementing Maybe and [a]
                       -- 'Identifier' instances.

instance ToJSON IdentifierValue where
  toJSON (IdentifierInt    i  ) = toJSON i
  toJSON (IdentifierString str) = toJSON str
  toJSON (IdentifierJSON   val) = val

  toEncoding (IdentifierInt    i  ) = toEncoding i
  toEncoding (IdentifierString str) = toEncoding str
  toEncoding (IdentifierJSON   val) = toEncoding val

class Identifier t where
  idDefaultLabel :: Text
  idValue        :: t -> IdentifierValue

-- Helpers for implementing 'Identifier' instances.
-- Usage:
--
-- instance Identifier Foo where
--     idDefaultLabel  = "foo"
--     idValue (Foo f) = {string,int64AsString,int}Identifier f

stringIdentifier :: String -> IdentifierValue
stringIdentifier = IdentifierString

int64AsStringIdentifier :: Int64 -> IdentifierValue
int64AsStringIdentifier = IdentifierString . show

-- | Should only be used when we can guarantee that the value will fit
-- into 52 bits (limitation due to JavaScript/JSON semantics),
-- normally 'int64AsStringIdentifier' should be used instead.
intIdentifier :: Int -> IdentifierValue
intIdentifier = IdentifierInt

jsonIdentifier :: ToJSON a => a -> IdentifierValue
jsonIdentifier = IdentifierJSON . toJSON

-- Default instances.

instance Identifier t => Identifier [t] where
  idDefaultLabel = idDefaultLabel @t <> "s"
  idValue ns = IdentifierJSON $ Array $ fromList $ fmap (toJSON . idValue) ns

instance Identifier t => Identifier (Maybe t)  where
  idDefaultLabel = idDefaultLabel @t
  idValue Nothing  = jsonIdentifier Null
  idValue (Just n) = idValue n

-- Helpers for client code.

identifier :: forall  t kv . (Identifier t, Aeson.KeyValue kv) => t -> kv
identifier n = (idDefaultLabel @t) .= (toJSON . idValue $ n)

identifierMapLabel
  :: forall  t kv . (Identifier t, Aeson.KeyValue kv) => (Text -> Text) -> t -> kv
identifierMapLabel f n = (f $ idDefaultLabel @t) .= (toJSON . idValue $ n)

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
logObject f a = object [logPair f a]

logObject_ :: (Loggable a) => a -> Value
logObject_ = logObject identity

-- | Convert datatype to Aeson Pair for logging purposes. Allows adjusting or
--   replacing of the default label
logPair :: (Loggable a) => (Text -> Text) -> a -> Pair
logPair f a = (f $ logDefaultLabel a, logValue a)

-- | Convert datatype to Aeson Pair for logging purposes. Default label will
--   be used.
logPair_ :: (Loggable a) => a -> Pair
logPair_ = logPair identity
