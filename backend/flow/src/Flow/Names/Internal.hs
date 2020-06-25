{-# LANGUAGE StrictData #-}

module Flow.Names.Internal
  ( Name(..)
  , DocumentName
  , UserName
  , MessageName
  , FieldName
  , StageName
  ) where

import Data.Aeson
import Database.PostgreSQL.PQTypes

data NameKind
    = DocumentName
    | UserName
    | MessageName
    | FieldName
    | StageName

newtype Name (a :: NameKind) = Name Text
  deriving (Eq, FromJSON, Ord, Show, ToJSON)

instance PQFormat (Name a) where
  pqFormat = pqFormat @Text

instance FromSQL (Name a) where
  type PQBase (Name a) = PQBase Text
  fromSQL mbase = Name <$> fromSQL mbase

instance ToSQL (Name a) where
  type PQDest (Name a) = PQDest Text
  toSQL (Name n) = toSQL n


type DocumentName = Name 'DocumentName
type UserName = Name 'UserName
type MessageName = Name 'MessageName
type FieldName = Name 'FieldName
type StageName = Name 'StageName
