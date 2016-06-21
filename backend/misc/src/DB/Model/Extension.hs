module DB.Model.Extension (
    Extension(..)
  , ununExtension
  ) where

import Data.String
import Database.PostgreSQL.PQTypes
import Data.Text (Text)

import KontraPrelude

newtype Extension = Extension { unExtension :: RawSQL () }
  deriving (Eq, Ord, Show, IsString)

ununExtension :: Extension -> Text
ununExtension = unRawSQL . unExtension
