module DB.Model.Extension (
    Extension(..)
  , ununExtension
  ) where

import Data.ByteString (ByteString)
import Data.String
import Database.PostgreSQL.PQTypes

import KontraPrelude

newtype Extension = Extension { unExtension :: RawSQL () }
  deriving (Eq, Ord, Show, IsString)

ununExtension :: Extension -> ByteString
ununExtension = unRawSQL . unExtension
