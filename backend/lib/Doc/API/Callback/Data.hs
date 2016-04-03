module Doc.API.Callback.Data (
    CallbackID
  , DocumentAPICallback(..)
  ) where

import Data.Int

import API.APIVersion
import DB
import Doc.DocumentID
import KontraPrelude

newtype CallbackID = CallbackID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''CallbackID)

instance FromSQL CallbackID where
  type PQBase CallbackID = PQBase Int64
  fromSQL mbase = CallbackID <$> fromSQL mbase
instance ToSQL CallbackID where
  type PQDest CallbackID = PQDest Int64
  toSQL (CallbackID n) = toSQL n

data DocumentAPICallback = DocumentAPICallback {
  dacID         :: !CallbackID
, dacDocumentID :: !DocumentID
, dacApiVersion :: !APIVersion
, dacURL        :: !String
, dacAttempts   :: !Int32
} deriving (Eq, Ord, Show)
