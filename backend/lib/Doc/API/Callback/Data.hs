module Doc.API.Callback.Data (
    CallbackID
  , DocumentAPICallback(..)
  ) where

import Data.Aeson
import Data.Int

import API.APIVersion
import DB
import Doc.DocumentID
import KontraPrelude
import Log.Identifier

newtype CallbackID = CallbackID Int64
  deriving (Eq, Ord, PQFormat)
deriving newtype instance Read CallbackID
deriving newtype instance Show CallbackID

instance Identifier CallbackID Int64 where
  idDefaultLabel _       = "callback_id"
  idValue (CallbackID k) = toJSON k

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

instance Loggable DocumentAPICallback where
  logValue DocumentAPICallback{..} = object [
      identifier_ dacID
    , identifier_ dacApiVersion
    , "url" .= dacURL
    , "attempt_count" .= dacAttempts
    ]
  logDefaultLabel _ = "document_api_callback"
