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
$(newtypeDeriveUnderlyingReadShow ''CallbackID)

instance Identifier CallbackID Int64 where
  gidentifier f n = f "callback_id" .= fmap (\(CallbackID k) -> k) n

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

instance LogObject DocumentAPICallback where
  logObject DocumentAPICallback{..} = object [
      identifier_ dacID
    , identifier_ dacApiVersion
    , "url" .= dacURL
    ]

instance LogDefaultLabel DocumentAPICallback where
  logDefaultLabel _ = "document_api_callback"
