module Doc.API.Callback.Types (
    CallbackID
  , DocumentAPICallback(..)
  ) where

import Data.Aeson
import Data.Int

import API.APIVersion
import DB
import Doc.DocumentID
import Log.Identifier

newtype CallbackID = CallbackID Int64
  deriving (Eq, Ord)
deriving newtype instance Read CallbackID
deriving newtype instance Show CallbackID

instance PQFormat CallbackID where
  pqFormat = pqFormat @Int64

instance Identifier CallbackID where
  idDefaultLabel = "callback_id"
  idValue (CallbackID k) = int64AsStringIdentifier k

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
, dacURL        :: !Text
, dacAttempts   :: !Int32
} deriving (Eq, Ord, Show)

instance Loggable DocumentAPICallback where
  logValue DocumentAPICallback {..} = object
    [ identifier dacID
    , identifier dacApiVersion
    , "url" .= dacURL
    , "attempt_count" .= dacAttempts
    ]
  logDefaultLabel _ = "document_api_callback"
