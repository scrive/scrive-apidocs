module Doc.API.Callback.Types
  ( CallbackID
  , DocumentAPICallback(..)
  ) where

import Data.Aeson
import Data.Int

import API.APIVersion
import Callback.Types (CallbackID)
import Doc.DocumentID
import Log.Identifier

data DocumentAPICallback = DocumentAPICallback
  { dacID         :: CallbackID
  , dacDocumentID :: DocumentID
  , dacApiVersion :: APIVersion
  , dacURL        :: Text
  , dacAttempts   :: Int32
  } deriving (Eq, Ord, Show)

instance Loggable DocumentAPICallback where
  logValue DocumentAPICallback {..} = object
    [ identifier dacID
    , identifier dacApiVersion
    , "url" .= dacURL
    , "attempt_count" .= dacAttempts
    ]
  logDefaultLabel _ = "document_api_callback"
