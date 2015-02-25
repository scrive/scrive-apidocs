module Doc.API.Callback.Data (
    DocumentAPICallback(..)
  ) where

import Data.Int

import Doc.DocumentID

data DocumentAPICallback = DocumentAPICallback {
  dacID       :: !DocumentID
, dacURL      :: !String
, dacAttempts :: !Int32
} deriving (Eq, Ord, Show)
