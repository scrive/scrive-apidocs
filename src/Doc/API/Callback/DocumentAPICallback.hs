module Doc.API.Callback.DocumentAPICallback (
    DocumentAPICallback(..)
  ) where

import Data.Int
import Data.Typeable
import Doc.DocumentID
import MinutesTime

data DocumentAPICallback = DocumentAPICallback {
    dacDocumentID :: DocumentID
  , dacExpires :: MinutesTime
  , dacURL :: String
  , dacAttempt :: Int32
  } deriving (Show, Typeable)
