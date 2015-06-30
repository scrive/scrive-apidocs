module API.V2 (
    noAPIV2CallFoundHandler
  -- * From API.V2.Errors
  , serverError
  , requestParameterMissing
  , requestParameterParseError
  , requestParameterInvalid
  , documentObjectVersionMismatch
  , documentStateError
  , documentStateErrorWithCode
  , signatoryStateError
  , documentActionForbidden
  , documentNotFound
  , resourceNotFound
  -- * Re-export modules
  , module API.V2.Monad
  , module API.V2.User
) where

import API.V2.Errors
import API.V2.Monad
import API.V2.User

import Data.Text (pack)
import Happstack.Server (askRq)
import Happstack.Server.Types (Response, rqUri)

import Kontra (Kontrakcja)
import KontraPrelude

-- | Helper for constructing API V2 routes
noAPIV2CallFoundHandler :: Kontrakcja m => m Response
noAPIV2CallFoundHandler = api $ do
  uri <- rqUri <$> askRq
  _ <- apiError $ endpointNotFound (pack uri)
  return $ Ok ()
