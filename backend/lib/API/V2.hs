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
  , signatoryLinkForDocumentNotFound
  , documentActionForbidden
  , documentNotFound
  , resourceNotFound
  -- * Re-export modules
  , module API.V2.Monad
  , module API.V2.User
) where

import Happstack.Server (askRq)
import Happstack.Server.Types (Response, rqUri)
import qualified Data.Text as T

import API.V2.Errors
import API.V2.Monad
import API.V2.User
import Kontra (Kontrakcja)

-- Handler for 404 for API calls. Has to be inserted directly into system main routing table
noAPIV2CallFoundHandler :: Kontrakcja m => m Response
noAPIV2CallFoundHandler = api $ do
  uri <- rqUri <$> askRq
  void . apiError $ endpointNotFound (T.pack uri)
  return $ Ok () -- This part will never be reached, since exception is thrown above
