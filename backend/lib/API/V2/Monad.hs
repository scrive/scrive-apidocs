{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.V2.Monad (
  -- * Response types
    APIResponse(..)
  -- * API Monad runner
  , api
  -- * API Monad utils
  , apiGuardJustM
  , apiError
) where

import Control.Monad.Catch
import Data.Unjson
import Happstack.Server (toResponse)
import Happstack.Server.Types
import Log as Log
import Text.JSON hiding (Ok)
import Text.JSON.Gen hiding (object)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Happstack.Server.Response as Web

import API.V2.Errors
import DB
import Kontra
import KontraPrelude
import Text.JSON.Convert
import Util.CSVUtil

-- | Wrapper around any API response. If forces us to select HTTP response code
data APIResponse a = Ok a | Created a | Accepted a

-- Define what we can respond from an API call
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id

instance ToAPIResponse BSL.ByteString where
  toAPIResponse bs  =
    setHeader "Content-Type" "text/plain; charset=UTF-8" $ Web.toResponse $ bs

instance ToAPIResponse JSValue where
  toAPIResponse jv =
    setHeader "Content-Type" "application/json; charset=UTF-8" $ Web.toResponse $ encode jv

instance ToAPIResponse (UnjsonDef a,a) where
  toAPIResponse (unjson,a) =
    setHeader "Content-Type" "application/json; charset=UTF-8" $ Web.toResponse $ unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True }) unjson a

instance ToAPIResponse CSV where
  toAPIResponse v = let r1 = Web.toResponse $ v in
    setHeader "Content-Type" "text/csv" r1

instance (ToAPIResponse a, ToAPIResponse b) => ToAPIResponse (Either a b) where
  toAPIResponse = either toAPIResponse toAPIResponse

instance ToAPIResponse a => ToAPIResponse (APIResponse a) where
  toAPIResponse (Ok a) = (toAPIResponse a) { rsCode = 200 }
  toAPIResponse (Created a) = (toAPIResponse a) { rsCode = 201 }
  toAPIResponse (Accepted a) = (toAPIResponse a) { rsCode = 202 }

instance ToAPIResponse () where
  toAPIResponse () = toResponse ""


-- | Convert the `APIResponse` return type to the appropriate `Response`
-- This defines the possible outputs of the API.
api :: (Kontrakcja m, ToAPIResponse v) => m (APIResponse v) -> m Response
api acc =  (toAPIResponse <$> acc) `catches` [
    Handler $ \ex@(SomeDBExtraException e) -> do
      -- API handler always returns a valid response. Due to that appHandler will not rollback - and we need to do it here
      rollback
      -- For some exceptions we do a conversion to APIError
      let ex' = tryToConvertConditionalExceptionIntoAPIError ex

      logAttention "API v2 Error:" $ object [
        "error" .= jsonToAeson (toJSValue e),
        "response_json" .= jsonToAeson (jsonFromSomeDBExtraException ex')
        ]

      return $ (toAPIResponse $ jsonFromSomeDBExtraException ex') {
        rsCode = httpCodeFromSomeDBExtraException ex'
      }
  ]

apiGuardJustM :: (MonadThrow m) => APIError -> m (Maybe a) -> m a
apiGuardJustM e a = a >>= maybe (apiError e) return

apiError :: (MonadThrow m) => APIError -> m a
apiError e = throwM . SomeDBExtraException $ e
