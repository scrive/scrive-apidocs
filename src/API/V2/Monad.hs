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

import Control.Exception.Lifted
import Control.Monad.Base
import Data.Unjson
import Happstack.Server (toResponse)
import Happstack.Server.Types
import Text.JSON hiding (Ok)
import Text.JSON.Gen hiding (object)
import qualified Happstack.Server.Response as Web

import API.V2.Errors
import DB
import Kontra
import KontraPrelude
import Log as Log
import Text.JSON.Convert
import Util.CSVUtil
import qualified Data.ByteString.Lazy.Char8 as BSL

-- | Wrapper around any API response. If forces us to select HTTP reposponse code
data APIResponse a = Ok a | Created a | Accepted a

-- Define what we can respond from an API call
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id

instance ToAPIResponse BSL.ByteString where
  toAPIResponse bs  =
    -- must be text/plain because some browsers complain about JSON type
    setHeader "Content-Type" "text/plain; charset=UTF-8" $ Web.toResponse $ bs

instance ToAPIResponse JSValue where
  toAPIResponse jv =
    -- must be text/plain because some browsers complain about JSON type
    setHeader "Content-Type" "text/plain; charset=UTF-8" $ Web.toResponse $ encode jv

instance ToAPIResponse (UnjsonDef a,a) where
  toAPIResponse (unjson,a) =
    -- must be text/plain because some browsers complain about JSON type
    setHeader "Content-Type" "text/plain; charset=UTF-8" $ Web.toResponse $ unjsonToByteStringLazy' (Options { pretty = True, indent = 2, nulls = True }) unjson a

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
    Handler $ \ex@(SomeKontraException e) -> do
      -- API handler always returns a valid response. Due to that appHandler will not rollback - and we need to do it here
      rollback
      logAttention "API v2 Error (may convert to error response):" $ object ["error" .= jsonToAeson (toJSValue e)]
      -- For some exceptions we do a conversion to APIError
      let ex' = tryToConvertConditionalExpectionIntoAPIError ex
      return $ (toAPIResponse $ jsonFromSomeKontraException $ tryToConvertConditionalExpectionIntoAPIError ex') {
        rsCode = httpCodeFromSomeKontraException $ tryToConvertConditionalExpectionIntoAPIError ex'
      }
  ]

apiGuardJustM :: (MonadBase IO m) => APIError -> m (Maybe a) -> m a
apiGuardJustM e a = a >>= maybe (apiError e) return

apiError :: (MonadBase IO m) => APIError -> m a
apiError e = throwIO . SomeKontraException $ e
