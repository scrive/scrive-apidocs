{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.V2.Monad (
  -- * Response types
    APIResponse(..)
  , ToAPIResponse(..)
  -- * API Monad runner
  , api
  -- * API Monad utils
  , module MonadUtils
) where

import Control.Monad.Catch
import Data.Aeson ((.=), object)
import Data.Unjson
import Happstack.Server (askRq, body, lookPairsBS, queryString, toResponse)
import Happstack.Server.Types
import Log
import Text.JSON hiding (Ok)
import Text.JSON.Gen hiding (object)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSU
import qualified Happstack.Server.Response as Web

import API.APIVersion
import API.Logging
import API.V2.Errors
import API.V2.MonadUtils as MonadUtils
import API.V2.User
import DB
import Kontra
import Text.JSON.Convert
import User.APILog.Model
import User.Model
import Util.CSVUtil

-- | Wrapper around any API response. If forces us to select HTTP response code
data APIResponse a = Ok a | Created a | Accepted a

-- Define what we can respond from an API call
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = identity

instance ToAPIResponse BSL.ByteString where
  toAPIResponse bs =
    setHeader "Content-Type" "text/plain; charset=UTF-8" $ Web.toResponse $ bs

instance ToAPIResponse JSValue where
  toAPIResponse jv =
    setHeader "Content-Type" "application/json; charset=UTF-8" $ Web.toResponse $ encode
      jv

instance ToAPIResponse A.Value where
  toAPIResponse aesonvalue =
    setHeader "Content-Type" "application/json; charset=UTF-8" $ Web.toResponse $ A.encode
      aesonvalue

instance ToAPIResponse A.Encoding where
  toAPIResponse aesonencoding =
    setHeader "Content-Type" "application/json; charset=UTF-8"
      $ Web.toResponse
      $ AE.encodingToLazyByteString aesonencoding

instance ToAPIResponse (UnjsonDef a,a) where
  toAPIResponse (unjson, a) =
    setHeader "Content-Type" "application/json; charset=UTF-8"
      $ Web.toResponse
      $ unjsonToByteStringLazy' unjsonOpts unjson a
    where unjsonOpts = Options { pretty = False, indent = 0, nulls = True }

instance ToAPIResponse CSV where
  toAPIResponse v =
    let r1 = Web.toResponse $ v in setHeader "Content-Type" "text/csv" r1

instance (ToAPIResponse a, ToAPIResponse b) => ToAPIResponse (Either a b) where
  toAPIResponse = either toAPIResponse toAPIResponse

instance ToAPIResponse a => ToAPIResponse (APIResponse a) where
  toAPIResponse (Ok       a) = (toAPIResponse a) { rsCode = 200 }
  toAPIResponse (Created  a) = (toAPIResponse a) { rsCode = 201 }
  toAPIResponse (Accepted a) = (toAPIResponse a) { rsCode = 202 }

instance ToAPIResponse () where
  toAPIResponse () = toResponse ""

-- | Convert the `APIResponse` return type to the appropriate `Response`
-- This defines the possible outputs of the API.
apiRun :: (Kontrakcja m, ToAPIResponse v) => m (APIResponse v) -> m Response
apiRun acc =
  (toAPIResponse <$> runAcc)
    `catches` [ Handler $ \ex@(SomeDBExtraException e) -> do
        -- API handler always returns a valid response. Due to that
        -- appHandler will not rollback - and we need to do it here
                  rollback
                  -- For some exceptions we do a conversion to APIError
                  let ex' = tryToConvertConditionalExceptionIntoAPIError ex

                  logInfo "API v2 Error:" $ object
                    [ "extra_exception" .= jsonToAeson (toJSValue e)
                    , "response_json" .= jsonToAeson (jsonFromSomeDBExtraException ex')
                    ]

                  return $ (toAPIResponse $ jsonFromSomeDBExtraException ex')
                    { rsCode = httpCodeFromSomeDBExtraException ex'
                    }
              ]
  where runAcc = addAPIUserToContext >> logUserCompanyIPAndApiVersion V2 acc

apiLog :: Kontrakcja m => m Response -> m Response
apiLog acc = do
  response <- acc
  request  <- askRq
  ctx      <- getContext
  when (get ctxisapilogenabled ctx && isAPIV2Call request) $ do
    mUser <- catchDBExtraException
      ((Just . fst) <$> getAPIUserWithAnyPrivileges)
      (\(APIError { errorType = InvalidAuthorization }) -> return Nothing)
    case mUser of
      Just user -> do
        queryPairs <- queryString $ lookPairsBS
        bodyPairs  <- body $ lookPairsBS
        let logData = CallLogData
              { cldRequest  = CallLogRequest { clrqURI        = rqUri request
                                             , clrqParamsGet = apiCallParam <$> queryPairs
                                             , clrqMethod     = show $ rqMethod request
                                             , clrqParamsPost = apiCallParam <$> bodyPairs
                                             }
              , cldResponse = CallLogResponse { clrsCode = fromIntegral $ rsCode response
                                              , clrsBody = BSU.toString $ rsBody response
                                              }
              }
        void . dbUpdate $ CreateCallLogItem (userid user) logData
      _ -> return ()
  return response
  where
    -- we will not read the file again from disk, it's enough to tell the
    -- developer, that this was a file
    apiCallParam (name, Left _filename) = CallLogParam name "transferred file"
    -- to save space, we will not store more than 50kB parameter
    apiCallParam (name, Right contents) = case BSU.length contents > 50000 of
      False -> CallLogParam name (BSU.toString contents)
      True  -> CallLogParam
        name
        (BSU.toString $ BS.concat
          [ BS.take 50000 $ contents
          , "..."
          , BSU.fromString . show . BS.length $ contents
          , " bytes of data"
          ]
        )

isAPIV2Call :: Request -> Bool
isAPIV2Call rq = "/api/v2/" `isPrefixOf` rqUri rq

api :: (Kontrakcja m, ToAPIResponse v) => m (APIResponse v) -> m Response
api = apiLog . apiRun
