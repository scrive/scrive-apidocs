{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.Monad.V2 (
                 APIError(),
                 serverError,
                 endPointNotFound,
                 invalidAuthorisation,
                 insufficientPrivileges,
                 requestParametersMissing,
                 requestParametersParseError,
                 requestParametersInvalid,
                 objectVersionMismatch,
                 documentStateError,
                 signatoryStateError,
                 documentAccessForbidden,
                 documentNotFound,
                 apiGuardJustM,
                 api,
                 APIResponse(..),
                 getAPIUser,
                 getAPIUserWithPrivileges,
                 getAPIUserWithAnyPrivileges,
                 getAPIUserWithPad,
                 noAPIV2CallFoundHandler
                 )
  where

import Control.Exception.Lifted
import Control.Monad.Base
import Happstack.Server (toResponse, askRq)
import Happstack.Server.Types
import Log as Log
import Text.JSON hiding (Ok)
import Text.JSON.Gen hiding (object)
import qualified Happstack.Server.Response as Web
import Data.Unjson
import Data.Text

import DB
import Kontra
import KontraPrelude
import OAuth.Model
import OAuth.Util
import Text.JSON.Convert
import User.Model
import Util.Actor
import Util.CSVUtil
import API.Monad.V2Errors
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


-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m, ToAPIResponse v) => m (APIResponse v) -> m Response
api acc =  (toAPIResponse <$> acc) `catches` [
    Handler $ \ex@(SomeKontraException e) -> do
      logAttention "API error (V2 will may convert error message):" $ object ["error" .= jsonToAeson (toJSValue e)]
      -- For some exceptions we do a conversion to APIError
      let ex' = tryToConvertConditionalExpectionIntoAPIError ex
      return $ (toAPIResponse $ jsonFromSomeKontraException $ tryToConvertConditionalExpectionIntoAPIError ex') {
        rsCode = httpCodeFromSomeKontraException $ tryToConvertConditionalExpectionIntoAPIError ex'
      }
  ]


apiGuardJustM :: (MonadBase IO m) => APIError -> m (Maybe a) -> m a
apiGuardJustM e a = a >>= maybe ((throwIO . SomeKontraException) e) return

-- get the user for the api; it can either be
--  1. OAuth using Authorization header
--  2. Session for Ajax client. ! Only if authorization header is empty !
getAPIUser :: Kontrakcja m => APIPrivilege -> m (User, Actor, Bool)
getAPIUser priv = getAPIUserWithPrivileges [priv]

-- Get the user for the API, as long as any privileges are there
getAPIUserWithAnyPrivileges :: Kontrakcja m => m (User, Actor, Bool)
getAPIUserWithAnyPrivileges = getAPIUserWithPrivileges [APIPersonal, APIDocCheck, APIDocSend, APIDocCreate]

-- Get the user for the API.
-- Either through:
-- 1. OAuth using the Authorization header
-- 2. Session for AJAX client (only if the Authorization is empty)
--
-- Only returns if *any* of the privileges in privs are issued.
getAPIUserWithPrivileges :: Kontrakcja m => [APIPrivilege] -> m (User, Actor, Bool)
getAPIUserWithPrivileges privs = do
  moauthuser <- getOAuthUser privs
  case moauthuser of
    Just (Left msg) -> (throwIO . SomeKontraException) $ invalidAuthorisation $ "You authorization is invalid:" `append` pack msg
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUser
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwIO . SomeKontraException) $ insufficientPrivileges "You need to authorize yourself to access this resource"

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> m (User, Actor, Bool)
getAPIUserWithPad priv = do
  moauthuser <- getOAuthUser [priv]
  case moauthuser of
    Just (Left msg) -> (throwIO . SomeKontraException) $ invalidAuthorisation $ "You authorization is invalid" `append` pack msg
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUserWithPad
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwIO . SomeKontraException) $ insufficientPrivileges "You need to authorize yourself to access this resource"


getSessionUser :: Kontrakcja m => m (Maybe (User, Actor))
getSessionUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor ctx user)

getSessionUserWithPad :: Kontrakcja m => m (Maybe (User, Actor))
getSessionUserWithPad = do
  ctx <- getContext
  case (ctxmaybeuser ctx `mplus` ctxmaybepaduser ctx) of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor ctx user)

getOAuthUser :: Kontrakcja m => [APIPrivilege] -> m (Maybe (Either String (User, Actor)))
getOAuthUser privs = do
  ctx <- getContext
  eauth <- getAuthorization
  case eauth of
    Nothing       -> return Nothing
    Just (Left l) -> return $ Just $ Left l
    Just (Right auth) -> do
      uap <- dbQuery $ GetUserIDForAPIWithPrivilege (oaAPIToken auth) (oaAPISecret auth) (oaAccessToken auth) (oaAccessSecret auth) privs
      case uap of
        Nothing -> return $ Just $ Left "OAuth credentials are invalid."
        Just (userid, apistring) -> do
          user <- apiGuardJustM (serverError "The User account for those credentials does not exist.") $ dbQuery $ GetUserByID userid
          let actor = apiActor ctx user apistring
          return $ Just $ Right (user, actor)

noAPIV2CallFoundHandler :: Kontrakcja m => m Response
noAPIV2CallFoundHandler = api $ do
  uri <- rqUri <$> askRq
  _ <- throwIO . SomeKontraException $ endPointNotFound $ "No API endpoint for " `append` (pack uri)
  return $ Ok ()
