{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.Monad.V1 (
                 APIError(),
                 badInput,
                 badInput',
                 notLoggedIn,
                 notLoggedIn',
                 forbidden,
                 forbidden',
                 actionNotAvailable,
                 actionNotAvailable',
                 serverError,
                 serverError',
                 noAvailableYet,
                 noAvailableYet',
                 conflictError,
                 conflictError',
                 apiGuard,
                 apiGuard',
                 apiGuardL,
                 apiGuardL',
                 apiGuardJustM,
                 api,
                 Ok(..),
                 Created(..),
                 Accepted(..),
                 Failed(..),
                 getAPIUser,
                 getAPIUserWithPrivileges,
                 getAPIUserWithAnyPrivileges,
                 getAPIUserWithPad,
                 )
  where

import Control.Monad.Catch
import Data.Typeable
import Data.Unjson
import Happstack.Server (toResponse)
import Happstack.Server.Types
import Log as Log
import Text.JSON hiding (Ok)
import Text.JSON.Gen hiding (object)
import qualified Happstack.Server.Response as Web

import DB
import Doc.Rendering
import Kontra
import KontraPrelude
import OAuth.Model
import OAuth.Util
import Text.JSON.Convert
import User.Model
import Util.Actor
import Util.CSVUtil
import Util.ZipUtil

-- | Respond with a 200 Created status
data Ok a = Ok a

-- | Respond with a 201 Created status
data Created a = Created a

-- | Respond with a 202 Accepted status
data Accepted a = Accepted a

-- | Respond with a 400 Bad Input status. Use it when you need to mark that request failed, but you don't want to rollback
data Failed a = Failed a

data APIError = BadInput           String
              | NotLoggedIn        String
              | Forbidden          String -- | also used for not found, since we don't want to reveal the non-existence of resources
              | ActionNotAvailable String
              | ServerError        String
              | NoAvailableYet     String
              | ConflictError      String
              deriving (Show, Eq, Typeable)

instance DBExtraException APIError

instance ToJSValue APIError where
  toJSValue (BadInput msg) = jsonError $ value "message" msg
  toJSValue (Forbidden msg) = jsonError $ value "message" msg
  toJSValue (NotLoggedIn msg) = jsonError $ do
                                  value "message" msg
                                  value "url" ("https://scrive.com/login"::String)
  toJSValue (ServerError msg) = jsonError $ value "message" msg
  toJSValue (ActionNotAvailable msg) = jsonError $ value "message" msg
  toJSValue (NoAvailableYet msg) = jsonError $ value "message" msg
  toJSValue (ConflictError msg) = jsonError $ value "message" msg


httpCodeFromAPIError :: APIError -> Int
httpCodeFromAPIError (BadInput {}) = 400
httpCodeFromAPIError (Forbidden {}) = 403
httpCodeFromAPIError (NotLoggedIn {}) = 403
httpCodeFromAPIError (ServerError {}) = 500
httpCodeFromAPIError (ActionNotAvailable {}) = 500
httpCodeFromAPIError (NoAvailableYet {}) = 420
httpCodeFromAPIError (ConflictError {}) = 409

httpCodeFromSomeDBExtraException :: SomeDBExtraException -> Int
httpCodeFromSomeDBExtraException (SomeDBExtraException ex) =
  case cast ex of
    Just (apierror :: APIError) -> httpCodeFromAPIError apierror
    Nothing -> 400

badInput :: String -> APIError
badInput = BadInput

badInput' :: APIError
badInput' = badInput "The input sent was invalid. Please try again."

notLoggedIn :: String -> APIError
notLoggedIn = NotLoggedIn

notLoggedIn' :: APIError
notLoggedIn' = notLoggedIn "You must identify yourself to access this resource."

forbidden :: String -> APIError
forbidden = Forbidden

forbidden' :: APIError
forbidden' = forbidden "The resource you are trying to access does not exist or you do not have permission to access it."

actionNotAvailable :: String -> APIError
actionNotAvailable = ActionNotAvailable

actionNotAvailable' :: APIError
actionNotAvailable' = actionNotAvailable "The action you requested is not available on this resource."

serverError :: String -> APIError
serverError = ServerError

serverError' :: APIError
serverError' = serverError "An internal server error occurred which could not be resolved."


noAvailableYet:: String -> APIError
noAvailableYet = NoAvailableYet

noAvailableYet':: APIError
noAvailableYet' = noAvailableYet "Resource is not yet available"

conflictError :: String -> APIError
conflictError = ConflictError

conflictError' :: APIError
conflictError' = conflictError "An internal server error occurred which could not be resolved."

-- Define what we can respond from an API call
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id

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

instance ToAPIResponse ZipArchive where
  toAPIResponse v = let r1 = Web.toResponse $ v in
    setHeader "Content-Type" "text/zip" r1

instance (ToAPIResponse a, ToAPIResponse b) => ToAPIResponse (Either a b) where
  toAPIResponse = either toAPIResponse toAPIResponse

instance ToAPIResponse a => ToAPIResponse (Ok a) where
  toAPIResponse (Ok a) = (toAPIResponse a) { rsCode = 200 }

instance ToAPIResponse a => ToAPIResponse (Created a) where
  toAPIResponse (Created a) = (toAPIResponse a) { rsCode = 201 }

instance ToAPIResponse a => ToAPIResponse (Accepted a) where
  toAPIResponse (Accepted a) = (toAPIResponse a) { rsCode = 202 }

instance ToAPIResponse a => ToAPIResponse (Failed a) where
  toAPIResponse (Failed a) = (toAPIResponse a) { rsCode = 400 }

instance ToAPIResponse () where
  toAPIResponse () = toResponse ""

jsonError :: JSONGen () -> JSValue
jsonError rest = runJSONGen $ do
  value "status" ("error"::String)
  rest



-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m, ToAPIResponse v) => m v -> m Response
api acc = (toAPIResponse <$> acc) `catches` [
    Handler $ \ex@(SomeDBExtraException e) -> do
      -- API handler always returns a valid response. Due to that appHandler will not rollback - and we need to do it here
      rollback
      logAttention "API error" $ object [
          "error" .= jsonToAeson (toJSValue e)
        ]
      return $ (toAPIResponse $ toJSValue e) {
        rsCode = httpCodeFromSomeDBExtraException ex
      }
  ]


apiGuardL' :: (Kontrakcja m, APIGuard m a b) => m a -> m b
apiGuardL' acc = apiGuard' =<< acc

apiGuardL :: (Kontrakcja m, APIGuard m a b) => APIError -> m a -> m b
apiGuardL e acc = apiGuard e =<< acc

apiGuard' :: (MonadThrow m, APIGuard m a b) => a -> m b
apiGuard' a = guardEither a >>= either (throwM . SomeDBExtraException) return

apiGuard :: (MonadThrow m, APIGuard m a b) => APIError -> a -> m b
apiGuard e a = guardEither a >>= either (const $ (throwM . SomeDBExtraException) e) return

apiGuardJustM :: (MonadThrow m) => APIError -> m (Maybe a) -> m a
apiGuardJustM e a = a >>= maybe ((throwM . SomeDBExtraException) e) return


-- | Unify the different types of guards with this class
class MonadThrow m => APIGuard m a b | a -> b where
  guardEither :: a -> m (Either APIError b)

instance MonadThrow m => APIGuard m (Maybe b) b where
  guardEither Nothing = return $ Left $ forbidden "The resource you are trying to access does not exist or you do not have permission to access it."
  guardEither (Just v) = return $ Right v

instance MonadThrow m => APIGuard m (Either String b) b where
  guardEither (Left s) = return $ Left $ serverError s
  guardEither (Right v) = return $ Right v

instance (MonadThrow m) => APIGuard m (Either FileError b) b where
  guardEither (Left _) = return $ Left $ serverError'
  guardEither (Right v) = return $ Right v

instance MonadThrow m => APIGuard m Bool () where
  guardEither False = return $ Left $ serverError'
  guardEither True  = return $ Right ()

{-
instance (Monad m, JSON b) => APIGuard m (Result b) b where
  guardEither (Error _) = return $ Left $ BadInput
  guardEither (Ok v) = return $ Right v
-}

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
    Just (Left err) -> (throwM . SomeDBExtraException) $ notLoggedIn err
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUser
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwM . SomeDBExtraException) notLoggedIn'

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> m (User, Actor, Bool)
getAPIUserWithPad priv = do
  moauthuser <- getOAuthUser [priv]
  case moauthuser of
    Just (Left err) -> (throwM . SomeDBExtraException) $ notLoggedIn err
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUserWithPad
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwM . SomeDBExtraException) notLoggedIn'


getSessionUser :: Kontrakcja m => m (Maybe (User, Actor))
getSessionUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor ctx user)

getSessionUserWithPad :: Kontrakcja m => m (Maybe (User, Actor))
getSessionUserWithPad = do
  ctx <- getContext
  case getContextUser ctx of
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
          user <- apiGuardL (serverError "The User account for those credentials does not exist.") $ dbQuery $ GetUserByID userid
          let actor = apiActor ctx user apistring
          return $ Just $ Right (user, actor)
