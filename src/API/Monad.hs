{-# LANGUAGE FunctionalDependencies, ExtendedDefaultRules #-}
module API.Monad (
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
                 APIMonad(..),
                 getAPIUser,
                 getAPIUserWithPad,
                 FormEncoded(..)
                 )
  where

import Control.Monad.Trans
import Happstack.Server (toResponse)
import Happstack.Server.Types
import Text.JSON hiding (Ok)
import Text.JSON.Gen
import qualified Happstack.Server.Response as Web
import Control.Monad.Error
import Control.Applicative
import Network.HTTP (urlEncodeVars)

import Crypto.RNG
import DB
import Kontra
import User.Model
import EvidenceLog.Model
import Util.HasSomeUserInfo
import DB.SQL2

import OAuth.Util
import OAuth.Model
import Doc.Rendering
import Util.Actor
import Text.StringTemplates.Templates
import Util.CSVUtil
import Util.ZipUtil
import Control.Exception.Lifted
import Control.Monad.Base
import Data.Typeable
import qualified Log as Log
import qualified Amazon as AWS

-- | Respond with a 200 Created status
data Ok a = Ok a

-- | Respond with a 201 Created status
data Created a = Created a

-- | Respond with a 202 Accepted status
data Accepted a = Accepted a

-- | Values to be form encoded
data FormEncoded = FormEncoded [(String, String)]

data APIError = BadInput           String
              | NotLoggedIn        String
              | Forbidden          String -- | also used for not found, since we don't want to reveal the non-existence of resources
              | ActionNotAvailable String
              | ServerError        String
              | NoAvailableYet     String
              | ConflictError      String
              deriving (Show, Eq, Typeable)

instance KontraException APIError

instance ToJSValue APIError where
  toJSValue (BadInput msg) = jsonError $ value "message" msg
  toJSValue (Forbidden msg) = jsonError $ value "message" msg
  toJSValue (NotLoggedIn msg) = jsonError $ do
                                  value "message" msg
                                  value "url" "https://scrive.com/login"
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

httpCodeFromSomeKontraException :: SomeKontraException -> Int
httpCodeFromSomeKontraException (SomeKontraException ex) =
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

instance ToAPIResponse () where
  toAPIResponse () = toResponse ""

instance ToAPIResponse FormEncoded where
  toAPIResponse (FormEncoded kvs) =
    let r1 = Web.toResponse $ urlEncodeVars kvs
    in setHeader "Content-Type" "application/x-www-form-urlencoded" r1

newtype APIMonad m a = AM { runAPIMonad :: m a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadIO, TemplatesMonad, Log.MonadLog, AWS.AmazonMonad)

instance MonadTrans APIMonad where
  lift = AM

deriving instance (MonadBase IO m) => MonadBase IO (APIMonad m)
deriving instance (MonadDB m) => MonadDB (APIMonad m)

instance KontraMonad m => KontraMonad (APIMonad m) where
  getContext = lift getContext
  modifyContext = lift . modifyContext

jsonError :: JSONGen () -> JSValue
jsonError rest = runJSONGen $ do
  value "status" "error"
  rest



-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m, ToAPIResponse v) => APIMonad m v -> m Response
api acc = (toAPIResponse <$> runAPIMonad acc)
          `catches` [ Handler $ \ex@(SomeKontraException e) ->
                        return $ ((toAPIResponse $ toJSValue e) { rsCode = httpCodeFromSomeKontraException ex })
                    ]


apiGuardL' :: (Kontrakcja m, APIGuard m a b) => m a -> APIMonad m b
apiGuardL' acc = apiGuard' =<< lift acc

apiGuardL :: (Kontrakcja m, APIGuard m a b) => APIError -> m a -> APIMonad m b
apiGuardL e acc = apiGuard e =<< lift acc

apiGuard' :: (MonadBase IO m, APIGuard m a b) => a -> APIMonad m b
apiGuard' a = guardEither a >>= either (throwIO . SomeKontraException) return

apiGuard :: (MonadBase IO m, APIGuard m a b) => APIError -> a -> APIMonad m b
apiGuard e a = guardEither a >>= either (const $ (throwIO . SomeKontraException) e) return

apiGuardJustM :: (MonadBase IO m) => APIError -> APIMonad m (Maybe a) -> APIMonad m a
apiGuardJustM e a = a >>= maybe ((throwIO . SomeKontraException) e) return


-- | Unify the different types of guards with this class
class MonadBase IO m => APIGuard m a b | a -> b where
  guardEither :: a -> APIMonad m (Either APIError b)

instance MonadBase IO m => APIGuard m (Maybe b) b where
  guardEither Nothing = return $ Left $ forbidden "The resource you are trying to access does not exist or you do not have permission to access it."
  guardEither (Just v) = return $ Right v

instance MonadBase IO m => APIGuard m (Either String b) b where
  guardEither (Left s) = return $ Left $ serverError s
  guardEither (Right v) = return $ Right v

instance (MonadBase IO m) => APIGuard m (Either FileError b) b where
  guardEither (Left _) = return $ Left $ serverError'
  guardEither (Right v) = return $ Right v

instance MonadBase IO m => APIGuard m Bool () where
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
getAPIUser :: Kontrakcja m => APIPrivilege -> APIMonad m (User, Actor, Bool)
getAPIUser priv = do
  moauthuser <- getOAuthUser priv
  case moauthuser of
    Just (Left err) -> (throwIO . SomeKontraException) $ notLoggedIn err
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUser
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwIO . SomeKontraException) notLoggedIn'

getAPIUserWithPad :: Kontrakcja m => APIPrivilege -> APIMonad m (User, Actor, Bool)
getAPIUserWithPad priv = do
  moauthuser <- getOAuthUser priv
  case moauthuser of
    Just (Left err) -> (throwIO . SomeKontraException) $ notLoggedIn err
    Just (Right (user, actor)) -> return (user, actor, True)
    Nothing -> do
      msessionuser <- getSessionUserWithPad
      case msessionuser of
        Just (user, actor) -> return (user, actor, False)
        Nothing -> (throwIO . SomeKontraException) notLoggedIn'


getSessionUser :: Kontrakcja m => APIMonad m (Maybe (User, Actor))
getSessionUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor (ctxtime ctx) (ctxipnumber ctx) user)

getSessionUserWithPad :: Kontrakcja m => APIMonad m (Maybe (User, Actor))
getSessionUserWithPad = do
  ctx <- getContext
  case (ctxmaybeuser ctx `mplus` ctxmaybepaduser ctx) of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor (ctxtime ctx) (ctxipnumber ctx) user)

getOAuthUser :: Kontrakcja m => APIPrivilege -> APIMonad m (Maybe (Either String (User, Actor)))
getOAuthUser priv = do
  ctx <- getContext
  eauth <- lift $ getAuthorization
  case eauth of
    Nothing       -> return Nothing
    Just (Left l) -> return $ Just $ Left l
    Just (Right auth) -> do
      uap <- dbQuery $ GetUserIDForAPIWithPrivilege (oaAPIToken auth) (oaAPISecret auth) (oaAccessToken auth) (oaAccessSecret auth) priv
      case uap of
        Nothing -> return $ Just $ Left "OAuth credentials are invalid."
        Just (userid, apistring) -> do
          user <- apiGuardL (serverError "The User account for those credentials does not exist.") $ dbQuery $ GetUserByID userid
          let actor = apiActor (ctxtime ctx) (ctxipnumber ctx) userid (getEmail user) apistring
          return $ Just $ Right (user, actor)
