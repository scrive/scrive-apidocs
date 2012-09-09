{-# LANGUAGE FunctionalDependencies #-}
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
                 apiGuard,
                 apiGuard',
                 apiGuardL,
                 apiGuardL',
                 api,
                 Created(..),
                 APIMonad(..),
                 getAPIUser,
                 FormEncoded(..)
                 
                 )


  where

import Control.Monad.Trans
import Happstack.Server (toResponse)
import Happstack.Server.Types
import Text.JSON
import qualified Happstack.Server.Response as Web
import Control.Monad.Error
import Control.Applicative
import Network.HTTP (urlEncodeVars)

import Crypto.RNG
import DB
import Util.JSON
import Utils.Either
import Kontra
import DBError
import User.Model
import EvidenceLog.Model
import Util.HasSomeUserInfo

import OAuth.Util
import OAuth.Model
import Doc.Rendering
import Util.Actor

import qualified Log

-- | Respond with a 201 Created status
data Created a = Created a

-- | Values to be form encoded
data FormEncoded = FormEncoded [(String, String)]
                   
data APIError = BadInput           String
              | NotLoggedIn        String
              | Forbidden          String -- | also used for not found, since we don't want to reveal the non-existence of resources
              | ActionNotAvailable String
              | ServerError        String
              deriving (Show, Eq)

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

instance Error APIError where
  noMsg  = serverError'
  strMsg = serverError

-- Define what we can respond from an API call
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id

instance ToAPIResponse JSValue where
  toAPIResponse jv = let r1 = Web.toResponse $ encode jv in
    setHeader "Content-Type" "text/plain" r1 -- must be text/plain because some browsers complain about JSON type

instance ToAPIResponse a => ToAPIResponse (Created a) where
  toAPIResponse (Created a) = (toAPIResponse a) { rsCode = 201 }
  
instance ToAPIResponse () where
  toAPIResponse () = toResponse ""

instance ToAPIResponse FormEncoded where
  toAPIResponse (FormEncoded kvs) = 
    let r1 = Web.toResponse $ urlEncodeVars kvs  
    in setHeader "Content-Type" "application/x-www-form-urlencoded" r1
    
newtype APIMonad m a = AM { runAPIMonad :: ErrorT APIError m a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadDB, MonadError APIError, MonadIO, MonadTrans)

instance KontraMonad m => KontraMonad (APIMonad m) where
  getContext = lift getContext
  modifyContext = lift . modifyContext

jsonError :: JSValue
jsonError = fromRight $ jsset ("status" :: String) ("error" :: String) jsempty



-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m, ToAPIResponse v) => APIMonad m v -> m Response
api acc = apiToResponse =<< runErrorT (runAPIMonad acc)

apiToResponse :: (ToAPIResponse a, Kontrakcja m) => Either APIError a -> m Response
apiToResponse r =
  case r of 
    Left (BadInput msg) ->
      Web.badRequest (toAPIResponse $ fromRight $ jsset "message" msg jsonError)
    Left (Forbidden msg) ->
      Web.forbidden (toAPIResponse $ fromRight $ jsset "message" msg jsonError)
    Left (NotLoggedIn msg) -> 
      Web.unauthorized (toAPIResponse $ fromRight $ 
                    jsset "message" msg jsonError >>=
                    jsset "url" "https://scrive.com/gb/en?logging")
    Left (ServerError msg) ->
      Web.internalServerError (toAPIResponse $ fromRight $ jsset "message" msg jsonError)
    Left (ActionNotAvailable msg) ->
      Web.resp 405 (toAPIResponse $ fromRight $ jsset "message" msg jsonError)
    Right v -> return $ toAPIResponse v

apiErrorFromDBError :: DBError -> APIError
apiErrorFromDBError DBResourceNotAvailable     = forbidden "The resource you are trying to access does not exist or you do not have permission to access it."
apiErrorFromDBError DBNotLoggedIn              = notLoggedIn "You must identify yourself to access this resource."
apiErrorFromDBError (DBDatabaseNotAvailable x) = serverError x
apiErrorFromDBError (DBActionNotAvailable x)   = actionNotAvailable x

apiGuardL' :: (Kontrakcja m, APIGuard m a b) => m a -> APIMonad m b
apiGuardL' acc = apiGuard' =<< lift acc

apiGuardL :: (Kontrakcja m, APIGuard m a b) => APIError -> m a -> APIMonad m b
apiGuardL e acc = apiGuard e =<< lift acc

apiGuard' :: (Monad m, APIGuard m a b) => a -> APIMonad m b
apiGuard' a = guardEither a >>= either throwError return
    
apiGuard :: (Monad m, APIGuard m a b) => APIError -> a -> APIMonad m b
apiGuard e a = guardEither a >>= either (const $ throwError e) return

-- | Unify the different types of guards with this class
class Monad m => APIGuard m a b | a -> b where
  guardEither :: a -> APIMonad m (Either APIError b)
  
instance Monad m => APIGuard m (Either DBError b) b where
  guardEither (Left e) = return $ Left $ apiErrorFromDBError e
  guardEither (Right v) = return $ Right v
              
instance Monad m => APIGuard m (Maybe b) b where
  guardEither Nothing = return $ Left $ forbidden "The resource you are trying to access does not exist or you do not have permission to access it."
  guardEither (Just v) = return $ Right v

instance Monad m => APIGuard m (Either String b) b where
  guardEither (Left s) = return $ Left $ serverError s
  guardEither (Right v) = return $ Right v

instance (Monad m) => APIGuard m (Either FileError b) b where
  guardEither (Left _) = return $ Left $ serverError'
  guardEither (Right v) = return $ Right v
  
instance Monad m => APIGuard m Bool () where
  guardEither False = return $ Left $ serverError'
  guardEither True  = return $ Right ()
  
{-
instance (Monad m, JSON b) => APIGuard m (Result b) b where
  guardEither (Error _) = return $ Left $ BadInput
  guardEither (Ok v) = return $ Right v
-}
  
-- get the user for the api; it can either be 
--  1. OAuth using Authorization header
--  2. Session for Ajax client
getAPIUser :: Kontrakcja m => APIMonad m (User, Actor)
getAPIUser = do
  moauthuser <- getOAuthUser
  Log.debug $ "moauthuser: " ++ show moauthuser
  case moauthuser of
    Just (user, actor) -> return (user, actor)
    Nothing -> do
      msessionuser <- getSessionUser
      Log.debug $ "msessionuser: " ++ show msessionuser
      case msessionuser of
        Just (user, actor) -> return (user, actor)
        Nothing -> throwError notLoggedIn'

getSessionUser :: Kontrakcja m => APIMonad m (Maybe (User, Actor))
getSessionUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing -> return Nothing
    Just user -> return $ Just (user, authorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (getEmail user))

getOAuthUser :: Kontrakcja m => APIMonad m (Maybe (User, Actor))
getOAuthUser = do
  Log.debug "getOAuthUser start"
  ctx <- getContext
  eauth <- lift $ getAuthorization
  case eauth of
    Left _ -> return Nothing
    Right auth -> do
      (userid, apistring) <- apiGuardL (forbidden "OAuth credentials are invalid.") $ 
                              dbQuery $ GetUserIDForAPIWithPrivilege (oaAPIToken auth) (oaAPISecret auth) (oaAccessToken auth) (oaAccessSecret auth) APIDocCreate
  
      user <- apiGuardL (serverError "The User account for those credentials does not exist.") $ dbQuery $ GetUserByID userid

      let actor = apiActor (ctxtime ctx) (ctxipnumber ctx) userid (getEmail user) apistring
      return $ Just (user, actor)
