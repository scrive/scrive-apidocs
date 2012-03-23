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
                 APIMonad(),
                 getAPIUser,
                 FormEncoded(..)
                 
                 )


  where

import Control.Monad.Trans
import Happstack.Server.Types
import Text.JSON
import qualified Happstack.Server.Response as Web
import Control.Monad.Error
import Control.Applicative
import Network.HTTP (urlEncodeVars)
import qualified Data.Text as T

import Util.JSON
import Misc
import Kontra
import DBError
import User.Model
import EvidenceLog.Model
import qualified Data.ByteString.UTF8 as BS hiding (length)
import Util.HasSomeUserInfo

import DB.Classes
import OAuth.Model
import Doc.DocStorage
import qualified Data.Map as Map
--import Happstack.Server.RqData
import Happstack.Server.Monads

import Data.Maybe
import OAuth.Parse

-- | Respond with a 201 Created status
data Created a = Created a

-- | Values to be form encoded
data FormEncoded = FormEncoded [(String, String)]
                   
data APIError = BadInput           T.Text
              | NotLoggedIn        T.Text
              | Forbidden          T.Text -- | also used for not found, since we don't want to reveal the non-existence of resources
              | ActionNotAvailable T.Text
              | ServerError        T.Text
              deriving (Show, Eq)

badInput :: String -> APIError
badInput = BadInput . T.pack

badInput' :: APIError
badInput' = badInput "The input sent was invalid. Please try again."

notLoggedIn :: String -> APIError
notLoggedIn = NotLoggedIn . T.pack

notLoggedIn' :: APIError
notLoggedIn' = notLoggedIn "You must identify yourself to access this resource."

forbidden :: String -> APIError
forbidden = Forbidden . T.pack

forbidden' :: APIError
forbidden' = forbidden "The resource you are trying to access does not exist or you do not have permission to access it."

actionNotAvailable :: String -> APIError
actionNotAvailable = ActionNotAvailable . T.pack

actionNotAvailable' :: APIError
actionNotAvailable' = actionNotAvailable "The action you requested is not available on this resource."

serverError :: String -> APIError
serverError = ServerError . T.pack

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

instance ToAPIResponse () where
  toAPIResponse _ = Web.toResponse ("" :: String)
    
instance ToAPIResponse a => ToAPIResponse (Created a) where
  toAPIResponse (Created a) = (toAPIResponse a) { rsCode = 201 }

instance ToAPIResponse FormEncoded where
  toAPIResponse (FormEncoded kvs) = 
    let r1 = Web.toResponse $ urlEncodeVars kvs
    in setHeader "Content-Type" "application/x-www-form-urlencoded" r1

newtype APIMonad m a = AM { runAPIMonad :: ErrorT APIError m a }
                     deriving (MonadTrans, Monad, MonadError APIError, Functor, Applicative, MonadIO)
                              
instance KontraMonad m => KontraMonad (APIMonad m) where
  getContext = lift getContext
  modifyContext = lift . modifyContext

jsonError :: JSValue
jsonError = fromRight $ jsset ("status" :: String) ("error" :: String) jsempty

{-
"The input sent was invalid. Please try again." 
"The resource you are trying to access does not exist or you do not have permission to access it."
"You must identify yourself to access this resource."
"We're sorry. The server just does not know what to do."
"The action you requested is not available on this resource."
-}
            
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

getAPIUser :: Kontrakcja m => APIMonad m (User, Either AuthorActor APIActor)
getAPIUser = do
  moauthuser <- getOAuthUser
  case moauthuser of
    Just (user, actor) -> return (user, Right actor)
    Nothing -> do
      msessionuser <- getSessionUser
      case msessionuser of
        Just (user, actor) -> return (user, Left actor)
        Nothing -> throwError notLoggedIn'

getSessionUser :: Kontrakcja m => APIMonad m (Maybe (User, AuthorActor))
getSessionUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing -> return Nothing
    Just user -> return $ Just (user, AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (BS.toString $ getEmail user))

getOAuthUser :: Kontrakcja m => APIMonad m (Maybe (User, APIActor))
getOAuthUser = do
  rq <- lift $ askRq
  ctx <- getContext
  case Map.lookup (BS.fromString "authorization") $ rqHeaders rq of
    Nothing -> return Nothing
    Just (HeaderPair _ auths) -> do

      auth <- apiGuard (notLoggedIn "Missing Authorization headers.") $ BS.toString <$> listToMaybe auths

      -- pull the data out of Authorization
      let params = splitAuthorization auth

      sigtype <- apiGuard (badInput "Authorization header must contain oauth_signature_method.") $ maybeRead =<< lookup "oauth_signature_method" params
      when (sigtype /= "PLAINTEXT") $ throwError $ badInput "oauth_signatory_method must be PLAINTEXT."

      (mapisecret, mtokensecret) <- apiGuard (badInput "Authorization header must contain a valid oauth_signature.") $ splitSignature =<< maybeRead =<< lookup "oauth_signature" params
      apisecret <- apiGuard (badInput "The API Secret is invalid format.") mapisecret
      tokensecret <- apiGuard (badInput "The Token Secret is invalid format.") mtokensecret
     
      apitoken    <- apiGuard (badInput "The Authorization header must contain oauth_consumer_key.") $ maybeRead =<< maybeRead =<< lookup "oauth_consumer_key" params
      accesstoken <- apiGuard (badInput "The Authorization header must contain oauth_token.") $ maybeRead =<< maybeRead =<< lookup "oauth_token" params

      (userid, apistring) <- apiGuardL (forbidden "OAuth credentials are invalid.") $ runDBQuery $ GetUserIDForAPIWithPrivilege apitoken apisecret accesstoken tokensecret APIDocCreate
  
      user <- apiGuardL (serverError "The User account for those credentials does not exist.") $ runDBQuery $ GetUserByID userid

      let actor = APIActor (ctxtime ctx) (ctxipnumber ctx) userid (BS.toString $ getEmail user) apistring
      return $ Just (user, actor)
