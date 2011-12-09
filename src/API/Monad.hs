{-# LANGUAGE OverlappingInstances, IncoherentInstances, FunctionalDependencies #-}

module API.Monad where

import Control.Monad.Trans
import Happstack.Server.Types
import Text.JSON
import Happstack.Server.Response
--import Happstack.Server.Monads
--import Happstack.Server.RqData
import Control.Monad.Error
import Control.Applicative

import Util.JSON
import Misc
import Kontra
import DBError
import User.Model

data Created a = Created a
                    
data APIError = BadInput
              | NotLoggedIn
              | Forbidden
              | ActionNotAvailable
              | ServerError
              deriving (Show, Eq)
                     
instance Error APIError where
  noMsg = ServerError

class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id
  
instance ToAPIResponse JSValue where
  toAPIResponse jv = let r1 = toResponse $ encode jv in
    setHeader "Content-Type" "text/plain" r1 -- must be text/plain to allow browsers who want to save stuff to files
    
instance ToAPIResponse a => ToAPIResponse (Created a) where
  toAPIResponse (Created a) = (toAPIResponse a) { rsCode = 201 }
  
newtype APIMonad m a = AM { runAPIMonad :: ErrorT APIError m a }
                     deriving (MonadTrans, Monad, MonadError APIError, Functor, Applicative)
                              
instance KontraMonad m => KontraMonad (APIMonad m) where
  getContext = lift getContext
  modifyContext = lift . modifyContext
  
instance MonadIO m => MonadIO (APIMonad m) where
  liftIO = lift . liftIO
  
{-
instance ServerMonad m => ServerMonad (APIMonad m) where
  askRq = lift askRq
  localRq a b = AM $ lift $ localRq a b
  
instance (Monad m, HasRqData m) => HasRqData (APIMonad m) where
  askRqEnv = lift askRqEnv
  localRqEnv a b = AM $ lift $ localRqEnv a b
  rqDataError a = lift $ rqDataError a
  -}

--instance MonadTrans APIMonad where
--  lift = AM . (liftM )

jsonError :: Either String JSValue
jsonError = (Right jsempty) >>=
            jsset "status" "error"
            
-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m, ToAPIResponse v) => APIMonad m v -> m Response
api acc = do
  r <- runErrorT $ runAPIMonad acc
  case r of 
    Left BadInput ->
      badRequest (toAPIResponse $ fromRight $
                  jsonError >>=
                  jsset "message" "The input sent was invalid. Please try again.")
    Left Forbidden ->
      forbidden (toAPIResponse $ fromRight $ 
                 jsonError >>=
                 jsset "message" "The resource you are trying to access does not exist or you do not have permission to access it.")
    Left NotLoggedIn -> 
      unauthorized (toAPIResponse $ fromRight $
                    jsonError >>=
                    jsset "message" "You must identify yourself to access this resource." >>=
                    jsset "url" "http://scrive.com/api/user/login")
    Left ServerError ->
      internalServerError (toAPIResponse $ fromRight $
                           jsonError >>=
                           jsset "message" "We're sorry. The server just does not know what to do.")
    Left ActionNotAvailable ->
      forbidden (toAPIResponse $ fromRight $
                 jsonError >>=
                 jsset "message" "The action you requested is not available on this resource.")
    Right v -> return $ toAPIResponse v

apiErrorFromDBError :: DBError -> APIError
apiErrorFromDBError DBResourceNotAvailable     = Forbidden
apiErrorFromDBError DBNotLoggedIn              = NotLoggedIn
apiErrorFromDBError (DBDatabaseNotAvailable _) = ServerError
apiErrorFromDBError (DBActionNotAvailable _)   = ActionNotAvailable

--apiMonadFromAPIResponse :: Monad m => APIResponse a -> APIMonad m a
--apiMonadFromAPIResponse = AM . return

apiGuardL :: (Kontrakcja m, APIGuard m a b) => m a -> APIMonad m b
apiGuardL acc = do
  res <- lift acc
  apiGuard res

apiGuardL' :: (Kontrakcja m, APIGuard m a b) => APIError -> m a -> APIMonad m b
apiGuardL' e acc = do
  res <- lift acc
  apiGuard' e res

apiGuard :: (Monad m, APIGuard m a b) => a -> APIMonad m b
apiGuard a = guardEither a >>= either throwError return
    
apiGuard' :: (Monad m, APIGuard m a b) => APIError -> a -> APIMonad m b
apiGuard' e a = guardEither a >>= either (const $ throwError e) return

class Monad m => APIGuard m a b | a -> b where
  guardEither :: a -> APIMonad m (Either APIError b)
  
instance Monad m => APIGuard m (Either DBError b) b where
  guardEither (Left e) = return $ Left $ apiErrorFromDBError e
  guardEither (Right v) = return $ Right v
              
instance Monad m => APIGuard m (Maybe b) b where
  guardEither Nothing = return $ Left Forbidden
  guardEither (Just v) = return $ Right v

instance Monad m => APIGuard m (Either String b) b where
  guardEither (Left _) = return $ Left $ ServerError
  guardEither (Right v) = return $ Right v
  
instance Monad m => APIGuard m Bool () where
  guardEither False = return $ Left $ ServerError
  guardEither True  = return $ Right ()
  
instance (Monad m, APIGuard m a b) => APIGuard m (APIMonad m a) b where
  guardEither acc = acc >>= guardEither
  
--instance (Kontrakcja m, APIGuard m a b) => APIGuard m (m a) b where
--  guardEither acc = lift acc >>= guardEither
  
instance (Kontrakcja m, APIGuard m a b) => APIGuard m (m a) b where
  guardEither acc = lift acc >>= guardEither
  
instance (Monad m, JSON b) => APIGuard m (Result b) b where
  guardEither (Error _) = return $ Left BadInput
  guardEither (Ok v) = return $ Right v

getAPIUser :: Kontrakcja m => APIMonad m User
getAPIUser = do
  Context {ctxmaybeuser} <- getContext
  apiGuard' NotLoggedIn ctxmaybeuser
  
