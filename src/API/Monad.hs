module API.Monad where

import Control.Monad
import Control.Monad.Trans
import Happstack.Server.Types
import Text.JSON
import Happstack.Server.Response

import Util.JSON
import Misc
import Kontra
import DBError
import User.Model

data APIResponse a = Created a
                   | OK a
                   | BadInput
                   | NotLoggedIn
                   | Forbidden
                   | ActionNotAvailable
                   | ServerError
                     
class ToAPIResponse a where
  toAPIResponse :: a -> Response

instance ToAPIResponse Response where
  toAPIResponse = id
  
instance ToAPIResponse JSValue where
  toAPIResponse jv = let r1 = toResponse $ encode jv in
    setHeader "Content-Type" "text/plain" r1 -- must be text/plain to allow browsers who want to save stuff to files

instance Monad APIResponse where
  return = OK
  
  Created x          >>= f = f x
  OK x               >>= f = f x
  BadInput           >>= _ = BadInput 
  NotLoggedIn        >>= _ = NotLoggedIn
  Forbidden          >>= _ = Forbidden 
  ActionNotAvailable >>= _ = ActionNotAvailable 
  ServerError        >>= _ = ServerError 

  fail _ = ServerError
  
newtype (Monad m) => APIMonad m a = AM { runAPIMonad :: m (APIResponse a) }
  
instance Monad m => Monad (APIMonad m) where                                     
  return = AM . return . OK
  
  AM x >>= f = AM $ do
    x' <- x
    case x' of
      Created v          -> runAPIMonad $ f v
      OK v               -> runAPIMonad $ f v
      BadInput           -> return $ BadInput
      NotLoggedIn        -> return $ NotLoggedIn
      Forbidden          -> return $ Forbidden
      ActionNotAvailable -> return $ ActionNotAvailable
      ServerError        -> return $ ServerError
  
apiCreated :: Monad m => a -> APIMonad m a
apiCreated = AM . return . Created

apiOK :: Monad m => a -> APIMonad m a
apiOK = AM . return . OK

apiBadInput :: Monad m => APIMonad m a
apiBadInput = AM $ return BadInput

apiNotLoggedIn :: Monad m => APIMonad m a
apiNotLoggedIn = AM $ return NotLoggedIn

apiForbidden :: Monad m => APIMonad m a
apiForbidden = AM $ return Forbidden

apiActionNotAvailable :: Monad m => APIMonad m a
apiActionNotAvailable = AM $ return ActionNotAvailable

apiServerError :: Monad m => APIMonad m a
apiServerError = AM $ return ServerError

instance MonadTrans APIMonad where
  lift = AM . (liftM OK)

jsonError :: Either String JSValue
jsonError = (Right jsempty) >>=
            jsset "status" "error"
            
-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m) => APIMonad m JSValue -> m Response
api acc = do
  r <- runAPIMonad acc
  case r of 
    BadInput ->
      badRequest (toAPIResponse $ fromRight $
                  jsonError >>=
                  jsset "message" "The input sent was invalid. Please try again.")
    Forbidden ->
      forbidden (toAPIResponse $ fromRight $ 
                 jsonError >>=
                 jsset "message" "The resource you are trying to access does not exist or you do not have permission to access it.")
    NotLoggedIn -> 
      unauthorized (toAPIResponse $ fromRight $
                    jsonError >>=
                    jsset "message" "You must identify yourself to access this resource." >>=
                    jsset "url" "http://scrive.com/api/user/login")
    ServerError ->
      internalServerError (toAPIResponse $ fromRight $
                           jsonError >>=
                           jsset "message" "We're sorry. The server just does not know what to do.")
    ActionNotAvailable ->
      forbidden (toAPIResponse $ fromRight $
                 jsonError >>=
                 jsset "message" "The action you requested is not available on this resource.")
    OK v -> ok $ toAPIResponse v
    Created v -> 
      (ok $ toAPIResponse v) >>=
      setRsCode 201

apiResponseFromDBError :: DBError -> APIResponse a
apiResponseFromDBError DBResourceNotAvailable = Forbidden
apiResponseFromDBError DBNotLoggedIn = NotLoggedIn
apiResponseFromDBError (DBDatabaseNotAvailable _) = ServerError
apiResponseFromDBError (DBActionNotAvailable _) = ActionNotAvailable

apiMonadFromAPIResponse :: Monad m => APIResponse a -> APIMonad m a
apiMonadFromAPIResponse = AM . return

class Monad m => APIGuard m b a where
  apiGuard  :: a   -> APIMonad m b
  apiGuardL :: m a -> APIMonad m b
  apiGuardL acc = lift acc >>= apiGuard
  
instance Monad m => APIGuard m b (Either DBError b)  where
  apiGuard (Left e)  = AM $ return $ apiResponseFromDBError e
  apiGuard (Right v) = return v
              
instance Monad m => APIGuard m b (Maybe b) where
  apiGuard Nothing = apiForbidden
  apiGuard (Just x) = return x

instance Monad m => APIGuard m b (Either String b) where
  apiGuard (Left _) = apiServerError
  apiGuard (Right v) = return v

getAPIUser :: Kontrakcja m => APIMonad m User
getAPIUser = (maybe apiNotLoggedIn apiOK . ctxmaybeuser) =<< (lift getContext)
