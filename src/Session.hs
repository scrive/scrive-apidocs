{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Session 
    ( Sessions
    , Session 
    , getFlashMessagesFromSession
    , getUserFromSession
    , handleSession
    , updateSessionWithContextData
    )
    where

import Control.Monad.Reader (MonadPlus(..), ap, ReaderT(..), asks, ask)
import Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data (Default, deriveAll, gFind')
import Happstack.Data.IxSet
import Happstack.Data.IxSet (IxSet(..), Indexable(..), (@=), delete, getOne, 
                             inferIxSet, noCalcs, updateIx)
import Happstack.Server (ServerMonad, withDataFn, readCookieValue,addCookie,FilterMonad(..),
                         Response,expireCookie,setHeaderM,withData,RqData)
import Happstack.Server.Cookie (Cookie,mkCookie,mkCookieHeader)
import Happstack.Server.HTTP.Types ()
import Happstack.State (Serialize, Version, Query, Update, deriveSerialize, getRandomR, 
                        mkMethods, query, update, mode, extension, Proxy(Proxy), Migrate, migrate)
import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import UserState (UserID,FlashMessage,GetUserByUserID(GetUserByUserID), User)
import MinutesTime
import Happstack.Server hiding (simpleHTTP)

$( deriveAll [''Ord, ''Eq, ''Default, ''Num]
   [d|
       newtype SessionId = SessionId Integer
    |])

instance Show SessionId where
    showsPrec prec (SessionId v) = showsPrec prec v

instance Read SessionId where
    readsPrec prec string = 
        [(SessionId k,v) | (k,v) <- readsPrec prec string]

$(deriveSerialize ''SessionId)
instance Version SessionId   
  
  
data SessionData0 = SessionData0 { userID0::Maybe UserID,
                                   flashMessages0::[FlashMessage]
                                 } deriving (Ord,Eq,Show,Typeable,Data)
$(deriveSerialize ''SessionData0)
instance Version (SessionData0)
                                   
data SessionData = SessionData {  
                                  userID::Maybe UserID,
                                  flashMessages::[FlashMessage],
                                  expires::MinutesTime
                               }  deriving (Ord,Eq,Show,Typeable,Data)
$(deriveSerialize ''SessionData) 
instance Version (SessionData) where
   mode = extension 1 (Proxy :: Proxy SessionData0)
     
instance Migrate SessionData0 SessionData where
      migrate _ = SessionData {userID = Nothing, flashMessages = [], expires = MinutesTime 0}
    
   

data Session = Session {sessionId::SessionId,
                        sessionData::SessionData}
               deriving (Ord, Eq, Show, Data, Typeable)

$(deriveSerialize ''Session)
instance Version (Session)
   
$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

-- Some helpers. MACID demands it before use.
-- |perform insert only if test is True
testAndInsert test a =
    maybeModify $ \ixset ->
        if test ixset
          then Just (insert a ixset)
          else Nothing

-- this should be sent upstream to mtl
maybeModify :: (MonadState s m) => (s -> Maybe s) -> m Bool
maybeModify f =
    do state <- get
       case f state of
         Nothing -> return False
         (Just state') -> 
             do put state' 
                return True

-- |get the session data associated with the supplied SessionId
getSession :: SessionId -> Query Sessions (Maybe (Session))
getSession sessionId = (return . getOne . (@= (sessionId :: SessionId))) =<< ask

-- |update the Session
updateSession :: (Session) -> Update (Sessions) ()
updateSession session = modify (updateIx (gFind' session :: SessionId) session)

-- |delete the session associated with the sessionId
-- returns the deleted session (mostly because we need some place to disambiguate the session type)
delSession :: SessionId -> Update Sessions (Maybe (Session))
delSession sessionId =
    do mSession <- (return . getOne . (@= (sessionId :: SessionId))) =<< get
       case mSession of
         Nothing -> return Nothing
         (Just session) -> 
             do modify (delete session)
                return (Just session)

-- |start a new session with the supplied session data
-- returns: the SessionId
newSession :: SessionData -> Update Sessions Session
newSession sessData =
    do sessId <- fmap SessionId $ getRandomR (0,1000000000)
       let session = (Session sessId sessData)
       r <- testAndInsert (isNothing . getOne . (@= sessId)) session
       if r
          then return (Session sessId sessData)
          else newSession sessData

-- * methods

$(mkMethods ''Sessions 
  [ 'getSession
  , 'updateSession
  , 'delSession
  , 'newSession
  ])

  
--- Session interface  
  
startSessionCookie :: (FilterMonad Response m,ServerMonad m, MonadIO m) => SessionId -> m ()
startSessionCookie sessionid = addCookie (60*60) $ mkCookie "sessionId" (show sessionid)

endSessionCookie :: (FilterMonad Response m,ServerMonad m,  MonadIO m) => m ()
endSessionCookie = expireCookie "sessionId" 
                                 
currentSessionIdFromCookie':: RqData (Maybe SessionId)
currentSessionIdFromCookie' = (optional (readCookieValue "sessionId")) 
 where optional c = (liftM Just c) `mplus` (return Nothing)
 
currentSession ::(MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session) 
currentSession = withDataFn currentSessionIdFromCookie' $ (\msid ->  
                            case (msid) of
                            Just sid-> query $ GetSession sid                           
                            Nothing ->  return  Nothing)

emptySessionData::IO SessionData                     
emptySessionData = do
                     now <- getMinutesTime
                     return $ SessionData {userID = Nothing,  flashMessages = [], expires = 60 `minutesAfter` now}  

                                                                
startSession :: (FilterMonad Response m,ServerMonad m,  MonadIO m, MonadPlus m) => m Session
startSession = do
                emptySessionData <- liftIO $ emptySessionData
                session <- update $ NewSession $ emptySessionData
                startSessionCookie (sessionId session)   
                return session                   
               
getUserFromSession::Session -> ServerPartT IO (Maybe User)               
getUserFromSession s = case (userID $ sessionData s) of
                        Just id -> query $ GetUserByUserID id
                        _ -> return Nothing    

getFlashMessagesFromSession s = return $ flashMessages $ sessionData s      
                  
                  
handleSession::ServerPartT IO Session                  
handleSession = do
                 msession <- currentSession    
                 case msession of 
                   Just session ->do
                                   now <- liftIO getMinutesTime
                                   if (now >= (expires $ sessionData $ session)) 
                                    then do
                                         update $ DelSession (sessionId session)
                                         startSession
                                    else return session      
                   Nothing -> startSession
                   

updateSessionWithContextData::Session -> (Maybe UserID)->[FlashMessage]->ServerPartT IO ()                                     
updateSessionWithContextData (Session id sd) u fm = update $ UpdateSession (Session id $ sd {userID = u, flashMessages = fm})