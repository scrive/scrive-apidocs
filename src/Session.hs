{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Session 
    ( Sessions
    , Session
    , SessionId    
    , startSessionForUser  
    , endSession
    , currentUserID
    , getFlashMessages
    , addFlashMessage
    , startSessionWhenNoSession
    , removeSessionIfExpired
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
                        mkMethods, query, update)
import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import UserState (UserID,FlashMessage)
import MinutesTime

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


$(deriveAll [''Ord, ''Eq,''Show,''Default]
  [d|
     data SessionData = SessionData {userID::Maybe UserID,
                                     flashMessages::[FlashMessage],
                                     expires::MinutesTime
                                    } 
   |])                                           

$(deriveSerialize ''SessionData)
instance Version SessionData   
    
$( deriveAll [''Ord, ''Eq, ''Show, ''Default]
   [d|
       data Session = Session {sessionId::SessionId,
                               sessionData::SessionData}
    |])

$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

$(deriveSerialize ''Session)
instance Version (Session)

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
newSession :: SessionData -> Update Sessions SessionId
newSession sessData =
    do sessId <- fmap SessionId $ getRandomR (0,1000000000)
       let session = (Session sessId sessData)
       r <- testAndInsert (isNothing . getOne . (@= sessId)) session
       if r
          then return sessId
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

removeSessionIfExpired::(MonadIO m, ServerMonad m, MonadPlus m,FilterMonad Response m) =>  m ()  
removeSessionIfExpired =  do
                          ms <- currentSession
                          now <- liftIO $ getMinutesTime
                          case ms of 
                            Just s ->  do
                                        when (now >= (expires $ sessionData $ s)) $
                                           do
                                            update $ DelSession (sessionId s)
                                            return ()
                                             
                            Nothing -> return ()
                                   
  
currentSessionIdFromCookie':: RqData (Maybe SessionId)
currentSessionIdFromCookie' = (optional (readCookieValue "sessionId")) 
 where optional c = (liftM Just c) `mplus` (return Nothing)

currentSessionIdFromCookie::(MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe SessionId)
currentSessionIdFromCookie = withDataFn currentSessionIdFromCookie'  return
 
currentSession ::(MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session) 
currentSession = withDataFn currentSessionIdFromCookie' $ (\msid ->  
                            case (msid) of
                            Just sid-> query $ GetSession sid                           
                            Nothing ->  return  Nothing)


fromSession_ :: (MonadIO m, ServerMonad m, MonadPlus m,FilterMonad Response m) => (Maybe SessionData -> a) -> m a
fromSession_ f = withDataFn currentSessionIdFromCookie' $ (\msid ->  
                          case (msid) of
                          Just sid-> 
                            do
                                  msession <- query $ GetSession sid
                                  case (msession) of
                                   Nothing -> return $ f Nothing
                                   Just sd   -> return $ f $ Just $ sessionData sd
                          Nothing ->  return $ f Nothing)

fromSession :: (MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => (SessionData -> a) -> m (Maybe a)
fromSession f = fromSession_ (\msession -> case msession of
                                              Nothing -> Nothing
                                              Just sd -> Just $ f sd)

modifySession:: (MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => (SessionData -> SessionData) -> m ()
modifySession f = withDataFn currentSessionIdFromCookie' $ (\msid -> 
                   case (msid) of
                          Just sid-> 
                                 do
                                  msession <- query $ GetSession sid
                                  case (msession) of
                                       Just session -> update $ UpdateSession $ session {sessionData = f (sessionData session)}
                                       Nothing -> return ()
                          Nothing -> return () )


--Session data access
                          
currentUserID::(MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe UserID)
currentUserID = liftM join (fromSession userID)
                          
addFlashMessage :: (MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => FlashMessage -> m ()
addFlashMessage fm = modifySession (\sd -> sd {flashMessages = fm:(flashMessages sd) })

getFlashMessages:: (MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) =>  m [FlashMessage]                       
getFlashMessages = do
                   l <- fromSession flashMessages
                   modifySession (\sd -> sd {flashMessages = []} )
                   case l of
                    Just l -> return l
                    _ -> return []

                    
--Contructors                     
                    
emptySessionDataWithUserID::UserID -> IO SessionData                                              
emptySessionDataWithUserID userid = do
                     now <- getMinutesTime
                     return $ SessionData {userID = Just userid,  flashMessages = [], expires = 60 `minutesAfter` now}
                     
emptySessionData::IO SessionData                     
emptySessionData = do
                     now <- getMinutesTime
                     return $ SessionData {userID = Nothing,  flashMessages = [], expires = 60 `minutesAfter` now}  

                     
--Starting and ending sessions                     
startSessionForUser :: (FilterMonad Response m,ServerMonad m,  MonadIO m, MonadPlus m) => UserID -> m ()
startSessionForUser userId = do
                              sessionDataForUser <- liftIO $ emptySessionDataWithUserID userId
                              sessionid <- update $ NewSession $ sessionDataForUser       
                              startSessionCookie sessionid    
   
                                           
startSessionWhenNoSession :: (FilterMonad Response m,ServerMonad m,  MonadIO m, MonadPlus m) => m ()
startSessionWhenNoSession = do
                           msession <- currentSession
                           when (isNothing msession) $
                             do
                              emptySessionData <- liftIO $ emptySessionData
                              sessionid <- update $ NewSession $ emptySessionData
                              startSessionCookie sessionid    
                              
                              
endSession :: (FilterMonad Response m,ServerMonad m,  MonadIO m, MonadPlus m) => m ()
endSession =  do
               sid <- currentSessionIdFromCookie 
               when (isJust sid) $
                 do update $ DelSession (fromJust sid)
                    return ()
               endSessionCookie
