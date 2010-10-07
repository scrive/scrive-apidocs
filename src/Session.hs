{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Session 
    ( -- * Basic Types
      SessionData
    , SessionId
    , Session
    , Sessions
      -- * State functions
    , GetSession(..)
    , UpdateSession(..)
    , DelSession(..)
    , NewSession(..)
    , startSession  
    , endSession
    , currentSessionId
    , currentUserID
    , emptySessionDataWithUserID
    , getFlashMessages
    , addFlashMessage
    )
    where

import Control.Monad.Reader (MonadPlus(..), ap, ReaderT(..), asks, ask)
import Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing)
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


$(deriveAll [''Ord, ''Eq, ''Read, ''Show,''Default]
  [d|
     data SessionData = SessionData {userID::Maybe UserID,
                                     flashMessages::[FlashMessage]
                                    } 
   |])                                           

$(deriveSerialize ''SessionData)
instance Version SessionData   
    
$( deriveAll [''Ord, ''Eq, ''Read, ''Show, ''Default]
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
  
startSession :: (FilterMonad Response m,ServerMonad m) => SessionId -> m ()
startSession sessionid = do
  let cookie = mkCookie "sessionId" (show sessionid)
  addCookie (60*60) cookie

endSession :: (FilterMonad Response m,ServerMonad m) => m ()
--endSession = expireCookie "sessionId"
endSession = do
  let cookie = mkCookie "sessionId" ""
  -- FIXME: this will remove all cookies, which is bad probably
  setHeaderM "Set-Cookie" (mkCookieHeader 0 cookie)

currentSessionId':: RqData (Maybe SessionId)
currentSessionId' = (optional (readCookieValue "sessionId")) 
 where optional c = (liftM Just c) `mplus` (return Nothing)

currentSessionId::(MonadIO m, ServerMonad m, MonadPlus m) => m (Maybe SessionId)
currentSessionId = withDataFn currentSessionId'  return
 
fromSession_ :: (MonadIO m, ServerMonad m, MonadPlus m) => (Maybe SessionData -> a) -> m a
fromSession_ f = withDataFn currentSessionId' $ (\msid ->  
                          case (msid) of
                          Just sid-> 
                            do
                                  msession <- query (GetSession sid)
                                  case (msession) of
                                   Nothing -> return $ f Nothing
                                   Just sd   -> return $ f $ Just $ sessionData sd
                          Nothing ->  return $ f Nothing)

fromSession :: (MonadIO m, ServerMonad m, MonadPlus m) => (SessionData -> a) -> m (Maybe a)
fromSession f = fromSession_ (\msession -> case msession of
                                              Nothing -> Nothing
                                              Just sd -> Just $ f sd)

modifySession:: (MonadIO m, ServerMonad m, MonadPlus m) => (SessionData -> SessionData) -> m ()
modifySession f = withDataFn currentSessionId' $ (\msid -> 
                   case (msid) of
                          Just sid-> 
                                 do
                                  msession <- query (GetSession sid)
                                  case (msession) of
                                       Just session -> update $ UpdateSession $ session {sessionData = f (sessionData session)}
                                       Nothing -> return ()
                          Nothing -> return () )

                          
currentUserID::(MonadIO m, ServerMonad m, MonadPlus m) => m (Maybe UserID)
currentUserID = liftM join (fromSession userID)
                          
addFlashMessage :: (MonadIO m, ServerMonad m, MonadPlus m) => FlashMessage -> m ()
addFlashMessage fm = modifySession (\sd -> sd {flashMessages = fm:(flashMessages sd) })

getFlashMessages:: (MonadIO m, ServerMonad m, MonadPlus m) =>  m [FlashMessage]                       
getFlashMessages = do
                   l <- fromSession flashMessages
                   modifySession (\sd -> sd {flashMessages = []} )
                   case l of
                    Just l -> return l
                    _ -> return []

                         
emptySessionDataWithUserID userid = SessionData {userID = Just userid,  flashMessages = []}                                                                                                                                             