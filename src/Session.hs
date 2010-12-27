{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
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

import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data
import Happstack.Data.IxSet
import qualified Happstack.Data.IxSet as IxSet
import Happstack.Server.HTTP.Types ()
import Happstack.State 
import UserState (UserID,FlashMessage,GetUserByUserID(GetUserByUserID), User)
import MinutesTime
import Happstack.Server
import System.Random
import Happstack.Util.Common ( readM)
import Misc (MagicHash(MagicHash))

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
                                   
data SessionData1 = SessionData1 {  
                                  userID1::Maybe UserID,
                                  flashMessages1::[FlashMessage],
                                  expires1::MinutesTime
                               }  deriving (Ord,Eq,Show,Typeable,Data)
$(deriveSerialize ''SessionData1) 
instance Version (SessionData1) where
   mode = extension 1 (Proxy :: Proxy SessionData0)
                               
data SessionData = SessionData {  
                                  userID::Maybe UserID,
                                  flashMessages::[FlashMessage],
                                  expires::MinutesTime,
                                  hash::MagicHash
                               }  deriving (Ord,Eq,Show,Typeable,Data)                               
                               
$(deriveSerialize ''SessionData) 
instance Version (SessionData) where
   mode = extension 2 (Proxy :: Proxy SessionData1)
     
instance Migrate SessionData0 SessionData1 where
      migrate (SessionData0 {userID0 = _, flashMessages0 = _}) = SessionData1 {userID1 = Nothing, flashMessages1 = [], expires1 = MinutesTime 0}
    
instance Migrate SessionData1 SessionData where
      migrate (SessionData1 {userID1 = _, flashMessages1 = _, expires1 = _})
                             = SessionData {userID = Nothing, flashMessages = [], expires = MinutesTime 0, hash = MagicHash 0}
    
      

data Session = Session {sessionId::SessionId,
                        sessionData::SessionData}
               deriving (Ord, Eq, Show, Data, Typeable)

$(deriveSerialize ''Session)
instance Version (Session)
   
$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

instance Component (Sessions) where
  type Dependencies (Sessions) = End
  initialValue = IxSet.empty

-- Some helpers. MACID demands it before use.
-- |perform insert only if test is True
testAndInsert :: (MonadState (IxSet a) m, Data a, Ord a, Indexable a b) =>(IxSet a -> Bool) -> a -> m Bool
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

--Info that we store in cookies  
data SessionCookieInfo =  SessionCookieInfo {
                                         cookieSessionId::SessionId, --While parsing we depend on it containing just nums
                                         cookieSessionHash::MagicHash --While parsing we depend on it starting with alpha
                                         }
instance Show (SessionCookieInfo) where         
    show sci = (show $ cookieSessionId sci) ++ "-" ++ (show $ cookieSessionHash sci)

instance Read (SessionCookieInfo) where         
     readsPrec _ s = do
                        let (sid,sh) = break (== '-') s
                        sid' <- readM sid  --if need to understand that just read about list monad
                        sh' <- readM (drop 1 sh)  
                        return $ (SessionCookieInfo {cookieSessionId = sid', cookieSessionHash=sh'},"")
    
cookieInfoFromSession::Session->SessionCookieInfo
cookieInfoFromSession s =  SessionCookieInfo {
                                         cookieSessionId = sessionId s,
                                         cookieSessionHash = hash $ sessionData s }
                                         
sessionAndCookieHashMatch::Session->SessionCookieInfo->Bool    
sessionAndCookieHashMatch session sci =  (cookieSessionHash sci) == (hash $ sessionData session)
--- Session interface  
  
startSessionCookie :: (FilterMonad Response m,ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookie session = addCookie (60*60*24) $ mkCookie "sessionId" $ show $ cookieInfoFromSession session
                                 
currentSessionInfoCookie:: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = (optional (readCookieValue "sessionId")) 
 where optional c = (liftM Just c) `mplus` (return Nothing)
 
currentSession ::(MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session) 
currentSession = withDataFn currentSessionInfoCookie $ (\mscd ->  
                            case (mscd) of
                             Just scd-> do 
                                         session <- query $ GetSession $ cookieSessionId scd                           
                                         if (isJust session && sessionAndCookieHashMatch (fromJust session) scd) 
                                          then return session                               
                                          else return Nothing
                             Nothing ->  return  Nothing)

emptySessionData::IO SessionData                     
emptySessionData = do
                     now <- getMinutesTime
                     magicHash <-  randomIO
                     return $ SessionData {userID = Nothing,  flashMessages = [], expires = 60 `minutesAfter` now, hash = magicHash }  

                                                                
startSession :: (FilterMonad Response m,ServerMonad m,  MonadIO m, MonadPlus m) => m Session
startSession = do
                emptySession <- liftIO $ emptySessionData
                session <- update $ NewSession $ emptySession
                startSessionCookie session
                return session                   
               
getUserFromSession::Session -> ServerPartT IO (Maybe User)               
getUserFromSession s = case (userID $ sessionData s) of
                        Just i -> query $ GetUserByUserID i
                        _ -> return Nothing    
                        
getFlashMessagesFromSession::Session -> ServerPartT IO  [FlashMessage]
getFlashMessagesFromSession s = return $ flashMessages $ sessionData s      
                  
                  
handleSession::ServerPartT IO Session                  
handleSession = do
                 msession <- currentSession    
                 case msession of 
                   Just session ->do
                                   now <- liftIO getMinutesTime
                                   if (now >= (expires $ sessionData $ session)) 
                                    then do
                                         _ <- update $ DelSession (sessionId session)
                                         startSession
                                    else return session      
                   Nothing -> startSession
                   

updateSessionWithContextData::Session -> (Maybe UserID)->[FlashMessage]->ServerPartT IO ()                                     
updateSessionWithContextData (Session i sd) u fm = do
                                                    now <- liftIO getMinutesTime  
                                                    update $ UpdateSession (Session i $ sd {userID = u, flashMessages = fm,  expires = 60 `minutesAfter` now})