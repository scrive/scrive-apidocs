{-# OPTIONS_GHC -Wall #-}
-- |Simple session support
module Session 
    ( Sessions
    , Session 
    , SessionId
    , getFlashMessagesFromSession
    , getUserFromSession
    , handleSession
    , updateSessionWithContextData
    , getELegTransactions
    
    -- | Functions usefull when we do remember passwords emails
    , createLongTermSession
    , findSession
    , getSessionId
    , getSessionMagicHash
    , getSessionUserID
    , dropSession
    , dropExpiredSessions
    )
    where

import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import qualified Data.Foldable as F
import Data.Generics
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data
import Happstack.Data.IxSet
import qualified Happstack.Data.IxSet as IxSet
import Happstack.State 
import User.UserState (UserID,FlashMessage,GetUserByUserID(GetUserByUserID), User)
import MinutesTime
import Happstack.Server (RqData, ServerMonad, FilterMonad, Response, mkCookie, addCookie, readCookieValue, withDataFn, ServerPartT, HasRqData, CookieLife(MaxAge))
import System.Random
import Happstack.Util.Common ( readM)
import Misc (MagicHash(MagicHash), mkTypeOf)
import ELegitimation.ELeg

-- | Session ID is a wrapped 'Integer' really
newtype SessionId = SessionId Integer
    deriving (Eq, Ord, Num, Typeable, Data)


instance Show SessionId where
    showsPrec prec (SessionId v) = showsPrec prec v

instance Read SessionId where
    readsPrec prec string = 
        [(SessionId k,v) | (k,v) <- readsPrec prec string]

$(deriveSerialize ''SessionId)
instance Version SessionId   
  
  
data SessionData0 = SessionData0 { userID0 :: Maybe UserID,
                                   flashMessages0 :: [FlashMessage]
                                 } deriving (Ord,Eq,Show,Typeable,Data)
$(deriveSerialize ''SessionData0)
instance Version (SessionData0)
                                   
data SessionData1 = SessionData1 {  
                                  userID1 :: Maybe UserID,
                                  flashMessages1 :: [FlashMessage],
                                  expires1 :: MinutesTime
                               }  deriving (Ord,Eq,Show,Typeable,Data)

$(deriveSerialize ''SessionData1) 
instance Version (SessionData1) where
   mode = extension 1 (Proxy :: Proxy SessionData0)
                               
data SessionData2 = SessionData2 {  
                                  userID2 :: Maybe UserID,
                                  flashMessages2 :: [FlashMessage],
                                  expires2 :: MinutesTime,
                                  hash2 :: MagicHash
                               }  deriving (Ord,Eq,Show,Typeable,Data)                               
                               
$(deriveSerialize ''SessionData2) 
instance Version (SessionData2) where
   mode = extension 2 (Proxy :: Proxy SessionData1)

-- | SessionData is everything we want to know about a person clicking
-- on the other side of the wire.
data SessionData = SessionData 
    { userID::Maybe UserID                    -- ^ Just 'UserID' if a person is logged in
    , flashMessages::[FlashMessage]           -- ^ 'FlashMessage's that are to be shown on next page
    , expires::MinutesTime                    -- ^ when does this session expire
    , hash::MagicHash                         -- ^ session security token
    , elegtransactions::[ELegTransaction]     -- ^ ELeg transaction stuff
    }  deriving (Ord,Eq,Show,Typeable,Data)                               
                               
$(deriveSerialize ''SessionData) 
instance Version (SessionData) where
    mode = extension 3 (Proxy :: Proxy SessionData2)
     
instance Migrate SessionData0 SessionData1 where
    migrate (SessionData0 { userID0 = _
                          , flashMessages0 = _
                          }) = 
        SessionData1 { userID1 = Nothing
                     , flashMessages1 = []
                     , expires1 = MinutesTime 0 0
                     }
    
instance Migrate SessionData1 SessionData2 where
    migrate (SessionData1 { userID1 = _
                          , flashMessages1 = _
                          , expires1 = _
                          }) = 
        SessionData2 { userID2 = Nothing
                     , flashMessages2 = []
                     , expires2 = MinutesTime 0 0
                     , hash2 = MagicHash 0
                     }

instance Migrate SessionData2 SessionData where
    migrate (SessionData2 { userID2 = _
                          , flashMessages2 = _
                          , expires2 = _
                          }) =
        SessionData { userID = Nothing
                    , flashMessages = []
                    , expires = MinutesTime 0 0
                    , hash = MagicHash 0
                    , elegtransactions=[]
                    }

-- | 'Session' data as we keep it in our database
data Session = Session { sessionId::SessionId
                       , sessionData::SessionData
                       }
               deriving (Ord, Eq, Show, Data)

-- | WARNING: This needs to specify \"Session\" as full data type
-- name.  Do not use the deriving mechanism as that one produces full
-- name with module like \"MyApp.MyModule.Session\" and this is used
-- in database table name.
instance Typeable Session where typeOf _ = mkTypeOf "Session"

$(deriveSerialize ''Session)
instance Version (Session)
   
-- FIXME: Use new Happstack6 indexing here to speed up things
$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

instance Component (Sessions) where
  type Dependencies (Sessions) = End
  initialValue = IxSet.empty

-- Some helpers. MACID demands it before use.
-- | Perform insert only if test is True
testAndInsert :: (MonadState (IxSet a) m, Data a, Ord a, Indexable a) =>(IxSet a -> Bool) -> a -> m Bool
testAndInsert test a =
    maybeModify $ \ixset ->
        if test ixset
          then Just (insert a ixset)
          else Nothing

-- | this should be sent upstream to mtl
maybeModify :: (MonadState s m) => (s -> Maybe s) -> m Bool
maybeModify f =
    do state <- get
       case f state of
         Nothing -> return False
         (Just state') -> 
             do put state' 
                return True

-- | Get the session data associated with the supplied 'SessionId'.
getSession :: SessionId -> Query Sessions (Maybe (Session))
getSession sessionId = (return . getOne . (@= (sessionId :: SessionId))) =<< ask

-- | Get the session data associated with the supplied 'UserId'.
getSessionByUserId :: UserID -> Query Sessions (Maybe (Session))
getSessionByUserId userid = return . F.find cmp . IxSet.toList =<< ask
    where
        cmp Session{sessionData = sd} =
            case userID sd of
                 Just uid -> uid == userid
                 Nothing  -> False

-- | Update the 'Session'.
--
-- FIXME: do not use gFind here as it is slow.
updateSession :: (Session) -> Update (Sessions) ()
updateSession session = modify (updateIx (gFind' session :: SessionId) session)

-- | Delete the session associated with the 'SessionId'.
-- Returns the deleted session.
delSession :: SessionId -> Update Sessions (Maybe (Session))
delSession sessionId =
    do mSession <- (return . getOne . (@= (sessionId :: SessionId))) =<< get
       case mSession of
         Nothing -> return Nothing
         Just session -> 
             do modify (delete session)
                return (Just session)

-- | Start a new session with the supplied session data
-- returns: the 'SessionId'
newSession :: SessionData -> Update Sessions Session
newSession sessData =
    do sessId <- fmap SessionId $ getRandomR (0,1000000000)
       let session = (Session sessId sessData)
       r <- testAndInsert (isNothing . getOne . (@= sessId)) session
       if r
          then return (Session sessId sessData)
          else newSession sessData


-- | Drops expired session from database, to be used with scheduler
-- We need to clean db once in a while since sessions are created in db for each sessionless request.
dropExpired :: MinutesTime -> Update Sessions ()
dropExpired now = do
    sessions <- ask
    let expired = (flip filter) (toList  sessions) (\s -> now >  60 `minutesAfter` (expires $ sessionData s))
    sequence_ $ map (modify . delete ) expired

$(mkMethods ''Sessions 
  [ 'getSession
  , 'getSessionByUserId
  , 'updateSession
  , 'delSession
  , 'newSession
  , 'dropExpired
  ])

-- | Info that we store in cookies.
--
-- FIXME: why do we need this? Doesn't session cover also auth token?
data SessionCookieInfo = SessionCookieInfo
    { cookieSessionId :: SessionId  --  While parsing we depend on it containing just nums
    , cookieSessionHash :: MagicHash --  While parsing we depend on it starting with alpha
    }

instance Show (SessionCookieInfo) where         
    show sci = (show $ cookieSessionId sci) ++ "-" ++ (show $ cookieSessionHash sci)

instance Read (SessionCookieInfo) where         
    readsPrec _ s = do
        let (sid,sh) = break (== '-') s
        sid' <- readM sid  --if need to understand that just read about list monad
        sh' <- readM (drop 1 sh)  
        return $ (SessionCookieInfo {cookieSessionId = sid', cookieSessionHash=sh'},"")
    
-- | Extract cookie from session.
cookieInfoFromSession :: Session -> SessionCookieInfo
cookieInfoFromSession s = SessionCookieInfo 
                          { cookieSessionId = sessionId s
                          , cookieSessionHash = hash $ sessionData s
                          }
                                        
-- | Check if cookie auth token matches. 
sessionAndCookieHashMatch :: Session -> SessionCookieInfo -> Bool    
sessionAndCookieHashMatch session sci = (cookieSessionHash sci) == (hash $ sessionData session)


-- | Add a session cookie to browser.  
startSessionCookie :: (FilterMonad Response m,ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookie session = addCookie (MaxAge (60*60*24)) $ mkCookie "sessionId" $ show $ cookieInfoFromSession session
                                 
-- | Read current session cookie from request.
currentSessionInfoCookie:: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = (optional (readCookieValue "sessionId")) 
 where optional c = (liftM Just c) `mplus` (return Nothing)
 
-- | Get current session based on cookies set.
currentSession ::(HasRqData m, MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session) 
currentSession = withDataFn currentSessionInfoCookie $ \mscd ->  
                 case mscd of
                     Just scd-> do 
                                 session <- query $ GetSession $ cookieSessionId scd                           
                                 if isJust session && sessionAndCookieHashMatch (fromJust session) scd
                                     then return session                               
                                     else return Nothing
                     Nothing ->  return Nothing

-- | Create empty session data. It has proper timeout already set.
emptySessionData :: IO SessionData                     
emptySessionData = do
    now <- getMinutesTime
    magicHash <-  randomIO
    return $ SessionData { userID = Nothing
                         , flashMessages = []
                         , expires = 60 `minutesAfter` now
                         , hash = magicHash
                         , elegtransactions = []
                         }  

-- | Check if session data is empty
isSessionDataEmpty :: SessionData -> Bool
isSessionDataEmpty SessionData{userID, flashMessages, elegtransactions} =
       userID == Nothing
    && Prelude.null flashMessages
    && Prelude.null elegtransactions

-- | Return ID for temporary session (not written to db)
tempSessionID :: SessionId
tempSessionID = SessionId $ -1

-- | Return empty, temporary session
startSession :: (FilterMonad Response m, ServerMonad m, MonadIO m, MonadPlus m) => m Session
startSession = liftIO emptySessionData >>= return . Session tempSessionID

-- | Get 'User' record from database based on userid in session       
getUserFromSession :: Session -> ServerPartT IO (Maybe User)               
getUserFromSession s = 
    case (userID $ sessionData s) of
        Just i -> query $ GetUserByUserID i
        _ -> return Nothing    

-- | Get 'FlashMessage's to be show at this request. Does not clear
-- flash message list though.
getFlashMessagesFromSession :: Session -> ServerPartT IO  [FlashMessage]
getFlashMessagesFromSession s = return $ flashMessages $ sessionData s      

-- | Handles session timeout. Starts new session when old session timed out.
handleSession :: ServerPartT IO Session                  
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

-- | Updates session data. If session is temporary and new
-- session data is non-empty, register session in the system
-- and add a cookie. Is user loggs in, check whether there is
-- an old session with his userid and throw it away.
updateSessionWithContextData :: Session -> Maybe UserID -> [FlashMessage] -> [ELegTransaction] -> ServerPartT IO ()
updateSessionWithContextData (Session i sd) u fm trans = do
    now <- liftIO getMinutesTime
    let newsd = sd {
        userID = u
        , flashMessages = fm
        , expires = 60 `minutesAfter` now
        , elegtransactions = trans
    }
    if i == tempSessionID && not (isSessionDataEmpty newsd)
       then do
           when (isNothing (userID sd) && isJust u) $ do
               msess <- query $ GetSessionByUserId $ fromJust u
               case msess of
                    Just sess -> do
                        _ <- update $ DelSession $ sessionId sess
                        return ()
                    Nothing   -> return ()
           update (NewSession newsd) >>= startSessionCookie
       else update $ UpdateSession (Session i newsd)

-- | This are special sessions used for passwords reminder links. Such links should be carefully.
createLongTermSession :: (MonadIO m) =>  UserID -> m Session
createLongTermSession uid = do
    now <- liftIO $ getMinutesTime
    magicHash <- liftIO $ randomIO
    let longUserSession = SessionData { userID = Just uid
                                      , flashMessages = []
                                      , expires = (60 * 12) `minutesAfter` now
                                      , hash = magicHash
                                      , elegtransactions = []
                                      }  
    update $ NewSession $ longUserSession

-- | Find session in the database. Check auth token match. Check timeout.
findSession :: (MonadIO m) => SessionId -> MagicHash -> m (Maybe Session)
findSession sid mh = do
    ms <- query $ GetSession $ sid
    case ms of 
        Just session ->
            do
                now <- liftIO getMinutesTime
                let hashmatch = mh == (hash $ sessionData session)
                let notexpired = now <= (expires $ sessionData $ session)
                if (hashmatch && notexpired)
                    then return $ Just session
                    else do
                        _ <- update $ DelSession (sessionId session)
                        return Nothing
        Nothing -> return Nothing
                
-- | Get session ID from Session.         
getSessionId :: Session -> SessionId
getSessionId = sessionId 

-- | Get session auth token from Session data.
getSessionMagicHash :: Session -> MagicHash
getSessionMagicHash = hash . sessionData

-- | Get user id from session data.
getSessionUserID :: Session -> (Maybe UserID)
getSessionUserID = userID . sessionData

-- | Delete session record from database.
dropSession :: SessionId -> IO ()
dropSession sid = (update $ DelSession sid) >> return ()

-- | Delete all expired session from database. The param says what
-- time we have now. All sessions that expire earlier than that are
-- plainly forgotten.
dropExpiredSessions :: MinutesTime -> IO ()
dropExpiredSessions = update . DropExpired

-- | Get e-leg from session.
getELegTransactions :: Session -> [ELegTransaction]
getELegTransactions = elegtransactions . sessionData