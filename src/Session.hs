-- |Simple session support
module Session
    ( Sessions
    , Session
    , SessionId
    , getUserFromSession
    , getCompanyFromSession
    , getLocationFromSession
    , getPadUserFromSession
    , handleSession
    , updateSessionWithContextData
    , getELegTransactions
    , getSessionXToken
    , getMagicHashes

    , getSessionId
    , getSessionMagicHash
    , getSessionUserID
    , dropSession
    , dropExpiredSessions
    , createServiceSession
    , loadServiceSession
    )
    where

import Control.Arrow (first)
import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import Acid.Monad
import Data.Acid hiding (update, query)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.IxSet
import Data.SafeCopy
import User.Model
import Numeric
import MinutesTime
import Happstack.Server (RqData, ServerMonad, FilterMonad, Response, mkCookie,
  readCookieValue, withDataFn, HasRqData, CookieLife(MaxAge), FromReqURI(..))
import System.Random (mkStdGen, randomR)
import System.Random.CryptoRNG ()
import Crypto.RNG (CryptoRNG, random)
import Misc
import ELegitimation.ELegTransaction
import Data.Typeable
import Cookies
import Company.Model
import DB hiding (getOne, query, update)
import MagicHash (MagicHash)
import Util.MonadUtils
import Doc.SignatoryLinkID
import qualified Data.Map as Map
import Data.List (find, sortBy)
import Data.Ord

newtype SessionId = SessionId Integer
    deriving (Eq, Ord, Num, Typeable)

$(deriveSafeCopy 0 'base ''SessionId)

instance Show SessionId where
    showsPrec prec (SessionId v) = showsPrec prec v

instance Read SessionId where
    readsPrec _prec string =
        [(SessionId k,v) | (k,v) <- readSigned readDec string]

instance FromReqURI SessionId where
    fromReqURI = readM

data SessionData = SessionData
    { userID           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires          :: MinutesTime       -- ^ when does this session expire
    , hash             :: MagicHash         -- ^ session security token
    , elegtransactions :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken           :: MagicHash         -- ^ Random string to prevent CSRF
    , location         :: String            -- ^ Id of the service
                                            -- that we are now working
                                            -- with. Also link to page
                                            -- that embeds it (hash
                                            -- location hack)
    , company          :: Maybe CompanyID
    , magichashes      :: Map.Map SignatoryLinkID MagicHash
    , padUserID        :: Maybe UserID      -- Other version of login - for pad devices, when you don't want to give control over a device.
    } deriving (Ord, Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''SessionData)

-- | 'Session' data as we keep it in our database
data Session = Session { sessionId::SessionId
                       , sessionData::SessionData
                       }
               deriving (Ord, Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''Session)

type Sessions = IxSet Session

instance Indexable Session where
        empty = ixSet
                [ ixFun (\x -> [sessionId x])
                , ixFun (\x -> case userID (sessionData x) of
                                   Nothing -> []
                                   Just userid -> [userid]
                        )
                , ixFun (\x -> case company (sessionData x) of
                            Nothing -> []
                            Just cid -> [cid])
                ]

-- Some helpers. MACID demands it before use.
-- | Perform insert only if test is True
testAndInsert :: (MonadState (IxSet a) m, Typeable a, Ord a, Indexable a) =>(IxSet a -> Bool) -> a -> m Bool
testAndInsert test a =
    maybeModify $ \ixset ->
        if test ixset
          then Just (insert a ixset)
          else Nothing

-- | this should be sent upstream to mtl
maybeModify :: (MonadState s m) => (s -> Maybe s) -> m Bool
maybeModify f = do
    s <- get
    case f s of
         Nothing -> return False
         Just s' -> do
             put s'
             return True

-- | Get the session data associated with the supplied 'SessionId'.
getSession :: SessionId -> Query Sessions (Maybe (Session))
getSession sessionId = (return . getOne . (@= sessionId)) =<< ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByUserId :: UserID -> Query Sessions [Session]
getSessionsByUserId userId = (return . toList . (@= userId)) =<< ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByCompanyId :: CompanyID -> Query Sessions [Session]
getSessionsByCompanyId companyId = (return . toList . (@= companyId)) =<< ask

-- | Update the 'Session'.
--
updateSession :: (Session) -> Update (Sessions) ()
updateSession session = modify (updateIx (sessionId session) session)

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
newSession :: Int -> SessionData -> Update Sessions Session
newSession seed sessData = loop $ mkStdGen seed
  where
    loop rng = do
      let (sessId, rng') = first SessionId $ randomR (0,1000000000) rng
      let session = (Session sessId sessData)
      r <- testAndInsert (isNothing . getOne . (@= sessId)) session
      if r
        then return (Session sessId sessData)
        else loop rng'

-- | Drops expired session from database, to be used with scheduler
-- We need to clean db once in a while since sessions are created in db for each sessionless request.
dropExpired :: MinutesTime -> Update Sessions ()
dropExpired now = do
    sessions <- get
    let expired = (flip filter) (toList  sessions) (\s -> now >  120 `minutesAfter` (expires $ sessionData s))
    sequence_ $ map (modify . delete) expired

$(makeAcidic ''Sessions
  [ 'getSession
  , 'getSessionsByUserId
  , 'getSessionsByCompanyId
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

instance FromReqURI SessionCookieInfo where
    fromReqURI = readM

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
startSessionCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookie session = do
    ishttps  <- isHTTPS
    addHttpOnlyCookie ishttps (MaxAge (60*60*24)) $ mkCookie "sessionId" $ show $ cookieInfoFromSession session
    addCookie ishttps (MaxAge (60*60*24)) $ mkCookie "xtoken" $ show $ xtoken $ sessionData session

-- | Read current session cookie from request.
currentSessionInfoCookie:: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = optional (readCookieValue "sessionId")

-- | Get current session based on cookies set.
currentSession :: (HasAcidState Sessions m, HasRqData m, MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session)
currentSession = withDataFn currentSessionInfoCookie $ \mscd -> case mscd of
  Just scd -> do
    session <- query $ GetSession $ cookieSessionId scd
    case session of
      Just s | sessionAndCookieHashMatch s scd -> return $ Just s
      _ -> return Nothing
  Nothing ->  return Nothing

-- | Create empty session data. It has proper timeout already set.
emptySessionData :: (MonadIO m, CryptoRNG m) => m SessionData
emptySessionData = do
    now       <- getMinutesTime
    magicHash <- random
    xhash     <- random
    return $ SessionData { userID = Nothing
                         , expires = 120 `minutesAfter` now
                         , hash = magicHash
                         , elegtransactions = []
                         , xtoken = xhash
                         , location = ""
                         , company = Nothing
                         , magichashes = Map.empty
                         , padUserID = Nothing
                         }

-- | Check if session data is empty
isSessionDataEmpty :: SessionData -> Bool
isSessionDataEmpty SessionData{userID, elegtransactions, location, company, magichashes, padUserID} =
       userID == Nothing
    && padUserID == Nothing   
    && Prelude.null elegtransactions && location == "" && company == Nothing
    && Map.null magichashes

-- | Return ID for temporary session (not written to db)
tempSessionID :: SessionId
tempSessionID = SessionId $ -1

-- | Return empty, temporary session
startSession :: (MonadIO m, CryptoRNG m) => m Session
startSession = emptySessionData >>= return . Session tempSessionID

-- | Get 'User' record from database based on userid in session
getUserFromSession :: MonadDB m => Session -> m (Maybe User)
getUserFromSession = liftMM (dbQuery . GetUserByID) . return . userID . sessionData

getPadUserFromSession :: MonadDB m => Session -> m (Maybe User)
getPadUserFromSession = liftMM (dbQuery . GetUserByID) . return . padUserID . sessionData

getCompanyFromSession :: MonadDB m => Session -> m (Maybe Company)
getCompanyFromSession = liftMM (dbQuery . GetCompany) . return . company . sessionData

getLocationFromSession :: Session -> String
getLocationFromSession = location . sessionData

-- | Handles session timeout. Starts new session when old session timed out.
handleSession :: (CryptoRNG m, FilterMonad Response m, HasAcidState Sessions m, HasRqData m, MonadIO m, MonadPlus m, ServerMonad m) => m Session
handleSession = do
  msession <- currentSession
  case msession of
    Just session -> do
      now <- getMinutesTime
      if now >= (expires $ sessionData $ session)
        then do
          _ <- update $ DelSession (sessionId session)
          startSession
        else return session
    Nothing -> startSession

-- | Updates session data. If session is temporary and new
-- session data is non-empty, register session in the system
-- and add a cookie. Is user loggs in, check whether there is
-- an old session with his userid and throw it away.
updateSessionWithContextData :: (FilterMonad Response m, HasAcidState Sessions m, ServerMonad m, MonadDB m, CryptoRNG m)
                             => Session
                             -> Maybe UserID
                             -> [ELegTransaction]
                             -> Map.Map SignatoryLinkID MagicHash
                             -> Maybe UserID
                             -> m ()
updateSessionWithContextData (Session i sd) u trans magichashes' pu = do
    now <- getMinutesTime
    rng <- random
    let newsd = sd
                { userID = u
                , expires = 120 `minutesAfter` now
                , elegtransactions = trans
                , magichashes = magichashes'
                , padUserID = pu
                }
    if i == tempSessionID && not (isSessionDataEmpty newsd)
       then do
           when (isNothing (userID sd) && isJust u) $ do
               sess <- query $ GetSessionsByUserId $ fromJust u
               case drop 5 $ reverse $ sortBy (comparing (expires . sessionData)) sess of
                    [] -> return ()
                    l  -> forM_ l (\s -> update $ DelSession $ sessionId s)
           update (NewSession rng newsd) >>= startSessionCookie
       else update $ UpdateSession (Session i newsd)

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
dropSession :: (HasAcidState Sessions m, MonadIO m) => SessionId -> m ()
dropSession sid = do
  _ <- update $ DelSession sid
  return ()

-- | Delete all expired session from database. The param says what
-- time we have now. All sessions that expire earlier than that are
-- plainly forgotten.
dropExpiredSessions :: (HasAcidState Sessions m, MonadIO m) => MinutesTime -> m ()
dropExpiredSessions = update . DropExpired

-- | Get e-leg from session.
getELegTransactions :: Session -> [ELegTransaction]
getELegTransactions = elegtransactions . sessionData

-- | Get recorded magic hashes from session object.
getMagicHashes :: Session -> Map.Map SignatoryLinkID MagicHash
getMagicHashes = magichashes . sessionData

-- | Get the xtoken from the session data
getSessionXToken :: Session -> MagicHash
getSessionXToken = xtoken . sessionData

--- | Creates a session for user and service
createServiceSession:: (MonadIO m, CryptoRNG m, HasAcidState Sessions m) => Either CompanyID UserID -> String -> m SessionId
createServiceSession userorcompany loc = do
    now <- getMinutesTime
    moldsession <- case userorcompany of
      Right uid -> query $ GetSessionsByUserId uid
      Left  cid -> query $ GetSessionsByCompanyId cid
    case find (\s -> now < expires (sessionData s)) moldsession of
      Just s -> return $ sessionId s
      Nothing   -> newSession' userorcompany loc

newSession' :: (MonadIO m, CryptoRNG m, HasAcidState Sessions m) => Either CompanyID UserID -> String -> m SessionId
newSession' userorcompany loc = do
  sd <- emptySessionData
  rng <- random
  session <-  update $ NewSession rng $  sd {
    userID = either (const Nothing) Just userorcompany
    , company = either Just (const Nothing) userorcompany
    , location = loc
    }
  return $ sessionId  session


-- This is used to connect user or company to session when it was created by same service
loadServiceSession :: (MonadIO m, ServerMonad m, HasAcidState Sessions m, FilterMonad Response m) => Either CompanyID UserID -> SessionId -> m Bool
loadServiceSession userorcompany ssid  = do
    msession <- query $ GetSession ssid
    case msession of
        Nothing -> return False
        Just session@(Session{sessionData}) ->
            case (userorcompany) of
              (Left cid) -> if (company sessionData == Just cid)
                                     then startSessionCookie session >> return True
                                     else return False
              (Right uid) -> if (userID sessionData == Just uid)
                                     then startSessionCookie session >> return True
                                     else return False


