-- | Simple session support
module Session (
    Sessions
  , Session
  , SessionID
  , DropExpiredSessions(..)
  , getUserFromSession
  , getCompanyFromSession
  , getLocationFromSession
  , getPadUserFromSession
  , handleSession
  , updateSessionWithContextData
  , getELegTransactions
  , getSessionXToken
  , getMagicHashes

  , getSessionID
  , getSessionMagicHash
  , getSessionUserID
  , createServiceSession
  , loadServiceSession
  ) where

import Control.Applicative hiding (optional)
import Control.Arrow (first, second)
import Control.Monad.Reader (ask)
import Control.Monad.State hiding (State)
import Acid.Monad
import Data.Acid hiding (update, query)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.IxSet
import Data.SafeCopy
import User.Model
import MinutesTime
import Happstack.Server (RqData, ServerMonad, FilterMonad, Response, mkCookie,
  readCookieValue, withDataFn, HasRqData, CookieLife(MaxAge), FromReqURI(..))
import System.Random (mkStdGen, randomR)
import Crypto.RNG (CryptoRNG, random)
import Misc
import ELegitimation.ELegTransaction
import Data.Typeable
import Cookies
import Company.Model
import DB hiding (getOne, query, update)
import MagicHash (MagicHash)
import Doc.SignatoryLinkID
import qualified Data.Map as Map
import Data.List (find, sortBy)
import Data.Ord

newtype SessionID = SessionID Integer
  deriving (Eq, Ord, Num, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SessionID)
$(deriveSafeCopy 0 'base ''SessionID)

instance FromReqURI SessionID where
  fromReqURI = maybeRead

data SessionData = SessionData {
    userID           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
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
data Session = Session {
    sessionID   :: SessionID
  , sessionData :: SessionData
  } deriving (Ord, Eq, Show, Typeable)
$(deriveSafeCopy 0 'base ''Session)

type Sessions = IxSet Session

instance Indexable Session where
  empty = ixSet [
      ixFun (\x -> [sessionID x])
    , ixFun (\x -> case userID (sessionData x) of
                     Nothing -> []
                     Just uid -> [uid]
            )
    , ixFun (\x -> [expires $ sessionData x])
    , ixFun (\x -> case company (sessionData x) of
                     Nothing -> []
                     Just cid -> [cid]
            )
    ]

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

-- | Get the session data associated with the supplied 'SessionID'.
getSession :: SessionID -> Query Sessions (Maybe Session)
getSession sid = return . getOne . (@= sid) =<< ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByUserID :: UserID -> Query Sessions [Session]
getSessionsByUserID uid = return . toList . (@= uid) =<< ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByCompanyID :: CompanyID -> Query Sessions [Session]
getSessionsByCompanyID cid = return . toList . (@= cid) =<< ask

-- | Update the 'Session'.
updateSession :: Session -> Update Sessions ()
updateSession session = modify $ updateIx (sessionID session) session

-- | Delete the session associated with the 'SessionID'.
-- Returns the deleted session.
deleteSession :: SessionID -> Update Sessions (Maybe (Session))
deleteSession sessionID = do
  msession <- return . getOne . (@= sessionID) =<< get
  case msession of
    Nothing -> return Nothing
    Just session -> do
      modify $ delete session
      return $ Just session

-- | Start a new session with the supplied session data
-- returns: the 'SessionID'
newSession :: Int -> SessionData -> Update Sessions Session
newSession seed sessData = loop $ mkStdGen seed
  where
    loop rng = do
      let (sessId, rng') = first SessionID $ randomR (0,1000000000) rng
      let session = (Session sessId sessData)
      r <- testAndInsert (isNothing . getOne . (@= sessId)) session
      if r
        then return (Session sessId sessData)
        else loop rng'

-- | Drops expired session from database, to be used with scheduler
-- We need to clean db once in a while since sessions are created in db for each sessionless request.
dropExpiredSessions :: MinutesTime -> Update Sessions ()
dropExpiredSessions time = do
  sessions <- get
  let expired = sessions @<= time
  modify (\s -> difference s expired)

$(makeAcidic ''Sessions [
    'getSession
  , 'getSessionsByUserID
  , 'getSessionsByCompanyID
  , 'updateSession
  , 'deleteSession
  , 'newSession
  , 'dropExpiredSessions
  ])

-- | Info that we store in cookies.
data SessionCookieInfo = SessionCookieInfo {
    cookieSessionID   :: SessionID -- While parsing we depend on it containing just nums
  , cookieSessionHash :: MagicHash -- While parsing we depend on it starting with alpha
  }

instance Show SessionCookieInfo where
  show SessionCookieInfo{..} =
    show cookieSessionID ++ "-" ++ show cookieSessionHash

instance Read SessionCookieInfo where
  readsPrec _ s = do
    let (sid, msh) = second (drop 1) $ break (== '-') s
        (sh, rest) = splitAt 16 msh
    case SessionCookieInfo <$> maybeRead sid <*> maybeRead sh of
      Just sci -> [(sci, rest)]
      Nothing  -> []

instance FromReqURI SessionCookieInfo where
  fromReqURI = maybeRead

-- | Extract cookie from session.
cookieInfoFromSession :: Session -> SessionCookieInfo
cookieInfoFromSession s = SessionCookieInfo {
    cookieSessionID = sessionID s
  , cookieSessionHash = hash $ sessionData s
  }

-- | Check if cookie auth token matches.
sessionAndCookieHashMatch :: Session -> SessionCookieInfo -> Bool
sessionAndCookieHashMatch session sci =
  cookieSessionHash sci == hash (sessionData session)

-- | Add a session cookie to browser.
startSessionCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookie session = do
  ishttps  <- isHTTPS
  addHttpOnlyCookie ishttps (MaxAge (60*60*24)) $ mkCookie "sessionID" $ show $ cookieInfoFromSession session
  addCookie ishttps (MaxAge (60*60*24)) $ mkCookie "xtoken" $ show $ xtoken $ sessionData session

-- | Read current session cookie from request.
currentSessionInfoCookie :: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = optional (readCookieValue "sessionID")

-- | Get current session based on cookies set.
currentSession :: (HasAcidState Sessions m, HasRqData m, MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session)
currentSession = withDataFn currentSessionInfoCookie $ \mscd -> case mscd of
  Just scd -> do
    session <- query $ GetSession $ cookieSessionID scd
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
  return SessionData {
      userID = Nothing
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
isSessionDataEmpty SessionData{..} =
     userID == Nothing
  && padUserID == Nothing
  && Prelude.null elegtransactions && location == "" && company == Nothing
  && Map.null magichashes

-- | Return ID for temporary session (not written to db)
tempSessionID :: SessionID
tempSessionID = SessionID $ -1

-- | Return empty, temporary session
startSession :: (MonadIO m, CryptoRNG m) => m Session
startSession = emptySessionData >>= return . Session tempSessionID

-- | Get 'User' record from database based on userid in session
getUserFromSession :: MonadDB m => Session -> m (Maybe User)
getUserFromSession = maybe (return Nothing) (dbQuery . GetUserByID) . userID . sessionData

getPadUserFromSession :: MonadDB m => Session -> m (Maybe User)
getPadUserFromSession = maybe (return Nothing) (dbQuery . GetUserByID) . padUserID . sessionData

getCompanyFromSession :: MonadDB m => Session -> m (Maybe Company)
getCompanyFromSession = maybe (return Nothing) (dbQuery . GetCompany) . company . sessionData

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
          _ <- update $ DeleteSession (sessionID session)
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
  let newsd = sd {
          userID = u
        , expires = 120 `minutesAfter` now
        , elegtransactions = trans
        , magichashes = magichashes'
        , padUserID = pu
        }
  if i == tempSessionID && not (isSessionDataEmpty newsd)
    then do
      when (isNothing (userID sd) && isJust u) $ do
        us <- query $ GetSessionsByUserID $ fromJust u
        mapM_ (update . DeleteSession . sessionID) $
         drop 5 $ reverse $ sortBy (comparing $ expires . sessionData) us
      update (NewSession rng newsd) >>= startSessionCookie
    else update $ UpdateSession (Session i newsd)

-- | Get session ID from Session.
getSessionID :: Session -> SessionID
getSessionID = sessionID

-- | Get session auth token from Session data.
getSessionMagicHash :: Session -> MagicHash
getSessionMagicHash = hash . sessionData

-- | Get user id from session data.
getSessionUserID :: Session -> (Maybe UserID)
getSessionUserID = userID . sessionData

-- | Get e-leg from session.
getELegTransactions :: Session -> [ELegTransaction]
getELegTransactions = elegtransactions . sessionData

-- | Get recorded magic hashes from session object.
getMagicHashes :: Session -> Map.Map SignatoryLinkID MagicHash
getMagicHashes = magichashes . sessionData

-- | Get the xtoken from the session data
getSessionXToken :: Session -> MagicHash
getSessionXToken = xtoken . sessionData

-- | Creates a session for user and service
createServiceSession :: (MonadIO m, CryptoRNG m, HasAcidState Sessions m) => Either CompanyID UserID -> String -> m SessionID
createServiceSession userorcompany loc = do
  now <- getMinutesTime
  moldsession <- case userorcompany of
    Right uid -> query $ GetSessionsByUserID uid
    Left  cid -> query $ GetSessionsByCompanyID cid
  case find (\s -> now < expires (sessionData s)) moldsession of
    Just s  -> return $ sessionID s
    Nothing -> do
      sd <- emptySessionData
      rng <- random
      session <-  update $ NewSession rng $  sd {
        userID = either (const Nothing) Just userorcompany
        , company = either Just (const Nothing) userorcompany
        , location = loc
        }
      return $ sessionID session

-- This is used to connect user or company to session when it was created by same service
loadServiceSession :: (MonadIO m, ServerMonad m, HasAcidState Sessions m, FilterMonad Response m) => Either CompanyID UserID -> SessionID -> m Bool
loadServiceSession userorcompany ssid  = do
  msession <- query $ GetSession ssid
  case msession of
    Nothing -> return False
    Just session@Session{sessionData} ->
      case (userorcompany) of
        Left cid -> if company sessionData == Just cid
                      then do
                        startSessionCookie session
                        return True
                      else return False
        Right uid -> if userID sessionData == Just uid
                       then do
                         startSessionCookie session
                         return True
                       else return False
