-- | Simple session support
module Session (
    Sessions
  , initialSessions
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
  ) where

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid (makeAcidic, Update, Query)
import Data.IxSet as I
import Data.List hiding (insert)
import Data.Maybe
import Data.Ord
import Data.SafeCopy
import Data.Typeable
import Data.Word
import Happstack.Server hiding (addCookie, Session)
import qualified Data.Map as M

import Acid.Monad
import Cookies
import Company.Model
import Crypto.RNG
import DB hiding (getOne, query, update)
import Doc.SignatoryLinkID
import ELegitimation.ELegTransaction
import MagicHash
import MinutesTime
import Utils.HTTP
import Utils.Monoid
import Utils.Read
import User.Model

newtype SessionID = SessionID Word64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SessionID)
$(deriveSafeCopy 0 'base ''SessionID)

nextSID :: SessionID -> SessionID
nextSID (SessionID n) = SessionID $ if n == maxBound then 1 else succ n

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
  , company          :: Maybe CompanyID   -- Drop this with load balancing task. We don't support loging as company anymore.
  , magichashes      :: M.Map SignatoryLinkID MagicHash
  , padUserID        :: Maybe UserID      -- Other version of login - for pad devices, when you don't want to give control over a device.
  } deriving (Ord, Eq, Show, Typeable)
$(deriveSafeCopy 0 'base ''SessionData)

-- | 'Session' data as we keep it in our database
data Session = Session {
    sessionID   :: SessionID
  , sessionData :: SessionData
  } deriving (Ord, Eq, Show, Typeable)
$(deriveSafeCopy 0 'base ''Session)

data Sessions = Sessions {
    nextSessionID :: SessionID
  , sessions      :: IxSet Session
  }
$(deriveSafeCopy 0 'base ''Sessions)

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

initialSessions :: Sessions
initialSessions = Sessions {
    nextSessionID = SessionID 1
  , sessions = I.empty
  }

-- | Get the session data associated with the supplied 'SessionID'.
getSession :: SessionID -> Query Sessions (Maybe Session)
getSession sid = return . getOne . (@= sid) =<< sessions `fmap` ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByUserID :: UserID -> Query Sessions [Session]
getSessionsByUserID uid = return . toList . (@= uid) =<< sessions `fmap` ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionsByCompanyID :: CompanyID -> Query Sessions [Session]
getSessionsByCompanyID cid = return . toList . (@= cid) =<< sessions `fmap` ask

-- | Update the 'Session'.
updateSession :: Session -> Update Sessions ()
updateSession session@Session{sessionID} = modify $
  \s -> s { sessions = updateIx sessionID session $ sessions s }

-- | Delete the session associated with the 'SessionID'.
-- Returns the deleted session.
deleteSession :: SessionID -> Update Sessions (Maybe (Session))
deleteSession sid = do
  msession <- return . getOne . (@= sid) =<< sessions `fmap` get
  case msession of
    Nothing -> return Nothing
    Just session -> do
      modify $ \s -> s { sessions = deleteIx sid $ sessions s }
      return $ Just session

-- | Start a new session with the supplied session data
newSession :: SessionData -> Update Sessions Session
newSession sessData = do
  Sessions{..} <- get
  let session = Session {
          sessionID = nextSessionID
        , sessionData = sessData
        }
  put Sessions {
      nextSessionID = nextSID nextSessionID
    , sessions = insert session sessions
    }
  return session

-- | Drops expired session from database, to be used with scheduler
-- We need to clean db once in a while since sessions are created in db for each sessionless request.
dropExpiredSessions :: MinutesTime -> Update Sessions ()
dropExpiredSessions time = do
  expired <- toList . (@<= time) . sessions <$> get
  modify $ \s -> s { sessions = foldr (\ss acc -> deleteIx (sessionID ss) acc) (sessions s) expired }

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
  addHttpOnlyCookie ishttps (MaxAge (60*60*24)) $ mkCookie "sessionId" $ show $ cookieInfoFromSession session
  addCookie ishttps (MaxAge (60*60*24)) $ mkCookie "xtoken" $ show $ xtoken $ sessionData session

-- | Read current session cookie from request.
currentSessionInfoCookie :: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = optional (readCookieValue "sessionId")

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
    , magichashes = M.empty
    , padUserID = Nothing
    }

-- | Check if session data is empty
isSessionDataEmpty :: SessionData -> Bool
isSessionDataEmpty SessionData{..} =
     userID == Nothing
  && padUserID == Nothing
  && Prelude.null elegtransactions && location == "" && company == Nothing
  && M.null magichashes

-- | Return ID for temporary session (not written to db)
tempSessionID :: SessionID
tempSessionID = SessionID 0

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
updateSessionWithContextData :: (FilterMonad Response m, HasAcidState Sessions m, ServerMonad m, MonadDB m)
                             => Session
                             -> Maybe UserID
                             -> [ELegTransaction]
                             -> M.Map SignatoryLinkID MagicHash
                             -> Maybe UserID
                             -> m ()
updateSessionWithContextData (Session i sd) u trans magichashes' pu = do
  now <- getMinutesTime
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
      update (NewSession newsd) >>= startSessionCookie
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
getMagicHashes :: Session -> M.Map SignatoryLinkID MagicHash
getMagicHashes = magichashes . sessionData

-- | Get the xtoken from the session data
getSessionXToken :: Session -> MagicHash
getSessionXToken = xtoken . sessionData
