-- |Simple session support
module Session
    ( Sessions
    , Session
    , SessionId
    , getUserFromSession
    , getCompanyFromSession
    , getLocationFromSession
    , handleSession
    , updateSessionWithContextData
    , getELegTransactions
    , getSessionXToken
    , getMagicHashes

    -- | Functions usefull when we do remember passwords emails
    --, createLongTermSession
    , findSession
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
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe (isNothing,isJust, fromJust)
import Happstack.Data.IxSet
import qualified Happstack.Data.IxSet as IxSet
import Happstack.State
import User.Model
import Numeric
import MinutesTime
import Happstack.Server (RqData, ServerMonad, FilterMonad, Response, mkCookie,
  readCookieValue, withDataFn, ServerPartT, HasRqData, CookieLife(MaxAge), FromReqURI(..))
import System.Random (StdGen, randomR)
import System.Random.CryptoRNG ()
import Crypto.RNG (CryptoRNGState, CryptoRNG, random, inIO)
import Happstack.Util.Common ( readM)
import Misc (mkTypeOf, isSecure, isHTTPS)
import ELegitimation.ELegTransaction
import Data.Typeable
import API.Service.Model
import Cookies
import Company.Model
import DB.Classes
import MagicHash (MagicHash, unsafeMagicHash)
import Util.MonadUtils
import Doc.SignatoryLinkID
import qualified Data.Map as Map
import Misc (optional)
-- | Session ID is a wrapped 'Integer' really
newtype SessionId = SessionId Integer
    deriving (Eq, Ord, Num, Typeable)


instance Show SessionId where
    showsPrec prec (SessionId v) = showsPrec prec v

instance Read SessionId where
    readsPrec _prec string =
        [(SessionId k,v) | (k,v) <- readSigned readDec string]

instance FromReqURI SessionId where
    fromReqURI = readM

$(deriveSerialize ''SessionId)
instance Version SessionId

{------------------------------------------------------------------------------}
-- THIS IS OLD FLASH MESSAGE STRUCTURE, IT'S FOR COMPATIBILITY PURPOSES ONLY.
-- ACTUAL VERSION IS IN FLASHMESSAGE MODULE. DO NOT EXPOSE ANY OF BELOW FUNCTIONS.
-- Below code should be removed as soon as we get rid of old Session instances
-- in Session (we do not serialize flash messages to database anymore).
newtype FlashMessage = FlashMessage (FlashType, String)
  deriving (Eq, Ord, Show, Typeable)

data FlashType
    = SigningRelated
    | OperationDone
    | OperationFailed
    | Modal
      deriving (Eq, Ord, Show, Typeable)

newtype FlashMessage0 = FlashMessage0 BS.ByteString
  deriving Typeable

instance Migrate FlashMessage0 FlashMessage where
    migrate (FlashMessage0 msg) =
        FlashMessage (SigningRelated, BS.toString msg)

instance Version FlashType
instance Version FlashMessage0
instance Version FlashMessage where
    mode = extension 1 (Proxy :: Proxy FlashMessage0)

$(deriveSerializeFor [''FlashMessage0, ''FlashMessage, ''FlashType])
{------------------------------------------------------------------------------}

data SessionData0 = SessionData0 { userID0 :: Maybe UserID,
                                   flashMessages0 :: [FlashMessage]
                                 } deriving (Ord,Eq,Show,Typeable)
$(deriveSerialize ''SessionData0)
instance Version (SessionData0)

data SessionData1 = SessionData1 {
                                  userID1 :: Maybe UserID,
                                  flashMessages1 :: [FlashMessage],
                                  expires1 :: MinutesTime
                               }  deriving (Ord,Eq,Show,Typeable)

$(deriveSerialize ''SessionData1)
instance Version (SessionData1) where
   mode = extension 1 (Proxy :: Proxy SessionData0)

data SessionData2 = SessionData2 {
                                  userID2 :: Maybe UserID,
                                  flashMessages2 :: [FlashMessage],
                                  expires2 :: MinutesTime,
                                  hash2 :: MagicHash
                               }  deriving (Ord,Eq,Show,Typeable)

$(deriveSerialize ''SessionData2)
instance Version (SessionData2) where
   mode = extension 2 (Proxy :: Proxy SessionData1)

data SessionData3 = SessionData3
    { userID3           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , flashMessages3    :: [FlashMessage]    -- ^ 'FlashMessage's that are to be shown on next page
    , expires3          :: MinutesTime       -- ^ when does this session expire
    , hash3             :: MagicHash         -- ^ session security token
    , elegtransactions3 :: [ELegTransaction] -- ^ ELeg transaction stuff
    } deriving (Ord,Eq,Show,Typeable)

data SessionData4 = SessionData4
    { userID4           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , flashMessages4    :: [FlashMessage]    -- ^ 'FlashMessage's that are to be shown on next page
    , expires4          :: MinutesTime       -- ^ when does this session expire
    , hash4             :: MagicHash         -- ^ session security token
    , elegtransactions4 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken4           :: MagicHash         -- ^ Random string to prevent CSRF
    } deriving (Ord,Eq,Show,Typeable)

data SessionData5 = SessionData5
    { userID5           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires5          :: MinutesTime       -- ^ when does this session expire
    , hash5             :: MagicHash         -- ^ session security token
    , elegtransactions5 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken5           :: MagicHash         -- ^ Random string to prevent CSRF
    } deriving (Ord,Eq,Show,Typeable)

-- | SessionData is everything we want to know about a person clicking
-- on the other side of the wire.
data SessionData6 = SessionData6
    { userID6           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires6          :: MinutesTime       -- ^ when does this session expire
    , hash6             :: MagicHash         -- ^ session security token
    , elegtransactions6 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken6           :: MagicHash         -- ^ Random string to prevent CSRF
    , service6          :: Maybe ServiceID   -- ^ Id of the service that we are now working with
    } deriving (Ord,Eq,Show,Typeable)

data SessionData7 = SessionData7
    { userID7           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires7          :: MinutesTime       -- ^ when does this session expire
    , hash7             :: MagicHash         -- ^ session security token
    , elegtransactions7 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken7           :: MagicHash         -- ^ Random string to prevent CSRF
    , service7          :: Maybe (ServiceID,String)   -- ^ Id of the service that we are now working with. Also lint to page that embeds it (hash location hack)
    } deriving (Ord,Eq,Show,Typeable)

data SessionData8 = SessionData8
    { userID8           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires8          :: MinutesTime       -- ^ when does this session expire
    , hash8             :: MagicHash         -- ^ session security token
    , elegtransactions8 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken8           :: MagicHash         -- ^ Random string to prevent CSRF
    , service8          :: Maybe (ServiceID,String)   -- ^ Id of the service that we are now working with. Also lint to page that embeds it (hash location hack)
    , company8        :: Maybe CompanyID
    } deriving (Ord,Eq,Show,Typeable)

data SessionData9 = SessionData9
    { userID9           :: Maybe UserID      -- ^ Just 'UserID' if a person is logged in
    , expires9          :: MinutesTime       -- ^ when does this session expire
    , hash9             :: MagicHash         -- ^ session security token
    , elegtransactions9 :: [ELegTransaction] -- ^ ELeg transaction stuff
    , xtoken9           :: MagicHash         -- ^ Random string to prevent CSRF
    , location9         :: String            -- ^ Id of the service that we are now working with. Also link to page that embeds it (hash location hack)
    , company9          :: Maybe CompanyID
    } deriving (Ord,Eq,Show,Typeable)

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

    } deriving (Ord,Eq,Show,Typeable)

$(deriveSerialize ''SessionData3)
instance Version (SessionData3) where
    mode = extension 3 (Proxy :: Proxy SessionData2)

$(deriveSerialize ''SessionData4)
instance Version (SessionData4) where
    mode = extension 4 (Proxy :: Proxy SessionData3)

$(deriveSerialize ''SessionData5)
instance Version (SessionData5) where
    mode = extension 5 (Proxy :: Proxy SessionData4)

$(deriveSerialize ''SessionData6)
instance Version (SessionData6) where
    mode = extension 6 (Proxy :: Proxy SessionData5)

$(deriveSerialize ''SessionData7)
instance Version (SessionData7) where
    mode = extension 7 (Proxy :: Proxy SessionData6)

$(deriveSerialize ''SessionData8)
instance Version (SessionData8) where
    mode = extension 8 (Proxy :: Proxy SessionData7)

$(deriveSerialize ''SessionData9)
instance Version (SessionData9) where
    mode = extension 9 (Proxy :: Proxy SessionData8)

$(deriveSerialize ''SessionData)
instance Version (SessionData) where
    mode = extension 10 (Proxy :: Proxy SessionData9)


instance Migrate SessionData0 SessionData1 where
    migrate (SessionData0 { userID0 = _
                          , flashMessages0 = _
                          }) =
        SessionData1 { userID1 = Nothing
                     , flashMessages1 = []
                     , expires1 = fromSeconds 0
                     }

instance Migrate SessionData1 SessionData2 where
    migrate (SessionData1 { userID1 = _
                          , flashMessages1 = _
                          , expires1 = _
                          }) =
        SessionData2 { userID2 = Nothing
                     , flashMessages2 = []
                     , expires2 = fromSeconds 0
                     , hash2 = unsafeMagicHash 0
                     }

instance Migrate SessionData2 SessionData3 where
    migrate (SessionData2 { userID2 = _
                          , flashMessages2 = _
                          , expires2 = _
                          }) =
        SessionData3 { userID3 = Nothing
                    , flashMessages3 = []
                    , expires3 = fromSeconds 0
                    , hash3 = unsafeMagicHash 0
                    , elegtransactions3 = []
                    }

instance Migrate SessionData3 SessionData4 where
    migrate (SessionData3 { userID3 = _
                          , flashMessages3 = _
                          , expires3 = _
                          , elegtransactions3 = _
                          }) =
        SessionData4 { userID4           = Nothing
                    , flashMessages4    = []
                    , expires4          = fromSeconds 0
                    , hash4             = unsafeMagicHash 0
                    , elegtransactions4 = []
                    , xtoken4           = unsafeMagicHash 0
                    }

instance Migrate SessionData4 SessionData5 where
    migrate SessionData4{} =
        SessionData5 { userID5           = Nothing
                    , expires5          = fromSeconds 0
                    , hash5             = unsafeMagicHash 0
                    , elegtransactions5 = []
                    , xtoken5           = unsafeMagicHash 0
                    }

instance Migrate SessionData5 SessionData6 where
    migrate SessionData5{} =
        SessionData6 { userID6           = Nothing
                    , expires6          = fromSeconds 0
                    , hash6             = unsafeMagicHash 0
                    , elegtransactions6 = []
                    , xtoken6           = unsafeMagicHash 0
                    , service6          = Nothing
                    }

instance Migrate SessionData6 SessionData7 where
    migrate SessionData6{} =
        SessionData7 { userID7           = Nothing
                    , expires7          = fromSeconds 0
                    , hash7             = unsafeMagicHash 0
                    , elegtransactions7 = []
                    , xtoken7           = unsafeMagicHash 0
                    , service7          = Nothing
                    }

instance Migrate SessionData7 SessionData8 where
    migrate SessionData7{} =
        SessionData8 { userID8           = Nothing
                    , expires8          = fromSeconds 0
                    , hash8             = unsafeMagicHash 0
                    , elegtransactions8 = []
                    , xtoken8           = unsafeMagicHash 0
                    , service8          = Nothing
                    , company8          = Nothing
                    }

instance Migrate SessionData8 SessionData9 where
    migrate SessionData8{} =
        SessionData9 { userID9           = Nothing
                     , expires9          = fromSeconds 0
                     , hash9             = unsafeMagicHash 0
                     , elegtransactions9 = []
                     , xtoken9           = unsafeMagicHash 0
                     , location9         = ""
                     , company9          = Nothing
                     }

instance Migrate SessionData9 SessionData where
    migrate SessionData9 
              { userID9
              , expires9
              , hash9
              , elegtransactions9
              , xtoken9
              , location9
              , company9
              } =
        SessionData { userID           = userID9
                    , expires          = expires9
                    , hash             = hash9
                    , elegtransactions = elegtransactions9
                    , xtoken           = xtoken9
                    , location         = location9
                    , company          = company9
                    , magichashes      = Map.empty
                    }

-- | 'Session' data as we keep it in our database
data Session = Session { sessionId::SessionId
                       , sessionData::SessionData
                       }
               deriving (Ord, Eq, Show)

-- | WARNING: This needs to specify \"Session\" as full data type
-- name.  Do not use the deriving mechanism as that one produces full
-- name with module like \"MyApp.MyModule.Session\" and this is used
-- in database table name.
instance Typeable Session where typeOf _ = mkTypeOf "Session"

$(deriveSerialize ''Session)
instance Version (Session)

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

instance Component (Sessions) where
  type Dependencies (Sessions) = End
  initialValue = IxSet.empty

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
getSessionByUserId :: UserID -> Query Sessions (Maybe (Session))
getSessionByUserId userId = (return . getOne . (@= userId)) =<< ask

-- | Get the session data associated with the supplied 'UserID'.
getSessionByCompanyId :: CompanyID -> Query Sessions (Maybe (Session))
getSessionByCompanyId companyId = (return . getOne . (@= companyId)) =<< ask

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
newSession :: StdGen -> SessionData -> Update Sessions Session
newSession rng sessData =
    do let (sessId,rng') = first SessionId $ randomR (0,1000000000) rng
       let session = (Session sessId sessData)
       r <- testAndInsert (isNothing . getOne . (@= sessId)) session
       if r
          then return (Session sessId sessData)
          else newSession rng' sessData


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
  , 'getSessionByCompanyId
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
startSessionCookie :: (FilterMonad Response m,ServerMonad m, MonadIO m, Functor m) => Session -> m ()
startSessionCookie session = do
    issecure <- isSecure
    ishttps  <- isHTTPS
    when issecure $ do
        addHttpOnlyCookie ishttps (MaxAge (60*60*24)) $ mkCookie "sessionId" $ show $ cookieInfoFromSession session
        addCookie ishttps (MaxAge (60*60*24)) $ mkCookie "xtoken" $ show $ xtoken $ sessionData session

-- | Read current session cookie from request.
currentSessionInfoCookie:: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = (optional (readCookieValue "sessionId"))

-- | Get current session based on cookies set.
currentSession ::(HasRqData m, MonadIO m, ServerMonad m, MonadPlus m, FilterMonad Response m) => m (Maybe Session)
currentSession =
    withDataFn currentSessionInfoCookie $ \mscd ->
        case mscd of
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
                         , expires = 60 `minutesAfter` now
                         , hash = magicHash
                         , elegtransactions = []
                         , xtoken = xhash
                         , location = ""
                         , company = Nothing
                         , magichashes = Map.empty
                         }

-- | Check if session data is empty
isSessionDataEmpty :: SessionData -> Bool
isSessionDataEmpty SessionData{userID, elegtransactions, location, company, magichashes} =
       userID == Nothing
    && Prelude.null elegtransactions && location == "" && company == Nothing
    && Map.null magichashes

-- | Return ID for temporary session (not written to db)
tempSessionID :: SessionId
tempSessionID = SessionId $ -1

-- | Return empty, temporary session
startSession :: (MonadIO m, CryptoRNG m) => m Session
startSession = emptySessionData >>= return . Session tempSessionID

-- | Get 'User' record from database based on userid in session
getUserFromSession :: DBEnv -> Session -> ServerPartT IO (Maybe User)
getUserFromSession dbenv s =
  liftMM (ioRunDB dbenv . dbQuery . GetUserByID) (return $ userID $ sessionData s)

getCompanyFromSession :: DBEnv -> Session -> ServerPartT IO (Maybe Company)
getCompanyFromSession dbenv s =
  liftMM (ioRunDB dbenv . dbQuery . GetCompany) (return $ company $ sessionData s)

getLocationFromSession :: Session -> ServerPartT IO String
getLocationFromSession s = return $ location $ sessionData s


-- | Handles session timeout. Starts new session when old session timed out.
handleSession :: CryptoRNGState -> ServerPartT IO Session
handleSession r = do
    msession <- currentSession
    case msession of
        Just session ->do
                      now <- liftIO getMinutesTime
                      if (now >= (expires $ sessionData $ session))
                          then do
                             -- _ <- update $ DelSession (sessionId session)
                             liftIO $ inIO r $ startSession
                          else return session
        Nothing -> inIO r $ startSession

-- | Updates session data. If session is temporary and new
-- session data is non-empty, register session in the system
-- and add a cookie. Is user loggs in, check whether there is
-- an old session with his userid and throw it away.
updateSessionWithContextData :: StdGen
                             -> Session
                             -> Maybe UserID
                             -> [ELegTransaction]
                             -> Map.Map SignatoryLinkID MagicHash
                             -> ServerPartT IO ()
updateSessionWithContextData rng (Session i sd) u trans magichashes' = do
    now <- liftIO getMinutesTime
    let newsd = sd
                { userID = u
                , expires = 60 `minutesAfter` now
                , elegtransactions = trans
                , magichashes = magichashes'
                }
    if i == tempSessionID && not (isSessionDataEmpty newsd)
       then do
           when (isNothing (userID sd) && isJust u) $ do
               msess <- query $ GetSessionByUserId $ fromJust u
               case msess of
                    Just _sess -> do
                        -- _ <- update $ DelSession $ sessionId sess
                        return ()
                    Nothing   -> return ()
           update (NewSession rng newsd) >>= startSessionCookie
       else update $ UpdateSession (Session i newsd)

-- | This are special sessions used for passwords reminder links. Such links should be carefully.
{-createLongTermSession :: (MonadIO m) =>  UserID -> m Session
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
    -}

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
dropExpiredSessions :: MonadIO m => MinutesTime -> m ()
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
createServiceSession:: (MonadIO m, CryptoRNG m) => Either CompanyID UserID -> String -> m SessionId
createServiceSession userorcompany loc = do
    now <- liftIO getMinutesTime
    moldsession <- case userorcompany of
      Right uid -> query $ GetSessionByUserId uid
      Left  cid -> query $ GetSessionByCompanyId cid
    case moldsession of
      Just s -> if now >= expires (sessionData s)
                then do
                  -- _ <- update $ DelSession $ sessionId s
                  newSession' userorcompany loc
                else return $ sessionId s
      Nothing -> newSession' userorcompany loc

newSession' :: (MonadIO m, CryptoRNG m) => Either CompanyID UserID -> String -> m SessionId
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
loadServiceSession :: (MonadIO m, Functor m, ServerMonad m, FilterMonad Response m) => Either CompanyID UserID -> SessionId -> m Bool
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

