{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Session 
    ( -- * Basic Types
      SessionData
    , SessionId(..)
    , Session(..)
    , Sessions
      -- * State functions
    , GetSession(..)
    , UpdateSession(..)
    , DelSession(..)
    , NewSession(..)
    , withSessionId
    , withSessionData
    , withSessionDataSP
    , withMSessionId
    , withMSessionData
    , withMSessionDataSP
    , withMSessionDataSP2
    , startSession  
    , endSession
    )
    where

import Control.Applicative ((<$>),optional)
import Control.Applicative hiding (empty)
import Control.Monad.Reader (MonadPlus(..), ap, ReaderT(..), asks, ask)
import Control.Monad.State hiding (State)
import Data.Generics
import Data.Maybe (isNothing)
import Happstack.Data (Default, deriveAll, gFind')
import Happstack.Data.IxSet
import Happstack.Data.IxSet (IxSet(..), Indexable(..), (@=), delete, getOne, inferIxSet, noCalcs, updateIx)
import Happstack.Server (ServerMonad, withDataFn, readCookieValue,addCookie,FilterMonad(..),Response,expireCookie,setHeaderM)
import Happstack.Server.Cookie (Cookie,mkCookie,mkCookieHeader)
import Happstack.Server.HTTP.Types ()
import Happstack.State (Serialize, Version, Query, Update, deriveSerialize, getRandomR, mkMethods, query)
import qualified Control.Applicative as Applicative
import qualified Data.Set as Set

-- |perform insert only if test is True
testAndInsert :: (Indexable a b,
                  Ord a,
                  Data a,
                  MonadState (IxSet a) m) =>
                 (IxSet a -> Bool) -> a -> m Bool
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

-- * this should go in Data.Map
{-
insertIfNew :: (Ord k) => k -> a -> M.Map k a -> Maybe (M.Map k a)
insertIfNew k v m =
    case M.insertLookupWithKey (\_ _ oldValue -> oldValue) k v m of
      (Nothing, m') -> Just m'
      (Just _, _) -> return m
-}

class (Ord s, Serialize s, Data s, Default s) => SessionData s

$( deriveAll [''Ord, ''Eq, ''Default, ''Num]
   [d|
       newtype SessionId = SessionId Integer
    |])

instance Show SessionId where
    showsPrec prec (SessionId v) = showsPrec prec v

instance Read SessionId where
    readsPrec prec string = 
        [(SessionId k,v) | (k,v) <- readsPrec prec string]

$( deriveAll [''Ord, ''Eq, ''Read, ''Show, ''Default]
   [d|
       data Session a = Session SessionId a
    |])

$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

$(deriveSerialize ''Session)
instance (Version a) => Version (Session a)
$(deriveSerialize ''SessionId)
instance Version SessionId

-- |get the session data associated with the supplied SessionId
getSession :: (Data a, Ord a) => SessionId -> Query (Sessions a) (Maybe (Session a))
getSession sessionId = (return . getOne . (@= (sessionId :: SessionId))) =<< ask

-- |update the Session
updateSession :: (Data a, Ord a) => (Session a) -> Update (Sessions a) ()
updateSession session = modify (updateIx (gFind' session :: SessionId) session)

-- |delete the session associated with the sessionId
-- returns the deleted session (mostly because we need some place to disambiguate the session type)
delSession :: (Data a, Ord a) => SessionId -> Update (Sessions a) (Maybe (Session a))
delSession sessionId =
    do mSession <- (return . getOne . (@= (sessionId :: SessionId))) =<< get
       case mSession of
         Nothing -> return Nothing
         (Just session) -> 
             do modify (delete session)
                return (Just session)

-- |start a new session with the supplied session data
-- returns: the SessionId
newSession :: (Data a, Serialize a, Ord a) => a -> Update (Sessions a) SessionId
newSession sessData =
    do sessId <- SessionId <$> getRandomR (0,1000000000)
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

withSessionId :: (MonadPlus m, ServerMonad m) => (SessionId -> m a) -> m a
withSessionId = withDataFn (readCookieValue "sessionId")

withSessionData :: (Ord a, Serialize a, Data a, MonadIO m, MonadPlus m) => SessionId -> (a -> m r) -> m r
withSessionData sID f =
    do mSessionData <- query (GetSession sID)
       case mSessionData of
         Nothing -> mzero
         (Just (Session _ sessionData)) ->
             f sessionData

withSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m, MonadPlus m) => SessionId -> (a -> m r) -> m r
withSessionDataSP' sID f =
       do mSessionData <- query (GetSession sID)
          case mSessionData of
            Nothing -> mzero
            (Just (Session _ sessionData)) ->
                f sessionData

withSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m, ServerMonad m, MonadPlus m) => (a -> m r) -> m r
withSessionDataSP f = withSessionId (\sID -> withSessionDataSP' sID f)

withMSessionId :: (ServerMonad m, MonadPlus m) => (Maybe SessionId -> m r) -> m r
withMSessionId f = withDataFn (optional (readCookieValue "sessionId")) $ \mSid -> f mSid

withMSessionData :: (Ord a, Serialize a, Data a, MonadIO m) => SessionId -> (Maybe a -> m r) -> m r
withMSessionData sID f =
    do mSessionData <- query (GetSession sID)
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just sessionData)

withMSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m) => Maybe SessionId -> (Maybe a -> m r) -> m r
withMSessionDataSP' Nothing f = f Nothing
withMSessionDataSP' (Just sID) f =
    do mSessionData <- query . GetSession $ sID
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just sessionData)

withMSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m, ServerMonad m, MonadPlus m) => (Maybe a -> m r) -> m r
withMSessionDataSP f =
    withMSessionId (\sID -> withMSessionDataSP' sID f)


withMSessionDataSP2' :: (Ord a, Serialize a, Data a, MonadIO m) => Maybe SessionId -> (Maybe (SessionId,a) -> m r) -> m r
withMSessionDataSP2' Nothing f = f Nothing
withMSessionDataSP2' (Just sID) f =
    do mSessionData <- query . GetSession $ sID
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just (sID,sessionData))

withMSessionDataSP2 :: (Ord a, Serialize a, Data a, MonadIO m, ServerMonad m, MonadPlus m) => (Maybe (SessionId,a) -> m r) -> m r
withMSessionDataSP2 f =
    withMSessionId (\sID -> withMSessionDataSP2' sID f)

-- * Simple Applicative and Alternative instances for RqData via ReaderT
instance (Monad m) => Applicative (ReaderT r m) where
    pure = return
    (<*>) = ap

instance (MonadPlus m) => Alternative (ReaderT r m) where
    empty = unwrapMonad Applicative.empty
    f <|> g = unwrapMonad $ (WrapMonad f) <|> (WrapMonad g)

{-
instance (ServerMonad m) => ServerMonad (XMLGenT m) where
    askRq   = XMLGenT askRq
    localRq f (XMLGenT m) = XMLGenT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (XMLGenT m) where
    setFilter = XMLGenT . setFilter
    composeFilter f = XMLGenT (composeFilter f)
    getFilter (XMLGenT m) = XMLGenT (getFilter m)

instance (WebMonad a m) => WebMonad a (XMLGenT m) where
    finishWith r = XMLGenT $ finishWith r      
-}