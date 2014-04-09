module Session.Data (
    SessionID
  , tempSessionID
  , Session(..)
  , emptySession
  , session
  ) where

import Data.Typeable
import ActionQueue.Core
import ActionQueue.Scheduler
import Crypto.RNG
import DB
import MinutesTime
import MagicHash
import Session.SessionID
import Session.Tables
import User.UserID
import Happstack.Server hiding (Session)
import Utils.HTTP

data Session = Session {
    sesID        :: SessionID
  , sesUserID    :: Maybe UserID
  , sesPadUserID :: Maybe UserID
  , sesExpires   :: MinutesTime
  , sesToken     :: MagicHash
  , sesCSRFToken :: MagicHash
  , sesDomain    :: String
  } deriving (Eq, Show, Typeable)

emptySession :: (CryptoRNG m, MonadDB m, ServerMonad m) => m Session
emptySession = do
  now <- getMinutesTime
  token <- random
  csrf_token <- random
  domain <- currentDomain
  return Session {
    sesID        = tempSessionID
  , sesUserID    = Nothing
  , sesPadUserID = Nothing
  , sesExpires   = now
  , sesToken     = token
  , sesCSRFToken = csrf_token
  , sesDomain    = domain
}

session :: Action SessionID Session (Maybe UserID, Maybe UserID, MagicHash, MagicHash, String) Scheduler
session = Action {
    qaTable = tableSessions
  , qaSetFields = \(uid, puid, token, csrf_token, domain) -> do
    sqlSet "user_id" uid
    sqlSet "pad_user_id" puid
    sqlSet "token" token
    sqlSet "csrf_token" csrf_token
    sqlSet "domain" domain
  , qaSelectFields = ["id", "user_id", "pad_user_id", "expires", "token", "csrf_token", "domain"]
  , qaIndexField = "id"
  , qaExpirationDelay = "2 hours"
  , qaDecode = \(sid, user_id, pad_user_id, expires, token, csrf_token, domain) -> Session {
      sesID = sid
    , sesUserID = user_id
    , sesPadUserID = pad_user_id
    , sesExpires = expires
    , sesToken = token
    , sesCSRFToken = csrf_token
    , sesDomain = domain
    }
  , qaUpdateSQL = \Session{..} -> toSQLCommand $ sqlUpdate "sessions" $ do
      sqlSet "user_id" sesUserID
      sqlSet "pad_user_id" sesPadUserID
      sqlSet "token" sesToken
      sqlSet "csrf_token" sesCSRFToken
      sqlSet "domain" sesDomain
      sqlWhereEq (qaIndexField session) sesID
  , qaEvaluateExpired = \Session{sesID} -> do
    _ <- dbUpdate $ DeleteAction session sesID
    return ()
  }
