module Session.Data (
    SessionID
  , tempSessionID
  , Session(..)
  , emptySession
  , session
  ) where

import Control.Monad.IO.Class
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Scheduler
import Crypto.RNG
import DB
import DB.SQL2
import MinutesTime
import MagicHash
import Session.SessionID
import Session.Tables
import User.UserID

data Session = Session {
    sesID        :: SessionID
  , sesUserID    :: Maybe UserID
  , sesPadUserID :: Maybe UserID
  , sesExpires   :: MinutesTime
  , sesToken     :: MagicHash
  , sesCSRFToken :: MagicHash
  } deriving (Eq, Show, Typeable)

emptySession :: (CryptoRNG m, MonadIO m) => m Session
emptySession = do
  now <- getMinutesTime
  token <- random
  csrf_token <- random
  return Session {
    sesID        = tempSessionID
  , sesUserID    = Nothing
  , sesPadUserID = Nothing
  , sesExpires   = now
  , sesToken     = token
  , sesCSRFToken = csrf_token
}

session :: Action SessionID Session (Maybe UserID, Maybe UserID, MagicHash, MagicHash) Scheduler
session = Action {
    qaTable = tableSessions
  , qaFields = \(uid, puid, token, csrf_token) -> [
      sql "user_id" uid
    , sql "pad_user_id" puid
    , sql "token" token
    , sql "csrf_token" csrf_token
    ]
  , qaSelectFields = ["id", "user_id", "pad_user_id", "expires", "token", "csrf_token"]
  , qaIndexField = "id"
  , qaExpirationDelay = "2 hours"
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \Session{..} -> toSQLCommand $ sqlUpdate "sessions" $ do
      sqlSet "user_id" sesUserID
      sqlSet "pad_user_id" sesPadUserID
      sqlSet "token" sesToken
      sqlSet "csrf_token" sesCSRFToken
      sqlWhereEq (qaIndexField session) sesID
  , qaEvaluateExpired = \Session{sesID} -> do
    _ <- dbUpdate $ DeleteAction session sesID
    return ()
  }
  where
    decoder acc sid user_id pad_user_id expires token csrf_token = Session {
        sesID = sid
      , sesUserID = user_id
      , sesPadUserID = pad_user_id
      , sesExpires = expires
      , sesToken = token
      , sesCSRFToken = csrf_token
      } : acc
