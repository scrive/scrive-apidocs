module Session.Types (
    module SessionID
  , Session(..)
  , emptySession
  ) where

import Control.Monad.Catch
import Crypto.RNG
import Data.Typeable
import Happstack.Server hiding (Session)

import DB
import MagicHash
import MinutesTime
import Session.SessionID as SessionID
import User.UserID
import Utils.HTTP

data Session = Session {
    sesID        :: SessionID
  , sesUserID    :: Maybe UserID
  , sesPadUserID :: Maybe UserID
  , sesExpires   :: UTCTime
  , sesToken     :: MagicHash
  , sesCSRFToken :: MagicHash
  , sesDomain    :: Text
  } deriving (Eq, Show, Typeable)

emptySession
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m, ServerMonad m) => m Session
emptySession = do
  now        <- currentTime
  token      <- random
  csrf_token <- random
  domain     <- currentDomain
  return Session { sesID        = SessionID.tempSessionID
                 , sesUserID    = Nothing
                 , sesPadUserID = Nothing
                 , sesExpires   = now
                 , sesToken     = token
                 , sesCSRFToken = csrf_token
                 , sesDomain    = domain
                 }
