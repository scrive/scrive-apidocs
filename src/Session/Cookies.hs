module Session.Cookies (
    SessionCookieInfo(..)
  , startSessionCookie
  , currentSessionInfoCookie
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Happstack.Server hiding (Session, addCookie)

import Cookies
import MagicHash
import Utils.HTTP
import Utils.Read
import Session.Data

-- | Info that we store in cookies.
data SessionCookieInfo = SessionCookieInfo {
    cookieSessionID    :: SessionID -- While parsing we depend on it containing just nums
  , cookieSessionToken :: MagicHash -- While parsing we depend on it starting with alpha
  }

instance Show SessionCookieInfo where
  show SessionCookieInfo{..} =
    show cookieSessionID ++ "-" ++ show cookieSessionToken

instance Read SessionCookieInfo where
  readsPrec _ s = do
    let (sid, msh) = second (drop 1) $ break (== '-') s
        (sh, rest) = splitAt 16 msh
    case SessionCookieInfo <$> maybeRead sid <*> maybeRead sh of
      Just sci -> [(sci, rest)]
      Nothing  -> []

instance FromReqURI SessionCookieInfo where
  fromReqURI = maybeRead

-- | Add a session cookie to browser.
startSessionCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m)
                   => Session -> m ()
startSessionCookie Session{..} = do
  ishttps  <- isHTTPS
  addHttpOnlyCookie ishttps (MaxAge (60*60*24)) $
    mkCookie "sessionId" . show $ SessionCookieInfo {
        cookieSessionID = sesID
      , cookieSessionToken = sesToken
      }
  addCookie ishttps (MaxAge (60*60*24)) $
    mkCookie "xtoken" $ show sesCSRFToken

-- | Read current session cookie from request.
currentSessionInfoCookie :: RqData (Maybe SessionCookieInfo)
currentSessionInfoCookie = optional (readCookieValue "sessionId")
