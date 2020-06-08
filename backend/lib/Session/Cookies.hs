module Session.Cookies (
    SessionCookieInfo(..)
  , startSessionCookie
  , startSessionCookieWithExpiry
  , stopSessionCookie
  , sessionCookieInfoFromSession
  , currentSessionInfoCookies
  , isXTokenCookieBroken
  , cookieNameSessionID
  , cookieNameXToken
  ) where

import Control.Monad.IO.Class
import Happstack.Server hiding (Session, addCookie)
import qualified Data.Text as T

import Auth.Session
  ( SessionCookieInfo(..), cookieNameSessionID, cookieNameXToken
  )
import Cookies
import Session.Constant
import Session.Types
import Utils.HTTP

mkCookieFromText :: Text -> Text -> Cookie
mkCookieFromText h v = mkCookie (T.unpack h) (T.unpack v)

-- | Add a session cookie to browser.
startSessionCookie
  :: (FilterMonad Response m, ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookie s = do
  ishttps <- isHTTPS
  addHttpOnlyCookie ishttps (MaxAge maxSessionTimeoutSecs)
    . mkCookieFromText cookieNameSessionID
    . showt
    $ sessionCookieInfoFromSession s
  addCookie ishttps (MaxAge maxSessionTimeoutSecs)
    . mkCookieFromText cookieNameXToken
    $ showt (sesCSRFToken s)

-- | Add a session cookie to browser with same expiry as supplied session
startSessionCookieWithExpiry
  :: (FilterMonad Response m, ServerMonad m, MonadIO m) => Session -> m ()
startSessionCookieWithExpiry s = do
  ishttps <- isHTTPS
  addHttpOnlyCookie ishttps (Expires . sesExpires $ s)
    . mkCookieFromText cookieNameSessionID
    . showt
    $ sessionCookieInfoFromSession s
  addCookie ishttps (Expires . sesExpires $ s) . mkCookieFromText cookieNameXToken $ showt
    (sesCSRFToken s)

-- | Remove session cookie from browser.
stopSessionCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m) => m ()
stopSessionCookie = do
  ishttps <- isHTTPS
  addHttpOnlyCookie ishttps (MaxAge 0) $ mkCookieFromText cookieNameSessionID ""
  addCookie ishttps (MaxAge 0) $ mkCookieFromText cookieNameXToken ""

sessionCookieInfoFromSession :: Session -> SessionCookieInfo
sessionCookieInfoFromSession s =
  SessionCookieInfo { cookieSessionID = sesID s, cookieSessionToken = sesToken s }

isXTokenCookieBroken :: (FilterMonad Response m, ServerMonad m, MonadIO m) => m Bool
isXTokenCookieBroken = do
  sidCookie    <- lookCookieValues cookieNameSessionID . rqHeaders <$> askRq
  xtokenCookie <- lookCookieValues cookieNameXToken . rqHeaders <$> askRq
  return $ case (sidCookie, xtokenCookie) of
    (_ : _, _ : _) -> False
    ([]   , []   ) -> False
    _              -> True

-- | Read current session cookie from request.
currentSessionInfoCookies :: ServerMonad m => m [SessionCookieInfo]
currentSessionInfoCookies =
  mapMaybe maybeRead . lookCookieValues cookieNameSessionID . rqHeaders <$> askRq
