module Session.Cookies (
    SessionCookieInfo(..)
  , startSessionCookie
  , currentSessionInfoCookies
  ) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Char
import Happstack.Server hiding (Session, addCookie)

import Cookies
import KontraPrelude
import MagicHash
import Session.Data
import Utils.HTTP
import Utils.Read

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
currentSessionInfoCookies :: RqData [SessionCookieInfo]
currentSessionInfoCookies = readCookiesValues "sessionId"

{- IE 10 is sending cookies for both domain and subdomain (scrive.com & nj.scrive.com)
   We need to read them both, since we have no idea which is the right one.

   To protect against overload attack, we limit number of session cookies supported to 10.
-}
readCookiesValues :: (Monad m, HasRqData m,Read a) => String -> m [a]
readCookiesValues name = do
  (_,_, cookiesWithNames) <- askRqEnv
  let cookies = take 10 $ map snd $ filter (\c -> (fst c) == (map toLower name)) cookiesWithNames
  return $ map $fromJust $ filter isJust $ maybeRead <$> cookieValue <$> cookies

