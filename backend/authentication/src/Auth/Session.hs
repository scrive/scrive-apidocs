module Auth.Session where

import Control.Arrow
import Happstack.Server
import TextShow (fromText)
import qualified Data.Text as T

import Auth.MagicHash
import Auth.Session.SessionID

type XToken = MagicHash

-- | Info that we store in the session cookie.
data SessionCookieInfo = SessionCookieInfo
  { cookieSessionID    :: SessionID -- While parsing we depend on it containing just nums
  , cookieSessionToken :: MagicHash -- While parsing we depend on it starting with alpha
  }

instance Show SessionCookieInfo where
  show SessionCookieInfo {..} = show cookieSessionID ++ "-" ++ show cookieSessionToken

instance TextShow SessionCookieInfo where
  showb SessionCookieInfo {..} =
    showb cookieSessionID <> fromText "-" <> showb cookieSessionToken

instance Read SessionCookieInfo where
  readsPrec _ s = do
    let (sid, msh ) :: (String, String) = second (drop 1) $ break (== '-') s
        (sh , rest)                     = splitAt 16 msh
    case SessionCookieInfo <$> maybeRead (T.pack sid) <*> maybeRead (T.pack sh) of
      Just sci -> [(sci, rest)]
      Nothing  -> []

instance FromReqURI SessionCookieInfo where
  fromReqURI = maybeRead . T.pack

cookieNameXToken :: T.Text
cookieNameXToken = "xtoken"

cookieNameSessionID :: T.Text
cookieNameSessionID = "sessionId"

