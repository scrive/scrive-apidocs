module Flow.Server.Cookies where

import Data.Binary.Builder
import Servant.API
import Web.Cookie
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Auth.Session
import Auth.Session.Constant
import Flow.Routes.Types (Host)

newtype Cookies' = Cookies' { fromCookies' :: Cookies }
  deriving Show

instance FromHttpApiData Cookies' where
  parseHeader     = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . T.encodeUtf8

instance ToHttpApiData Cookies' where
  toUrlPiece (Cookies' c) =
    T.decodeUtf8 . BSL.toStrict . toLazyByteString $ renderCookies c

readCookie :: Read a => Text -> Cookies -> Maybe a
readCookie name cookies = do -- Maybe
  cookie <-
    T.dropWhile (== '"')
    .   T.dropWhileEnd (== '"')
    .   T.decodeLatin1
    <$> lookup (T.encodeUtf8 name) cookies
  maybeRead cookie

setCookieOptions :: Bool -> SetCookie
setCookieOptions secure = def
  { setCookiePath   = Just "/"
  , setCookieMaxAge = Just $ fromIntegral maxSessionTimeoutSecs
  , setCookieSecure = secure
  }

setCookieSessionID :: SetCookie -> SessionCookieInfo -> SetCookie
setCookieSessionID options sessionCookieInfo = options
  { setCookieName     = T.encodeUtf8 cookieNameSessionID
  , setCookieValue    = BS.pack $ "\"" <> show sessionCookieInfo <> "\""
  , setCookieHttpOnly = True
  }

setCookieXToken :: SetCookie -> XToken -> SetCookie
setCookieXToken options xtoken = options
  { setCookieName  = T.encodeUtf8 cookieNameXToken
  , setCookieValue = BS.pack $ "\"" <> show xtoken <> "\""
  }

addAuthCookieHeaders
  :: ( AddHeader "Set-Cookie" SetCookie orig1 c
     , AddHeader "Set-Cookie" SetCookie orig2 orig1
     )
  => Bool
  -> (SessionCookieInfo, XToken)
  -> orig2
  -> c
addAuthCookieHeaders secure (sessionCookieInfo, xtoken) =
  addHeader (setCookieSessionID options sessionCookieInfo)
    . addHeader (setCookieXToken options xtoken)
  where options = setCookieOptions secure

cookieDomain :: Maybe Host -> Text
cookieDomain = fromMaybe "scrive.com"



