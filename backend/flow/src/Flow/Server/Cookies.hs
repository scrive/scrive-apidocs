{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Server.Cookies where

import Servant.API
import Web.Cookie
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Auth.Session
import Auth.Session.Constant
import Flow.Routes.Types (Host)

newtype Cookies' = Cookies' Cookies -- type Cookies = [(BS.ByteString, BS.ByteString)]
  deriving Show

instance FromHttpApiData Cookies' where
  parseHeader     = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . T.encodeUtf8

instance ToHttpApiData Cookies' where
  toUrlPiece = undefined -- TODO: Implement, for use in Client.hs

readAuthCookies :: Cookies -> Maybe AuthCookies
readAuthCookies cookies = do
  sessionCookie <- readCookie cookieNameSessionID cookies
  xtoken        <- readCookie cookieNameXToken cookies
  pure $ AuthCookies sessionCookie xtoken

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
  -> AuthCookies
  -> orig2
  -> c
addAuthCookieHeaders secure AuthCookies {..} =
  addHeader (setCookieSessionID options authCookieSession)
    . addHeader (setCookieXToken options authCookieXToken)
  where options = setCookieOptions secure

cookieDomain :: Maybe Host -> Text
cookieDomain = fromMaybe "scrive.com"



