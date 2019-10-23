-----------------------------------------------------------------------------
-- |
-- Module      :  Routing
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Schema for all pages and posts
-----------------------------------------------------------------------------
module Routing ( hGet
               , hGetWrap
               , hPost
               , hDelete
               , hDeleteAllowHttp
               , hPut
               , hPatch
               , hPostNoXToken
               , hPostAllowHttp
               , hPostNoXTokenHttp
               , hGetAllowHttp
               , https
               , allowHttp
               , toK0, toK1, toK2, toK3, toK4, toK5, toK6
               , ToResp(..)
               ) where

import Data.List.Split
import Happstack.Server
  ( Method(DELETE, GET, PATCH, POST, PUT), Response, ToMessage(..) )

import Happstack.StaticRouting
import Log
import Text.JSON
import qualified Data.Aeson as A
import qualified Data.Text as T

import AppView as V
import FlashMessage (addFlashCookie, toCookieValue)
import Happstack.Fields
import InternalResponse
import Kontra
import KontraLink
import Redirect
import Session.Cookies
import Util.CSVUtil
import Util.ZipUtil
import Utils.HTTP

class ToResp a where
    toResp :: a -> Kontra Response

instance ToResp Response where
  toResp = return

instance ToResp (Kontra Response) where
  toResp = id

instance ToResp KontraLink where
  toResp = sendRedirect

instance ToResp String where
  toResp = page . return

instance ToResp Text where
  toResp = page . return . T.unpack

instance ToResp JSValue where
  toResp = simpleJsonResponse

instance ToResp A.Value where
  toResp = simpleAesonResponse

instance ToResp () where
  toResp _ = simpleHtmlResponse ""

instance (ToResp a , ToResp b) => ToResp (Either a b) where
  toResp = either toResp toResp

instance ToResp InternalKontraResponse where
  toResp ikr = do
    case getFlashMessage ikr of
      Just f -> do
        void $ addFlashCookie (toCookieValue f)
        return ()
      Nothing -> return ()
    toResp (eitherify ikr)

instance ToResp CSV where
  toResp = return . toResponse

instance ToResp ZipArchive where
  toResp = return . toResponse

hPostWrap
  :: Path Kontra Kontra a Response
  => (Kontra Response -> Kontra Response)
  -> a
  -> Route (Kontra Response)
hPostWrap = path POST

hGetWrap
  :: Path Kontra Kontra a Response
  => (Kontra Response -> Kontra Response)
  -> a
  -> Route (Kontra Response)
hGetWrap = path GET

hDeleteWrap
  :: Path Kontra Kontra a Response
  => (Kontra Response -> Kontra Response)
  -> a
  -> Route (Kontra Response)
hDeleteWrap = path DELETE

hPutWrap
  :: Path Kontra Kontra a Response
  => (Kontra Response -> Kontra Response)
  -> a
  -> Route (Kontra Response)
hPutWrap = path PUT

hPatchWrap
  :: Path Kontra Kontra a Response
  => (Kontra Response -> Kontra Response)
  -> a
  -> Route (Kontra Response)
hPatchWrap = path PATCH

{- To change standard string to page -}
page :: Kontra String -> Kontra Response
page pageBody = do
  pb <- pageBody
  renderFromBody $ T.pack pb

hPost :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPost = hPostWrap (https . guardXToken)

hGet :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hGet = hGetWrap https

hDelete :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hDelete = hDeleteWrap https

hDeleteAllowHttp :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hDeleteAllowHttp = hDeleteWrap allowHttp

hPut :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPut = hPutWrap https

hPatch :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPatch = hPatchWrap https

hGetAllowHttp :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hGetAllowHttp = hGetWrap allowHttp

hPostAllowHttp :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPostAllowHttp = hPostWrap allowHttp

hPostNoXToken :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPostNoXToken = hPostWrap https

hPostNoXTokenHttp :: Path Kontra Kontra a Response => a -> Route (Kontra Response)
hPostNoXTokenHttp = hPostWrap allowHttp

https :: Kontra Response -> Kontra Response
https action = do
  secure   <- isSecure
  useHttps <- get ctxusehttps <$> getContext
  if secure || not useHttps then action else sendSecureLoopBack

allowHttp :: Kontrakcja m => m Response -> m Response
allowHttp action = action

-- | Use to enforce a specific arity of a handler to make it explicit
-- how requests are routed and convert returned value to Responses
toK0 :: ToResp r => Kontra r -> Kontra Response
toK0 m = m >>= toResp

toK1 :: ToResp r => (a -> Kontra r) -> (a -> Kontra Response)
toK1 m a = m a >>= toResp

toK2 :: ToResp r => (a -> b -> Kontra r) -> (a -> b -> Kontra Response)
toK2 m a b = m a b >>= toResp

toK3 :: ToResp r => (a -> b -> c -> Kontra r) -> (a -> b -> c -> Kontra Response)
toK3 m a b c = m a b c >>= toResp

toK4
  :: ToResp r => (a -> b -> c -> d -> Kontra r) -> (a -> b -> c -> d -> Kontra Response)
toK4 m a b c d = m a b c d >>= toResp

toK5
  :: ToResp r
  => (a -> b -> c -> d -> e -> Kontra r)
  -> (a -> b -> c -> d -> e -> Kontra Response)
toK5 m a b c d e = m a b c d e >>= toResp

toK6
  :: ToResp r
  => (a -> b -> c -> d -> e -> f -> Kontra r)
  -> (a -> b -> c -> d -> e -> f -> Kontra Response)
toK6 m a b c d e f = m a b c d e f >>= toResp

guardXToken :: Kontra Response -> Kontra Response
guardXToken action = do
  ctx <- getContext
  let unQuote          = filter (not . (== '"'))
      tokensFromString = catMaybes . map (maybeRead . T.pack . unQuote) . splitOn ";"
  mxtokenString <- getField cookieNameXToken
  case mxtokenString of
    Just xtokenString
      | get ctxxtoken ctx `elem` tokensFromString (T.unpack xtokenString) -> action
    _ -> do -- Requests authorized by something else then xtoken, can't access session data or change context stuff.
      logInfo "Invalid xtoken value, anonymousing context"
        $ object ["xtoken" .= mxtokenString]
      withAnonymousContext action
