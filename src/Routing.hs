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
               , hPut
               , hPostNoXToken
               , hPostAllowHttp
               , hGetAllowHttp
               , hGetAjax
               , https
               , RedirectOrContent, allowHttp
               , toK0, toK1, toK2, toK3, toK4, toK5, toK6
               , ToResp, toResp
                 )where

import Control.Monad.Error (catchError)
import Data.Functor
import AppView as V
import Data.Maybe
import Happstack.Server(Response, Method(GET, POST, DELETE, PUT), rsCode)
import Happstack.StaticRouting
import KontraError (internalError)
import KontraLink
import Misc
import Kontra
import qualified User.UserControl as UserControl
import Redirect
import Text.JSON

type RedirectOrContent = Either KontraLink String

kpath :: (Path Kontra Kontra' h a) => Method -> (Kontra a -> Kontra b) -> h
      -> Route (Kontra' b)
kpath m h = path m (unKontra . h)

class ToResp a where
    toResp:: a -> Kontra Response

instance ToResp Response where
    toResp = return

instance ToResp (Kontra Response) where
    toResp = id

instance ToResp KontraLink where
    toResp = sendRedirect

instance ToResp String where
    toResp = page . return

instance ToResp JSValue where
    toResp = simpleResponse . encode

instance (ToResp a , ToResp b) => ToResp (Either a b) where
    toResp = either toResp toResp

hPostWrap :: Path Kontra Kontra' a Response => (Kontra Response -> Kontra Response) -> a -> Route (Kontra' Response)
hPostWrap = kpath POST

hGetWrap :: Path Kontra Kontra' a Response => (Kontra Response -> Kontra Response) -> a -> Route (Kontra' Response)
hGetWrap = kpath GET

hDeleteWrap :: Path Kontra Kontra' a Response => (Kontra Response -> Kontra Response) -> a -> Route (Kontra' Response)
hDeleteWrap = kpath DELETE

hPutWrap :: Path Kontra Kontra' a Response => (Kontra Response -> Kontra Response) -> a -> Route (Kontra' Response)
hPutWrap = kpath PUT


{- To change standard string to page-}
page:: Kontra String -> Kontra Response
page pageBody = do
    pb <- pageBody
    ctx <- getContext
    if (isNothing $ ctxservice ctx)
     then renderFromBody TopDocument kontrakcja pb
     else embeddedPage pb





{- Use this to mark that request will try to get data from our service and embed it on our website
   It returns a script that if embeded on site will force redirect to main page
   Ajax request should not contain redirect
-}

hGetAjax :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hGetAjax = hGetWrap wrapAjax

wrapAjax :: Kontra Response -> Kontra Response
wrapAjax action = noRedirect action `catchError`
                  const ajaxError -- Soft redirects should be supported here, ask MR

noRedirect::Kontra Response -> Kontra Response
noRedirect action = do
    response <- action
    if (rsCode response == 303) then internalError
                                else return response

hPost :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hPost = hPostWrap (https . guardXToken)

hGet :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hGet = hGetWrap https

hDelete :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hDelete = hDeleteWrap https

hPut :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hPut = hPutWrap https

hGetAllowHttp :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hGetAllowHttp = hGetWrap allowHttp

hPostAllowHttp :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hPostAllowHttp = hPostWrap allowHttp

hPostNoXToken :: Path Kontra Kontra' a Response => a -> Route (Kontra' Response)
hPostNoXToken = hPostWrap https

https:: Kontra Response -> Kontra Response
https action = do
    secure <- isSecure
    if secure
       then action
       else sendSecureLoopBack


allowHttp:: Kontrakcja m => m Response -> m Response
allowHttp action = do
    secure <- isSecure
    loging <- isFieldSet "logging"
    logged <- isJust <$> ctxmaybeuser <$> getContext
    if (secure || (not $ loging || logged))
       then action
       else sendSecureLoopBack

guardXToken:: Kontra Response -> Kontra Response
guardXToken = (>>) UserControl.guardXToken

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

toK4 :: ToResp r => (a -> b -> c -> d -> Kontra r) -> (a -> b -> c -> d -> Kontra Response)
toK4 m a b c d = m a b c d >>= toResp

toK5 :: ToResp r => (a -> b -> c -> d -> e -> Kontra r) -> (a -> b -> c -> d -> e -> Kontra Response)
toK5 m a b c d e = m a b c d e >>= toResp

toK6 :: ToResp r => (a -> b -> c -> d -> e -> f -> Kontra r) -> (a -> b -> c -> d -> e -> f -> Kontra Response)
toK6 m a b c d e f = m a b c d e f >>= toResp
