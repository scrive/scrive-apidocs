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
               , hPostNoXToken
               , hPostAllowHttp
               , hGetAllowHttp
               , hGetAjax
               , https
               , RedirectOrContent, allowHttp
               , toK0, toK1, toK2, toK3, toK4, toK5
               , ToResp, toResp
                 )where

import Control.Monad.State
import Control.Monad.IO.Class()
import Data.Functor
import AppView as V
import Data.Maybe
import Happstack.Server(Response, Method(GET,POST), rsCode)
import Happstack.StaticRouting
import KontraLink
import Misc
import Kontra
import qualified User.UserControl as UserControl
import Redirect
import Text.JSON

type RedirectOrContent = Either KontraLink String

class ToResp a where
    toResp:: a -> Kontra Response

instance ToResp Response where
    toResp = return

instance ToResp KontraLink where
    toResp = sendRedirect

instance ToResp String where
    toResp = page . return

instance ToResp JSValue where
    toResp = simpleResponse . encode

instance (ToResp a , ToResp b) => ToResp (Either a b) where
    toResp = either toResp toResp

hPostWrap :: (ToResp a, Path Kontra h a b) =>
             (Kontra Response -> Kontra b) -> h -> Route (Kontra b)
hPostWrap f = path POST (f . (>>= toResp))

hGetWrap :: (ToResp a, Path Kontra h a b) =>
             (Kontra Response -> Kontra b) -> h -> Route (Kontra b)
hGetWrap f = path GET (f . (>>= toResp))

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

hGetAjax :: (ToResp a, Path Kontra h a Response) =>
            h -> Route (Kontra Response)
hGetAjax = hGetWrap wrapAjax

wrapAjax :: Kontra Response -> Kontra Response
wrapAjax action = (noRedirect action) `mplus` ajaxError -- Soft redirects should be supported here, ask MR

noRedirect::Kontra Response -> Kontra Response
noRedirect action = do
    response <- action
    if (rsCode response /= 303)
       then return response
       else mzero

hPost :: (ToResp a, Path Kontra h a Response) => h -> Route (Kontra Response)
hPost = hPostWrap (https . guardXToken)

hGet :: (ToResp a, Path Kontra h a Response) => h -> Route (Kontra Response)
hGet = hGetWrap https

hGetAllowHttp :: (ToResp a, Path Kontra h a Response) =>
                 h -> Route (Kontra Response)
hGetAllowHttp = hGetWrap allowHttp

hPostAllowHttp :: (ToResp a, Path Kontra h a Response) =>
                 h -> Route (Kontra Response)
hPostAllowHttp = hPostWrap allowHttp

hPostNoXToken :: (ToResp a, Path Kontra h a Response) =>
                 h -> Route (Kontra Response)
hPostNoXToken = hPostWrap https

https:: Kontra Response -> Kontra Response
https action = do
    secure <- isSecure
    if secure
       then action
       else sendSecureLoopBack


allowHttp:: Kontra Response -> Kontra Response
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
-- how requests are routed
toK0 :: Kontra a -> Kontra a
toK0 = id

toK1 :: (a -> Kontra b) -> (a -> Kontra b)
toK1 = id

toK2 :: (a -> b -> Kontra c) -> (a -> b -> Kontra c)
toK2 = id

toK3 :: (a -> b -> c -> Kontra d) -> (a -> b -> c -> Kontra d)
toK3 = id

toK4 :: (a -> b -> c -> d -> Kontra e) -> (a -> b -> c -> d -> Kontra e)
toK4 = id

toK5 :: (a -> b -> c -> d -> e -> Kontra f) -> (a -> b -> c -> d -> e -> Kontra f)
toK5 = id

