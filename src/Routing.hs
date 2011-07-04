-----------------------------------------------------------------------------
-- |
-- Module      :  Routing
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Schema for all pages and posts
-----------------------------------------------------------------------------
module Routing ( hGet0,           hGet1,           hGet2,           hGet3,           hGet4,           hGet5,
                 hPost0,          hPost1,          hPost2,          hPost3,          hPost4,          hPost5,
                 hPostNoXToken0,  hPostNoXToken1,  hPostNoXToken2,  hPostNoXToken3,  hPostNoXToken4,  hPostNoXToken5,
                 hPostAllowHttp0, hPostAllowHttp1, hPostAllowHttp2, hPostAllowHttp3, hPostAllowHttp4, hPostAllowHttp5,
                 hGetAllowHttp0,  hGetAllowHttp1,  hGetAllowHttp2,  hGetAllowHttp3,  hGetAllowHttp4,  hGetAllowHttp5,
                 hGetAjax0,       hGetAjax1,       hGetAjax2,       hGetAjax3,       hGetAjax4,       hGetAjax5,
                 RedirectOrContent, allowHttp,
                 toK0, toK1, toK2, toK3, toK4, toK5
                 )where

import Control.Monad.State
import Control.Monad.IO.Class()
import Data.Functor
import AppView as V
import Data.Maybe
import Happstack.Server hiding (simpleHTTP,host,body)
import KontraLink
import Misc
import Kontra
import qualified User.UserControl as UserControl
import Redirect

type RedirectOrContent = Either KontraLink String

class Post a where
    hPostWrap :: (Kontra Response -> Kontra Response) -> a -> Kontra Response

class Get a where
    hGetWrap :: (Kontra Response -> Kontra Response) -> a -> Kontra Response

class ToResp a where
    toResp:: a -> Kontra Response

instance ToResp Response where
    toResp = return

instance ToResp KontraLink where
    toResp = sendRedirect

instance ToResp String where
    toResp = page . return

instance (ToResp a , ToResp b) => ToResp (Either a b) where
    toResp = either toResp toResp

instance Post (Kontra KontraLink) where
    hPostWrap f a = methodM POST >> f (a >>= toResp)

instance (ToResp a ) => Get (Kontra a) where
    hGetWrap f a = methodM GET >> f (a >>= toResp)

instance (Post r,FromReqURI a) => Post (a -> r) where
    hPostWrap f a =  path $ \s -> hPostWrap f (a s)

instance (Get r,FromReqURI a) => Get (a -> r) where
    hGetWrap f a =  path $ \s -> hGetWrap f (a s)







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

hGetAjax ::(Get a) =>  a -> Kontra Response
hGetAjax = hGetWrap wrapAjax

wrapAjax :: Kontra Response -> Kontra Response
wrapAjax action = (noRedirect action) `mplus` ajaxError -- Soft redirects should be supported here, ask MR

noRedirect::Kontra Response -> Kontra Response
noRedirect action = do
    response <- action
    if (rsCode response /= 303)
       then return response
       else mzero

{- Http and Https checking-}
class IOFunction0 a where
class IOFunction1 a where
class IOFunction2 a where
class IOFunction3 a where
class IOFunction4 a where
class IOFunction5 a where

instance MonadIO m => IOFunction0 (m a) where
instance MonadIO m => IOFunction1 (a -> m b) where
instance MonadIO m => IOFunction2 (a0 -> a1 -> m b) where
instance MonadIO m => IOFunction3 (a0 -> a1 -> a2 -> m b) where
instance MonadIO m => IOFunction4 (a0 -> a1 -> a2 -> a3 -> m b) where
instance MonadIO m => IOFunction5 (a0 -> a1 -> a2 -> a3 -> a4 -> m b) where

hGet0 :: (Get a, IOFunction0 a) =>  a -> Kontra Response
hGet0 = hGet
hGet1 :: (Get a, IOFunction1 a) => a -> Kontra Response
hGet1 = hGet
hGet2 :: (Get a, IOFunction2 a) => a -> Kontra Response
hGet2 = hGet
hGet3 :: (Get a, IOFunction3 a) => a -> Kontra Response
hGet3 = hGet
hGet4 :: (Get a, IOFunction4 a) => a -> Kontra Response
hGet4 = hGet
hGet5 :: (Get a, IOFunction5 a) => a -> Kontra Response
hGet5 = hGet

hGetAjax0 :: (Get a, IOFunction0 a) => a -> Kontra Response
hGetAjax0 = hGetAjax
hGetAjax1 :: (Get a, IOFunction1 a) => a -> Kontra Response
hGetAjax1 = hGetAjax
hGetAjax2 :: (Get a, IOFunction2 a) => a -> Kontra Response
hGetAjax2 = hGetAjax
hGetAjax3 :: (Get a, IOFunction3 a) => a -> Kontra Response
hGetAjax3 = hGetAjax
hGetAjax4 :: (Get a, IOFunction4 a) => a -> Kontra Response
hGetAjax4 = hGetAjax
hGetAjax5 :: (Get a, IOFunction5 a) => a -> Kontra Response
hGetAjax5 = hGetAjax

hPost0 :: (Post a, IOFunction0 a) => a -> Kontra Response
hPost0 =  hPost
hPost1 :: (Post a, IOFunction1 a) => a -> Kontra Response
hPost1 =  hPost
hPost2 :: (Post a, IOFunction2 a) => a -> Kontra Response
hPost2 =  hPost
hPost3 :: (Post a, IOFunction3 a) => a -> Kontra Response
hPost3 =  hPost
hPost4 :: (Post a, IOFunction4 a) => a -> Kontra Response
hPost4 =  hPost
hPost5 :: (Post a, IOFunction5 a) => a -> Kontra Response
hPost5 =  hPost

hGetAllowHttp0 :: (Get a, IOFunction0 a) =>  a -> Kontra Response
hGetAllowHttp0 = hGetAllowHttp
hGetAllowHttp1 :: (Get a, IOFunction1 a) => a -> Kontra Response
hGetAllowHttp1 = hGetAllowHttp
hGetAllowHttp2 :: (Get a, IOFunction2 a) => a -> Kontra Response
hGetAllowHttp2 = hGetAllowHttp
hGetAllowHttp3 :: (Get a, IOFunction3 a) => a -> Kontra Response
hGetAllowHttp3 = hGetAllowHttp
hGetAllowHttp4 :: (Get a, IOFunction4 a) => a -> Kontra Response
hGetAllowHttp4 = hGetAllowHttp
hGetAllowHttp5 :: (Get a, IOFunction5 a) => a -> Kontra Response
hGetAllowHttp5 = hGetAllowHttp

hPostAllowHttp0 :: (Post a, IOFunction0 a) => a -> Kontra Response
hPostAllowHttp0 =  hPostAllowHttp
hPostAllowHttp1 :: (Post a, IOFunction1 a) => a -> Kontra Response
hPostAllowHttp1 =  hPostAllowHttp
hPostAllowHttp2 :: (Post a, IOFunction2 a) => a -> Kontra Response
hPostAllowHttp2 =  hPostAllowHttp
hPostAllowHttp3 :: (Post a, IOFunction3 a) => a -> Kontra Response
hPostAllowHttp3 =  hPostAllowHttp
hPostAllowHttp4 :: (Post a, IOFunction4 a) => a -> Kontra Response
hPostAllowHttp4 =  hPostAllowHttp
hPostAllowHttp5 :: (Post a, IOFunction5 a) => a -> Kontra Response
hPostAllowHttp5 =  hPostAllowHttp

hPostNoXToken0 :: (Post a, IOFunction0 a) => a -> Kontra Response
hPostNoXToken0 =  hPostNoXToken
hPostNoXToken1 :: (Post a, IOFunction1 a) => a -> Kontra Response
hPostNoXToken1 =  hPostNoXToken
hPostNoXToken2 :: (Post a, IOFunction2 a) => a -> Kontra Response
hPostNoXToken2 =  hPostNoXToken
hPostNoXToken3 :: (Post a, IOFunction3 a) => a -> Kontra Response
hPostNoXToken3 =  hPostNoXToken
hPostNoXToken4 :: (Post a, IOFunction4 a) => a -> Kontra Response
hPostNoXToken4 =  hPostNoXToken
hPostNoXToken5 :: (Post a, IOFunction5 a) => a -> Kontra Response
hPostNoXToken5 =  hPostNoXToken



hPost :: (Post a) =>  a -> Kontra Response
hPost = hPostWrap (https . guardXToken)

hGet :: (Get a) =>  a -> Kontra Response
hGet = hGetWrap https

hGetAllowHttp :: (Get a) =>  a -> Kontra Response
hGetAllowHttp =  hGetWrap allowHttp

hPostAllowHttp :: (Post a) =>  a -> Kontra Response
hPostAllowHttp =  hPostWrap allowHttp

hPostNoXToken :: (Post a) =>  a -> Kontra Response
hPostNoXToken =  hPostWrap (https)


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

-- | Enforce functions in Kontrakcja typeclass to Kontra monad since
-- routing demands it and I'm not sure yet how to fix it properly
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
