{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Routing
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Schema for all pages and posts 
-----------------------------------------------------------------------------
module Routing where

import Control.Monad.State
import Data.Functor
import AppView as V
import Data.Maybe
import HSP.XML (cdata)
import Happstack.Server hiding (simpleHTTP,host,body)
import KontraLink
import Misc
import Kontra
import qualified User.UserControl as UserControl
import qualified AppLogger as Log (error, security, debug)
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
    ctx <- get 
    if (isNothing $ ctxservice ctx)
     then renderFromBody TopDocument kontrakcja $ cdata pb
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
    logged <- isJust <$> ctxmaybeuser <$> get
    if (secure || (not $ loging || logged))
       then action
       else sendSecureLoopBack

guardXToken:: Kontra Response -> Kontra Response
guardXToken = (>>) UserControl.guardXToken
