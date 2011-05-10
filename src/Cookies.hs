{-# OPTIONS_GHC -Wall -Werror #-}
module Cookies where

import Control.Monad.State hiding (State)
import Happstack.Server.Types (addHeader)
import Happstack.Server (Response)
import qualified Happstack.Server.Internal.Cookie as HSI
import qualified Happstack.Server.Internal.Monads as HSI

-- | Add a non-http only cookie
addCookie :: (MonadIO m, HSI.FilterMonad Response m) => Bool -> HSI.CookieLife -> HSI.Cookie -> m ()
addCookie issecure life cookie = do
    l <- liftIO $ HSI.calcLife life
    let secure = if issecure
                    then ";Secure"
                    else ""
    addHeaderM "Set-Cookie" $ HSI.mkCookieHeader l cookie ++ secure
    where
        addHeaderM a v = HSI.composeFilter $ \res -> addHeader a v res

-- | Ripped from Happstack-Server and modified to support HttpOnly cookies
addHttpOnlyCookie :: (MonadIO m, HSI.FilterMonad Response m) => Bool -> HSI.CookieLife -> HSI.Cookie -> m ()
addHttpOnlyCookie issecure life cookie = do
    l <- liftIO $ HSI.calcLife life
    let secure = if issecure
                    then ";Secure"
                    else ""
    (addHeaderM "Set-Cookie") $ HSI.mkCookieHeader l cookie ++ ";HttpOnly" ++ secure
    where
        addHeaderM a v = HSI.composeFilter $ \res-> addHeader a v res
