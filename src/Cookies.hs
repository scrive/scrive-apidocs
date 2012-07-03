module Cookies where

import Control.Monad.IO.Class
import Happstack.Server as HS

-- | Add a non-http only cookie
addCookie :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addCookie issecure life cookie =
  HS.addCookie life $ cookie { secure = issecure, httpOnly = False }

addHttpOnlyCookie :: (MonadIO m, FilterMonad Response m) => Bool -> CookieLife -> Cookie -> m ()
addHttpOnlyCookie issecure life cookie = do
  HS.addCookie life $ cookie { secure = issecure, httpOnly = True }
