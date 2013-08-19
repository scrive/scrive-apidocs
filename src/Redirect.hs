module Redirect
  ( sendRedirect
  , sendSecureLoopBack
  , guardLoggedIn
  ) where


import Control.Applicative ((<$>))
import Data.Maybe
import Happstack.Server hiding (finishWith)
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)

import Kontra
import KontraLink
import Happstack.Fields
import Util.FlashUtil
import Utils.HTTP
import User.UserView
import Util.FinishWith

seeOtherXML :: String -> Response
seeOtherXML url = toResponseBS (BS.fromString "text/html;charset=utf-8") $ BSL.fromString $ "<a href='"++url++"' alt='303 see other'>"++ url ++ "</a>"

{-|
   Redirects to the url relevant to the KontraLink.
-}
sendRedirect :: Kontrakcja m => KontraLink -> m Response
sendRedirect LoopBack = do
  referer <- fmap BS.toString <$> getHeaderM "referer"
  mainlink <- getHomeOrDesignViewLink
  let link = fromMaybe (show mainlink) referer
  seeOther link =<< setRsCode 303 (seeOtherXML link)

sendRedirect link@(LinkLogin _lang reason) = do
  curr <- rqUri <$> askRq
  qr <- rqQuery <$> askRq
  referer <- getField "referer"
  addFlashM $ flashMessageLoginRedirectReason reason
  let link' = show link ++ "?referer=" ++ (URL.encode . UTF.encode $ fromMaybe (curr++qr) referer)
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

sendRedirect link = do
 seeOther (show link) =<< setRsCode 303 (seeOtherXML $ show link)

sendSecureLoopBack :: Kontrakcja m => m Response
sendSecureLoopBack = do
  link <- getSecureLink
  seeOther link =<< setRsCode 303 (seeOtherXML link)
  where
    getSecureLink = (++) "https://" <$> currentLinkBody

guardLoggedIn :: (Kontrakcja m) => m ()
guardLoggedIn = do
  Context{ ctxmaybeuser } <- getContext
  case ctxmaybeuser of
    Nothing -> do
      ctx <- getContext
      finishWith $ sendRedirect $ LinkLogin (ctxlang ctx) NotLogged
    Just _ -> return ()
