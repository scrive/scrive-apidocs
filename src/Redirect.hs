module Redirect
  ( sendRedirect
  , sendSecureLoopBack
  , guardLoggedIn
  ) where

import Happstack.Server hiding (finishWith)
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified Data.ByteString.UTF8 as BS

import Happstack.Fields
import Kontra
import KontraLink
import KontraPrelude
import User.Lang
import User.UserView
import Util.FinishWith
import Util.FlashUtil
import Utils.HTTP

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

sendRedirect (LinkLogin lang reason) = do
  curr <- rqUri <$> askRq
  qr <- rqQuery <$> askRq
  referer <- getField "referer"
  addFlashM $ flashMessageLoginRedirectReason reason
  let link' = "/" ++ (codeFromLang lang) ++  "/enter?referer=" ++ (URL.encode . UTF.encode $ fromMaybe (curr++qr) referer)
  -- NOTE We could add  "#log-in" at the end. But it would overwrite hash that can be there, and hash is not send to server.
  -- So we let frontend take care of that on it's own. And frontend will fetch hash for referer 
  seeOther link' =<< setRsCode 303 (seeOtherXML link')


-- Backward compatibility. Someone could bookmark /login?referer=/d. We will redirect him to /en/enter. We need to make sure to keep original referer.
sendRedirect (LinkLoginDirect lang) = do
  referer <- getField "referer"
  let link' = case referer of
       Just r -> "/" ++ (codeFromLang lang) ++  "/enter?referer=" ++ (URL.encode . UTF.encode $ r)
       Nothing ->  "/" ++ (codeFromLang lang) ++  "/enter"
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
