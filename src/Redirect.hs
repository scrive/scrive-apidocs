module Redirect
  ( sendRedirect
  , sendSecureLoopBack
  , redirectKontraResponse
  , guardRightM
  , guardLoggedIn
  , GuardRight(..)
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
import qualified Log
import Happstack.Fields
import Util.FlashUtil
import Utils.HTTP
import DBError
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

sendRedirect BackToReferer = do
  referer <- getField "referer"
  mainlink <- getHomeOrDesignViewLink
  let link' = fromMaybe (show mainlink) referer
  let link  = if (null link') then (show mainlink) else link'
  seeOther link =<< setRsCode 303 (seeOtherXML link)

sendRedirect link@(LinkLogin _lang reason) = do
  curr <- rqUri <$> askRq
  qr <- rqQuery <$> askRq
  referer <- getField "referer"
  addFlashM $ flashMessageLoginRedirectReason reason
  let link' = show link ++ "&referer=" ++ (URL.encode . UTF.encode $ fromMaybe (curr++qr) referer)
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

sendRedirect link = do
 seeOther (show link) =<< setRsCode 303 (seeOtherXML $ show link)

sendSecureLoopBack :: Kontrakcja m => m Response
sendSecureLoopBack = do
  link <- getSecureLink
  seeOther link =<< setRsCode 303 (seeOtherXML link)
  where
    getSecureLink = (++) "https://" <$> currentLinkBody

redirectKontraResponse :: KontraLink -> Kontra Response
redirectKontraResponse link = do
  let linkstr = show link
  seeOther linkstr =<< setRsCode 303 (seeOtherXML linkstr)

-- moved here because of dependency problems

class GuardRight a where
  guardRight :: (Kontrakcja m) => Either a b -> m b

instance GuardRight String where
  guardRight (Right b) = return b
  guardRight (Left  a) = do
    Log.debug a
    internalError

instance GuardRight DBError where
  guardRight (Right b)            = return b
  guardRight (Left DBNotLoggedIn) = do
    ctx <- getContext
    finishWith $ sendRedirect $ LinkLogin (ctxlang ctx) NotLogged
  guardRight _                    = internalError

{- |
   Get the value from a Right or log an error and fail if it is a left
 -}
guardRightM :: (Kontrakcja m, GuardRight msg) => m (Either msg b) -> m b
guardRightM action = guardRight =<< action

guardLoggedIn :: (Kontrakcja m) => m ()
guardLoggedIn = do
  Context{ ctxmaybeuser } <- getContext
  case ctxmaybeuser of
    Nothing -> do
      ctx <- getContext
      finishWith $ sendRedirect $ LinkLogin (ctxlang ctx) NotLogged
    Just _ -> return ()
