module Redirect (sendRedirect,sendSecureLoopBack,redirectKontraResponse,guardRight,guardRightM,guardLoggedIn) where


import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import Happstack.Server.SimpleHTTP
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)

import Kontra
import KontraLink
import qualified AppLogger as Log
import Misc
import Util.FlashUtil
import DBError
import User.UserView

seeOtherXML :: String -> Response
seeOtherXML url = toResponseBS (BS.fromString "text/html;charset=utf-8") $ BSL.fromString $ "<a href='"++url++"' alt='303 see other'>"++ url ++ "</a>"

{-|
   Redirects to the url relevant to the KontraLink.
-}
sendRedirect :: Kontrakcja m => KontraLink -> m Response
sendRedirect LoopBack = do
  referer <- fmap BS.toString <$> getHeaderM "referer"
  let link = fromMaybe (show LinkMain) referer
  seeOther link =<< setRsCode 303 (seeOtherXML link)

sendRedirect BackToReferer = do
  referer <- getField "referer"
  let link' = fromMaybe (show LinkMain) referer
  let link  = if (null link') then (show LinkMain) else link'
  seeOther link =<< setRsCode 303 (seeOtherXML link)

sendRedirect link@(LinkLogin reason) = do
  curr <- rqUri <$> askRq
  referer <- getField "referer"
  addFlashM $ flashMessageLoginRedirectReason reason
  let link' = show link ++ "&referer=" ++ (URL.encode . UTF.encode $ fromMaybe curr referer)
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

sendRedirect link = do
 seeOther (show link) =<< setRsCode 303 (seeOtherXML $ show link)

sendSecureLoopBack :: Kontrakcja m => m Response
sendSecureLoopBack = do
    link <- getSecureLink
    seeOther link =<< setRsCode 303 (seeOtherXML link)

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
    mzero
    
instance GuardRight DBError where
  guardRight (Right b)            = return b
  guardRight (Left DBNotLoggedIn) = do
    r <- sendRedirect $ LinkLogin NotLogged
    finishWith r
  guardRight _                    = mzero
  
{- |
   Get the value from a Right or log an error and mzero if it is a left
 -}
guardRightM :: (Kontrakcja m, GuardRight msg) => m (Either msg b) -> m b
guardRightM action = guardRight =<< action

guardLoggedIn :: (Kontrakcja m) => m ()
guardLoggedIn = do
  Context{ ctxmaybeuser } <- getContext
  case ctxmaybeuser of
    Nothing -> do
      r <- sendRedirect $ LinkLogin NotLogged
      finishWith r
    Just _ -> return ()
