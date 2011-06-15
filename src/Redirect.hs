{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module Redirect (sendRedirect,sendSecureLoopBack) where

import Control.Applicative ((<$>))
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Happstack.Server.SimpleHTTP
import Kontra
import KontraLink
import Misc
import User.UserView
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)

seeOtherXML :: String -> Response
seeOtherXML url = toResponseBS (BS.fromString "text/html;charset=utf-8") $ BSL.fromString $ "<a href='"++url++"' alt='303 see other'>"++ url ++ "</a>"

{-|
   Redirects to the url relevant to the KontraLink.
-}
sendRedirect :: KontraLink -> Kontra Response
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
  templates <- ctxtemplates <$> get
  liftIO (flashMessageLoginRedirectReason templates reason) >>= maybe (return ()) addFlashMsg
  let link' = show link ++ "&referer=" ++ (URL.encode . UTF.encode $ fromMaybe curr referer)
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

sendRedirect link = do
 seeOther (show link) =<< setRsCode 303 (seeOtherXML $ show link)

sendSecureLoopBack :: Kontra Response
sendSecureLoopBack = do
    link <- getSecureLink
    seeOther link =<< setRsCode 303 (seeOtherXML link)
