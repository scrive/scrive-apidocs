{-# OPTIONS_GHC -Wall -F -pgmFtrhsx #-}
module Redirect (sendRedirect,sendSecureLoopBack) where

import Control.Applicative ((<$>))
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Maybe
import HSP
import Happstack.Server.SimpleHTTP
import Happstack.Server.HSP.HTML (webHSP)
import Kontra
import KontraLink
import Misc
import User.UserView
import qualified Codec.Binary.Url as URL
import qualified Codec.Binary.UTF8.String as UTF
import qualified Data.ByteString.UTF8 as BS
import qualified HSX.XMLGenerator as HSX (XML)

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>

{-|
   Redirects to the url relevant to the KontraLink.
-}
sendRedirect :: KontraLink -> Kontra Response
sendRedirect LoopBack = do
  referer <- fmap BS.toString <$> getHeaderM "referer"
  let link = fromMaybe (show LinkMain) referer
  response <- webHSP (seeOtherXML link)
  seeOther link =<< setRsCode 303  response

sendRedirect BackToReferer = do
  referer <- getField "referer"
  let link' = fromMaybe (show LinkMain) referer
  let link  = if (null link') then (show LinkMain) else link'
  response <- webHSP (seeOtherXML link)
  seeOther link =<< setRsCode 303  response

sendRedirect link@(LinkLogin reason) = do
  curr <- rqUri <$> askRq
  referer <- getField "referer"
  templates <- ctxtemplates <$> get
  liftIO (flashMessageLoginRedirectReason templates reason) >>= maybe (return ()) addFlashMsg
  let link' = show link ++ "&referer=" ++ (URL.encode . UTF.encode $ fromMaybe curr referer)
  response <- webHSP (seeOtherXML $ link')
  seeOther link' =<< setRsCode 303  response

sendRedirect link = do
  response <- webHSP (seeOtherXML $ show link)
  seeOther (show link) =<< setRsCode 303  response

sendSecureLoopBack :: Kontra Response
sendSecureLoopBack = do
    link <- getSecureLink
    response <- webHSP (seeOtherXML $ link)
    seeOther link =<< setRsCode 303  response
