module Redirect
  ( sendRedirect
  , sendSecureLoopBack
  ) where

import Happstack.Server hiding (finishWith)
import Network.HTTP.Base (urlEncode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Happstack.Fields
import Kontra
import KontraLink
import User.Lang
import Utils.HTTP
import Utils.String

seeOtherXML :: Text -> Response
seeOtherXML url =
  toResponseBS (BS.fromString "text/html;charset=utf-8")
    $  BSL.fromStrict
    $  TE.encodeUtf8
    $  "<a href='"
    <> (escapeString url)
    <> "' alt='303 see other'>"
    <> (escapeString url)
    <> "</a>"

urlEncodeText :: Text -> Text
urlEncodeText = T.pack . urlEncode . T.unpack

{-|
   Redirects to the url relevant to the KontraLink.
-}
sendRedirect :: Kontrakcja m => KontraLink -> m Response
sendRedirect LoopBack = do
  referer  <- fmap BS.toString <$> getHeaderM "referer"
  mainlink <- getHomeOrDesignViewLink
  let link = fromMaybe (show mainlink) referer
  seeOther link =<< setRsCode 303 (seeOtherXML $ T.pack link)

sendRedirect (LinkLogin lang) = do
  curr    <- T.pack <$> rqUri <$> askRq
  qr      <- T.pack <$> rqQuery <$> askRq
  referer <- getField "referer"
  let link' =
        "/"
          <> (codeFromLang lang)
          <> "/enter?referer="
          <> (urlEncodeText $ fromMaybe (curr <> qr) referer)
  -- NOTE We could add  "#log-in" at the end. But it would overwrite hash that can be there, and hash is not send to server.
  -- So we let frontend take care of that on it's own. And frontend will fetch hash for referer
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

-- Backward compatibility. Someone could bookmark /login?referer=/d. We will redirect him to /en/enter. We need to make sure to keep original referer.
sendRedirect (LinkLoginDirect lang) = do
  referer <- getField "referer"
  let link' = case referer of
        Just r  -> "/" <> (codeFromLang lang) <> "/enter?referer=" <> (urlEncodeText r)
        Nothing -> "/" <> (codeFromLang lang) <> "/enter"
  seeOther link' =<< setRsCode 303 (seeOtherXML link')

sendRedirect link@(LinkPermanentRedirect _) = do
  seeOther (show link) =<< setRsCode 301 (seeOtherXML $ showt link)

sendRedirect link = do
  seeOther (show link) =<< setRsCode 303 (seeOtherXML $ showt link)

sendSecureLoopBack :: Kontrakcja m => m Response
sendSecureLoopBack = do
  link <- getSecureLink
  seeOther link =<< setRsCode 303 (seeOtherXML link)
  where getSecureLink = (<>) "https://" <$> currentLinkBody
