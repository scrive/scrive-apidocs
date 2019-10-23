{-# OPTIONS_GHC #-}

module Utils.HTTP (
    isHTTPS
  , isSecure
  , currentDomain
  , getHttpHostpart
  , getHttpsHostpart
  , getHostpart
  , currentLink
  , currentLinkBody
  , urlEncodeVars
  ) where

import Happstack.Server
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as URI

currentDomain :: ServerMonad m => m Text
currentDomain = do
  rq <- askRq
  return $ maybe "scrive.com" TE.decodeUtf8 $ getHeader "host" rq

currentScheme :: ServerMonad m => m Text
currentScheme = do
  rq <- askRq
  return $ maybe "http" TE.decodeUtf8 $ getHeader "scheme" rq

isSecure :: ServerMonad m => m Bool
isSecure = (Just (BS.fromString "http") /=) `liftM` getHeaderM "scheme"

isHTTPS :: ServerMonad m => m Bool
isHTTPS = do
  rq <- askRq
  let mscheme = getHeader "scheme" rq
  return $ mscheme == Just (BS.fromString "https")

getHostpart :: ServerMonad m => m Text
getHostpart = do
  hostpart <- currentDomain
  scheme   <- currentScheme
  return $ scheme <> "://" <> hostpart

getHttpHostpart :: ServerMonad m => m Text
getHttpHostpart = do
  hostpart <- currentDomain
  return $ "http://" <> hostpart

getHttpsHostpart :: ServerMonad m => m Text
getHttpsHostpart = do
  hostpart <- currentDomain
  return $ "https://" <> hostpart

currentLink :: ServerMonad m => m Text -- We use this since we can switch to HTTPS whenever we want
currentLink = do
  scheme  <- currentScheme
  urlbody <- currentLinkBody
  return $ scheme <> "://" <> urlbody


currentLinkBody :: ServerMonad m => m Text
currentLinkBody = do
  rq       <- askRq
  hostpart <- currentDomain
  return $ hostpart <> (T.pack $ rqUri rq) <> (T.pack $ rqQuery rq)

urlEncodeVars :: [(BSLU.ByteString, BSLU.ByteString)] -> BSLU.ByteString
urlEncodeVars ((n, v) : []) =
  (BSL.fromChunks [(URI.urlEncode True $ BSL.toStrict n)])
    `BSL.append` "="
    `BSL.append` (BSL.fromChunks [(URI.urlEncode True $ BSL.toStrict v)])
urlEncodeVars []       = BSL.empty
urlEncodeVars (p : pp) = urlEncodeVars [p] `BSL.append` "&" `BSL.append` urlEncodeVars pp
