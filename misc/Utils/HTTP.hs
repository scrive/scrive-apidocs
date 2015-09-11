{-# OPTIONS_GHC #-}

module Utils.HTTP(
    isHTTPS
  , isSecure
  , currentDomain
  , getHttpHostpart
  , getHostpart
  , currentLink
  , currentLinkBody
  ) where

import Happstack.Server
import qualified Data.ByteString.UTF8 as BS

import KontraPrelude

currentDomain :: ServerMonad m => m String
currentDomain = do
  rq <- askRq
  return $ maybe "scrive.com" BS.toString $ getHeader "host" rq

currentScheme :: ServerMonad m => m String
currentScheme = do
  rq <- askRq
  return $ maybe "http" BS.toString $ getHeader "scheme" rq

isSecure :: ServerMonad m => m Bool
isSecure = (Just (BS.fromString "http") /=) `liftM` getHeaderM "scheme"

isHTTPS :: ServerMonad m => m Bool
isHTTPS = do
  rq <- askRq
  let mscheme = getHeader "scheme" rq
  return $ mscheme == Just (BS.fromString "https")

getHostpart :: ServerMonad m => m String
getHostpart = do
  hostpart <- currentDomain
  scheme <- currentScheme
  return $ scheme ++ "://" ++ hostpart

getHttpHostpart :: ServerMonad m => m String
getHttpHostpart = do
  hostpart <- currentDomain
  return $ "http://" ++ hostpart

currentLink :: ServerMonad m => m String -- We use this since we can switch to HTTPS whenever we want
currentLink = do
  scheme <- currentScheme
  urlbody <- currentLinkBody
  return $ scheme ++ "://" ++ urlbody


currentLinkBody :: ServerMonad m => m String
currentLinkBody = do
  rq <- askRq
  hostpart <- currentDomain
  return $ hostpart ++ (rqUri rq) ++ (rqQuery rq)
