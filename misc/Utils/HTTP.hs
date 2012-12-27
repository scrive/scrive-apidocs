module Utils.HTTP where

import Control.Monad
import Data.Char
import Data.List
import Happstack.Server
import qualified Data.ByteString.UTF8 as BS

class URLAble a where
   encodeForURL :: a -> String

isIphone :: ServerMonad m => m Bool
isIphone =  do
  magent <- fmap BS.toString  `liftM` (getHeaderM "User-Agent")
  case magent of
    Nothing -> return False
    Just agent -> return $ "iphone" `isInfixOf` (map toLower agent)

isSecure :: ServerMonad m => m Bool
isSecure = (Just (BS.fromString "http") /=) `liftM` getHeaderM "scheme"

isHTTPS :: ServerMonad m => m Bool
isHTTPS = do
  rq <- askRq
  let mscheme = getHeader "scheme" rq
  return $ mscheme == Just (BS.fromString "https")

getHostpart :: ServerMonad m => m String
getHostpart = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
  return $ scheme ++ "://" ++ hostpart

getHttpHostpart :: ServerMonad m => m String
getHttpHostpart = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  return $ "http://" ++ hostpart

getResourceHostpart :: ServerMonad m => m String
getResourceHostpart = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" (const "https") $ getHeader "scheme" rq
  return $ scheme ++ "://" ++ hostpart

currentLink :: ServerMonad m => m String -- We use this since we can switch to HTTPS whenever we wan't
currentLink = do
  secure <- isHTTPS
  urlbody <- currentLinkBody
  return $ if secure
    then "https://" ++ urlbody
    else "http://"  ++ urlbody

currentLinkBody :: ServerMonad m => m String
currentLinkBody = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  let fixurl a1 a2 = if ("/" `isSuffixOf` a1 && "/" `isPrefixOf` a2)
                     then drop 1 a2
                     else a2
  return $ hostpart ++ fixurl hostpart (rqUri rq) ++ fixurl (rqUri rq) (rqURL rq)
