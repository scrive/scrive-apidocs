{-# OPTIONS_GHC -Werror #-}
module FlashMessage where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Control.Monad.Trans (MonadIO)
import Happstack.Server
import Happstack.Util.Common (readM)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Base64 as B64

import Crypto
import Misc (isHTTPS)
import Session (addHttpOnlyCookie)
import User.UserState (FlashMessage)

updateFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => AESConf -> [FlashMessage] -> [FlashMessage] -> m ()
updateFlashCookie aesconf oldflashes newflashes =
    if null oldflashes && not (null newflashes)
       then addFlashCookie $ toCookieValue aesconf newflashes
       else
           if not (null oldflashes) && null newflashes
              then removeFlashCookie
              else return ()

toCookieValue :: AESConf -> [FlashMessage] -> String
toCookieValue conf flashes =
    BS.unpack . B64.encode . aesEncrypt conf . BSU.fromString $ show flashes

fromCookieValue :: AESConf -> String -> Maybe [FlashMessage]
fromCookieValue conf flashesdata = do
    case B64.decode $ BS.pack flashesdata of
         Right s -> readM . BSU.toString $ aesDecrypt conf s
         _       -> Nothing

addFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => String -> m ()
addFlashCookie flashesdata = do
    ishttps <- isHTTPS
    addHttpOnlyCookie ishttps (MaxAge $ 60*60*24) $ mkCookie "flashes" $ flashesdata

flashDataFromCookie :: RqData (Maybe String)
flashDataFromCookie = optional $ lookCookieValue "flashes"
    where optional c = (Just <$> c) `mplus` (return Nothing)

removeFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => m ()
removeFlashCookie = do
    ishttps <- isHTTPS
    addHttpOnlyCookie ishttps Expired $ mkCookie "flashes" ""
