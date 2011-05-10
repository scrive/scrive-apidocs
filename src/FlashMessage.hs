{-# OPTIONS_GHC -Werror #-}
module FlashMessage where

import Control.Applicative ((<$>))
import Control.Monad (mplus)
import Control.Monad.Trans (MonadIO)
import Data.Typeable
import Happstack.Server
import Happstack.State
import Happstack.Util.Common (readM)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Base64 as B64

import Crypto
import Misc (isHTTPS)
import Cookies

newtype FlashMessage = FlashMessage { unFlashMessage :: (FlashType, String) }
  deriving (Eq, Ord, Read, Show)

data FlashType
    = SigningRelated
    | OperationDone
    | OperationFailed
    | Modal
      deriving (Eq, Ord, Read, Show)

toFlashMsg :: FlashType -> String -> FlashMessage
toFlashMsg type_ msg = FlashMessage (type_, msg)

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
         Right s -> aesDecrypt conf s >>= readM . BSU.toString
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

{------------------------------------------------------------------------------}

-- Below code should be removed as soon as we get rid of old Session instances
-- in Session (we do not serialize flash messages to database anymore).

deriving instance Typeable FlashMessage
deriving instance Typeable FlashType

newtype FlashMessage0 = FlashMessage0 BS.ByteString
  deriving (Eq, Ord, Read, Show, Typeable)

instance Migrate FlashMessage0 FlashMessage where
    migrate (FlashMessage0 msg) =
        toFlashMsg SigningRelated $ BSU.toString msg

instance Version FlashType
instance Version FlashMessage0
instance Version FlashMessage where
    mode = extension 1 (Proxy :: Proxy FlashMessage0)

$(deriveSerializeFor [''FlashMessage0, ''FlashMessage, ''FlashType])
