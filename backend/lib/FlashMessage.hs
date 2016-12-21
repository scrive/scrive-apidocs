module FlashMessage (
      FlashType(..)
    , FlashMessage(..)
    , toFlashMsg
    , updateFlashCookie
    , addFlashCookie
    , removeFlashCookie
    , toCookieValue
    , fromCookieValue
    , flashDataFromCookie
    ) where

import Control.Monad.IO.Class
import Happstack.Server hiding (lookCookieValue)
import qualified Codec.Compression.GZip as GZip
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Cookies
import KontraPrelude hiding (optional)
import Utils.HTTP

data FlashType
    = OperationDone
    | OperationFailed
      deriving (Eq, Ord, Read, Show)

{-
   There are two flash "types": flash message is old one, it contains
   flash type and its content (message or html code). Flash template,
   however, contains only informations about how we can obtain message/
   html code from it - it contains flash type, template name and list
   of fields that should be filled. It's kinda "lazy" flash message. To
   convert flash template to flash message, we need to call instantiate
   on it. Thus we need to call instantiate on all flashes before rendering
   a page.

   Why all that? Because with storing flashes in cookie we are limited to 4kB
   in size of such flash message/modal and that's not good. However, for bigger
   modals flash template form will be several times smaller than its flash message
   form.

   Rule of thumb is:
   - normally use FlashMessage form (just as in the past)
   - if flash/modal stops rendering, it most likely means it's too big and
     needs to be stored in template form. I'm aware of the fact that this
     is less powerful than using Templates directly (mainly beacuse we are
     limited to String type for fields), but oh well, I couldn't come up
     with anything better
-}

data FlashMessage = FlashMessage {
                          flashType:: FlashType
                        , flashMessage:: String
                    }  deriving (Eq, Ord, Read, Show)

toFlashMsg :: FlashType -> String -> FlashMessage
toFlashMsg = FlashMessage

toCookieValue :: FlashMessage -> String
toCookieValue flash =
    "{\"type\":\"" <> flashtypestr <> "\", \"content\":\"" <> flashMessage flash <> "\"}"
  where
    flashtypestr = flashTypeToStr . flashType $ flash

flashTypeToStr :: FlashType -> String
flashTypeToStr OperationDone   = "success"
flashTypeToStr OperationFailed = "error"

addFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => String -> m ()
addFlashCookie flashesdata = do
    ishttps <- isHTTPS
    addHttpOnlyCookie ishttps (MaxAge $ 60*60*24) $ mkCookie "flashmessage" $ flashesdata
