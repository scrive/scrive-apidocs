module FlashMessage (
      FlashType(..)
    , FlashMessage(..)
    , unFlashMessage
    , toFlashMsg
    , toModal
    , toFlashTemplate
    , instantiate
    , updateFlashCookie
    , addFlashCookie
    , removeFlashCookie
    , toCookieValue
    , fromCookieValue
    , flashDataFromCookie
    ) where

import Control.Monad.IO.Class
import Happstack.Server
import Happstack.Util.Common (readM)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

import Cookies
import Misc (isHTTPS, optional)
import Templates.Templates
import Templates.TemplatesLoader

data FlashType
    = SigningRelated
    | OperationDone
    | OperationFailed
    | Modal
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

data FlashMessage
    = FlashMessage FlashType String
    | FlashTemplate FlashType String [(String, String)]
    deriving (Eq, Ord, Read, Show)

toFlashMsg :: FlashType -> String -> FlashMessage
toFlashMsg = FlashMessage

toModal :: String -> FlashMessage
toModal = FlashMessage Modal

toFlashTemplate :: FlashType -> String -> [(String, String)] -> FlashMessage
toFlashTemplate = FlashTemplate

unFlashMessage :: FlashMessage -> Maybe (FlashType, String)
unFlashMessage (FlashMessage ftype msg) = Just (ftype, msg)
unFlashMessage _ = Nothing

instantiate :: TemplatesMonad m => FlashMessage -> m FlashMessage
instantiate (FlashTemplate ftype templatename fields) = do
  ts <- getTemplates
  return $ toFlashMsg ftype $ renderTemplateMain ts templatename fields id
instantiate fm = return fm

updateFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => [FlashMessage] -> [FlashMessage] -> m ()
updateFlashCookie oldflashes newflashes =
    if null oldflashes && not (null newflashes)
       then addFlashCookie $ toCookieValue newflashes
       else
           if not (null oldflashes) && null newflashes
              then removeFlashCookie
              else return ()

toCookieValue :: [FlashMessage] -> String
toCookieValue flashes =
    BS.unpack . B64.encode . BS.concat
    . BSL.toChunks . GZip.compress . BSLU.fromString $ show flashes

fromCookieValue :: String -> Maybe [FlashMessage]
fromCookieValue flashesdata = do
    case B64.decode $ BS.pack flashesdata of
         Right s -> readM $ BSLU.toString $ GZip.decompress $ BSL.fromChunks [s]
         _       -> Nothing

addFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => String -> m ()
addFlashCookie flashesdata = do
    ishttps <- isHTTPS
    addHttpOnlyCookie ishttps (MaxAge $ 60*60*24) $ mkCookie "flashes" $ flashesdata

flashDataFromCookie :: RqData (Maybe String)
flashDataFromCookie = optional $ lookCookieValue "flashes"

removeFlashCookie :: (FilterMonad Response m, ServerMonad m, MonadIO m, Functor m) => m ()
removeFlashCookie = do
    ishttps <- isHTTPS
    addHttpOnlyCookie ishttps Expired $ mkCookie "flashes" ""
