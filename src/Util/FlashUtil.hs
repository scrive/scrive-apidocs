module Util.FlashUtil (
      module FlashMessage
    , Flashable(..)
    ) where

import KontraMonad
import FlashMessage
import Context

import Control.Monad.IO.Class

class Flashable a m where
    addFlash :: a -> m ()

instance KontraMonad m => Flashable FlashMessage m where
    addFlash = addFlashMsg

instance KontraMonad m => Flashable (FlashType, String) m where
    addFlash = addFlashMsg . uncurry toFlashMsg

instance (MonadIO m, KontraMonad m) => Flashable (IO FlashMessage) m where
    addFlash fm = liftIO fm >>= addFlashMsg

instance (MonadIO m, KontraMonad m) => Flashable (IO (Maybe FlashMessage)) m where
    addFlash fm = liftIO fm >>= maybe (return ()) addFlashMsg

addFlashMsg :: KontraMonad m => FlashMessage -> m ()
addFlashMsg flash =
    modifyContext $ \ctx@Context{ ctxflashmessages = flashmessages } ->
        ctx { ctxflashmessages = flash : flashmessages }
