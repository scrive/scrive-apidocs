module Util.FlashUtil (
      module FlashMessage
    , Flashable(..)
    , FlashableMonad(..)
    ) where

import Control.Monad.IO.Class

import KontraMonad
import FlashMessage
import Context

class Flashable a m where
    addFlash :: a -> m ()

instance KontraMonad m => Flashable FlashMessage m where
    addFlash = addFlashMsg

instance KontraMonad m => Flashable (FlashType, String) m where
    addFlash = addFlashMsg . uncurry toFlashMsg

instance (MonadIO m, KontraMonad m) => Flashable (IO FlashMessage) m where
    addFlash fm = liftIO fm >>= addFlashMsg

------------------------------------------------------------------

class FlashableMonad a where
    addFlashM :: KontraMonad m => m a -> m ()

instance FlashableMonad FlashMessage where
    addFlashM fm = fm >>= addFlashMsg

instance FlashableMonad (Maybe FlashMessage) where
    addFlashM fm = fm >>= maybe (return ()) addFlashMsg

------------------------------------------------------------------

addFlashMsg :: KontraMonad m => FlashMessage -> m ()
addFlashMsg flash =
    modifyContext $ \ctx@Context{ ctxflashmessages = flashmessages } ->
        ctx { ctxflashmessages = flash : flashmessages }
