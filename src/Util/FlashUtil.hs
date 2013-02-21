module Util.FlashUtil (
      module FlashMessage
    , Flashable(..)
    , addFlashM
    , addFlashMsg
    , flashOperationFailed
    ) where

import KontraMonad
import FlashMessage
import Context

class Flashable a m where
    addFlash :: a -> m ()

instance KontraMonad m => Flashable FlashMessage m where
    addFlash = addFlashMsg

instance KontraMonad m => Flashable (Maybe FlashMessage) m where
    addFlash = maybe (return ()) addFlashMsg

instance KontraMonad m => Flashable (FlashType, String) m where
    addFlash = addFlashMsg . uncurry toFlashMsg

------------------------------------------------------------------

addFlashM :: (Flashable a m, KontraMonad m) => m a -> m ()
addFlashM fm = fm >>= addFlash

------------------------------------------------------------------

addFlashMsg :: KontraMonad m => FlashMessage -> m ()
addFlashMsg flash =
    modifyContext $ \ctx@Context{ ctxflashmessages = flashmessages } ->
        ctx { ctxflashmessages = flash : flashmessages }

flashOperationFailed :: KontraMonad m => String -> m ()
flashOperationFailed msg = addFlash (OperationFailed, msg)
