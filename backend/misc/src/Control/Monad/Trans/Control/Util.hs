module Control.Monad.Trans.Control.Util where

import Control.Monad.Trans.Control

controlT :: (MonadTransControl t, Monad (t m), Monad m) => (Run t -> m (StT t a)) -> t m a
controlT f = liftWith f >>= restoreT . return

liftMask
  :: (Monad (t m), Monad m, MonadTransControl t)
  => (((forall a . m a -> m a) -> m (StT t b)) -> m (StT t b))
  -> (((forall a . t m a -> t m a) -> t m b) -> t m b)
liftMask fmask m =
  controlT $ \run -> fmask $ \release -> run $ m $ \f -> restoreT $ release (run f)
