module Utils.Exception where

import Control.Concurrent.Async.Lifted
import Control.Exception.Lifted
import Control.Monad.Trans.Control

-- | Run a monadic action and catch all exceptions it possibly throws. Note that
-- this (correctly) ignores asynchronous exceptions thrown to the caller thread.
tryAny :: MonadBaseControl IO m => m a -> m (Either SomeException a)
tryAny m = withAsync m waitCatch
