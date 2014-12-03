module KontraError where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Typeable
import GHC.Stack
import qualified Control.Exception.Lifted as E

data KontraError =
    Respond404
  | InternalError [String]
  deriving (Show, Typeable)

instance E.Exception KontraError

internalError :: MonadBase IO m => m a
internalError = do
  stack <- liftBase currentCallStack
  E.throwIO (InternalError stack)

respond404 :: MonadThrow m => m a
respond404 = throwM Respond404

{-# NOINLINE preventTailCallOptimization #-}
preventTailCallOptimization :: (Monad m) => m ()
preventTailCallOptimization = return ()
