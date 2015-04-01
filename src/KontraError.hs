module KontraError where

import Control.Monad.Base
import Data.Typeable
import GHC.Stack
import qualified Control.Exception.Lifted as E

import KontraPrelude

data KontraError =
    Respond404
  | InternalError [String]
  deriving (Show, Typeable)

instance E.Exception KontraError

internalError :: MonadBase IO m => m a
internalError = do
  stack <- liftBase currentCallStack
  E.throwIO (InternalError stack)

respond404 :: MonadBase IO m => m a
respond404 = E.throwIO Respond404

{-# NOINLINE preventTailCallOptimization #-}
preventTailCallOptimization :: (Monad m) => m ()
preventTailCallOptimization = return ()
