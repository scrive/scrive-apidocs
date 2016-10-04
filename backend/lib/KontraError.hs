module KontraError where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Text
import Data.Typeable
import GHC.Stack
import Log
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

respond404 :: MonadThrow m => m a
respond404 = throwM Respond404

{-# NOINLINE preventTailCallOptimization #-}
preventTailCallOptimization :: (Monad m) => m ()
preventTailCallOptimization = return ()

{- | Auxiliary function for configuration errors in code called from
`RoutingTable.staticRoutes`, e.g.,
"ERROR, no configuration for Salesforce". -}
noConfigurationError :: (MonadBase IO m, MonadLog m) => String -> m a
noConfigurationError field = do
  logAttention_ . pack $ "ERROR, no configuration for " ++ field
  internalError

{- | Auxiliary function for configuration warnings in code called from
`Cron.main`, e.g.,
"WARNING, no configuration for Salesforce". -}
noConfigurationWarning :: (MonadLog m) => String -> m ()
noConfigurationWarning field =
  logAttention_ .pack $ "WARNING, no configuration for " ++ field
