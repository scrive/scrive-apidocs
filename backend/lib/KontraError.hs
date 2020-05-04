module KontraError where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Text
import Data.Typeable
import GHC.Stack
import Log
import qualified Control.Exception.Lifted as E

data KontraError =
    Respond404
  | RespondWithCustomHttpCode Int Text
  | InternalError [String]
  | LinkInvalid
  deriving (Eq, Show, Typeable)

instance E.Exception KontraError

internalError :: MonadBase IO m => m a
internalError = do
  stack <- liftBase currentCallStack
  E.throwIO (InternalError stack)

respond404 :: MonadThrow m => m a
respond404 = throwM Respond404

respondWithCustomHttpCode :: MonadThrow m => Int -> Text -> m a
respondWithCustomHttpCode code message = throwM $ RespondWithCustomHttpCode code message

badRequest :: MonadThrow m => Text -> m a
badRequest = respondWithCustomHttpCode 400

unauthorized :: MonadThrow m => Text -> m a
unauthorized = respondWithCustomHttpCode 401

respondLinkInvalid :: MonadThrow m => m a
respondLinkInvalid = throwM LinkInvalid

{- | Auxiliary function for configuration errors in code called from
`RoutingTable.staticRoutes`, e.g.,
"ERROR, no configuration for Salesforce". -}
noConfigurationError :: (MonadBase IO m, MonadLog m) => Text -> m a
noConfigurationError field = do
  logAttention_ $ "ERROR, no configuration for " `append` field
  internalError

{- | Auxiliary function for configuration warnings in code called from
`Cron.main`, e.g.,
"WARNING, no configuration for Salesforce". -}
noConfigurationWarning :: (MonadLog m) => Text -> m ()
noConfigurationWarning field =
  logAttention_ $ "WARNING, no configuration for " `append` field
