module KontraError where

import Control.Monad.Error (MonadError, Error, noMsg, throwError)

data KontraError =
    Respond404
  | InternalError
  deriving Show

instance Error KontraError where
  noMsg = InternalError

internalError :: MonadError KontraError m => m a
internalError = throwError InternalError

respond404 :: MonadError KontraError m => m a
respond404 = throwError Respond404
