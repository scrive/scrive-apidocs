module API.V2.MonadUtils (
  -- * API Monad utils
    apiGuardJust
  , apiGuardJustM
  , apiError
  , bindMaybeM
) where

import Control.Monad.Catch

import API.V2.Errors
import DB

apiGuardJustM :: (MonadThrow m) => APIError -> m (Maybe a) -> m a
apiGuardJustM e a = a >>= maybe (apiError e) return

apiGuardJust :: MonadThrow m => APIError -> Maybe a -> m a
apiGuardJust e = maybe (apiError e) return

apiError :: (MonadThrow m) => APIError -> m a
apiError = throwM . SomeDBExtraException

bindMaybeM :: (Monad m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
bindMaybeM mx cont = case mx of
  Just x  -> cont x
  Nothing -> return Nothing
