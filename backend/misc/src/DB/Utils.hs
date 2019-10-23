module DB.Utils (
    explainAnalyze
  , loopOnUniqueViolation
  , onUniqueViolation
  ) where

import Control.Monad.Catch
import Data.String
import Data.Typeable
import Database.PostgreSQL.PQTypes

explainAnalyze :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m) => sql -> m String
explainAnalyze sql = do
  runQuery_ $ "EXPLAIN ANALYZE VERBOSE" <+> sql
  unlines <$> fetchMany runIdentity

-- | Execute monadic action and loop on UniqueViolation exception.
-- Needed for clean execution of cases "try to update a row and insert
-- a new one if it doesn't exist", as they're prone to race condition
-- and may throw UniqueViolation on the insert. Prevent it from
-- looping forever by bailing out after 10 tries.
loopOnUniqueViolation :: forall  m a . (MonadCatch m, MonadDB m) => m a -> m a
loopOnUniqueViolation action = loop 1
  where
    loop :: Int -> m a
    loop 10 = action
    loop !n = catch action $ \dbe@DBException {..} -> do
      case cast dbeError of
        Just DetailedQueryError {..} | qeErrorCode == UniqueViolation -> loop $ n + 1
        _ -> throwDB dbe

-- | Execute monadic action and perform alternative path on
-- UniqueViolation database exception.
onUniqueViolation :: (MonadCatch m, MonadDB m) => m a -> m a -> m a
onUniqueViolation action fallback = catch action $ \dbe@DBException {..} -> do
  case cast dbeError of
    Just DetailedQueryError {..} | qeErrorCode == UniqueViolation -> fallback
    _ -> throwDB dbe
