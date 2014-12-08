module DB.Utils (
    loopOnUniqueViolation
  ) where

import Control.Monad.Catch
import Data.Typeable
import Database.PostgreSQL.PQTypes

-- | Execute monad action and loop on UniqueViolation exception.
-- Needed for clean execution of cases "try to update a row
-- and insert a new one if it doesn't exist", as they're prone
-- to race condition and may throw UniqueViolation on the insert.
loopOnUniqueViolation :: MonadCatch m => m a -> m a
loopOnUniqueViolation action = catch action $ \dbe@DBException{..} -> do
  case cast dbeError of
    Just DetailedQueryError{..}
      | qeErrorCode == UniqueViolation -> loopOnUniqueViolation action
    _ -> throwM dbe
