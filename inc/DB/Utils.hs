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
-- Prevent it from looping forever by bailing out after 10 tries.
loopOnUniqueViolation :: forall m a. MonadCatch m => m a -> m a
loopOnUniqueViolation action = loop 1
  where
    loop :: Int -> m a
    loop 10 = action
    loop !n = catch action $ \dbe@DBException{..} -> do
      case cast dbeError of
        Just DetailedQueryError{..}
          | qeErrorCode == UniqueViolation -> loop $ n+1
        _ -> throwM dbe
