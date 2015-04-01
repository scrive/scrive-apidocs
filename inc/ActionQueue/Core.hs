{-# LANGUAGE ExistentialQuantification #-}
module ActionQueue.Core (
    Action(..)
  , GetAction(..)
  , GetExpiredActions(..)
  , NewAction(..)
  , UpdateAction(..)
  , DeleteAction(..)
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Typeable

import DB
import KontraPrelude
import MinutesTime

data Action idx t con n = forall row. FromRow row => Action {
    qaTable           :: Table
  , qaSetFields       :: (MonadState v m, SqlSet v) => con -> m ()
  , qaSelectFields    :: [SQL]
  , qaIndexField      :: SQL
  , qaExpirationDelay :: SQL
  , qaDecode          :: row -> t
  , qaUpdateSQL       :: t -> SQL
  , qaEvaluateExpired :: t -> n ()
  }

data GetAction idx t con n = GetAction (Action idx t con n) idx
instance (Show idx, ToSQL idx, MonadDB m, MonadThrow m, MonadTime m) => DBQuery m (GetAction idx t con n) (Maybe t) where
  query (GetAction Action{..} aid) = do
    -- Updating 'expires' on every access is costly and results in
    -- quite a lot of database races for a single row in database, at
    -- least for user sessions.

    now <- currentTime
    -- We update 'expires' only when less than 90% of
    -- qaExpirationDelay of time left till expire.
    runQuery_ $ "SELECT" <+> sqlConcatComma qaSelectFields
        <+> "FROM" <+> raw (tblName qaTable)
        <+> "WHERE" <+> qaIndexField <+> "=" <?> aid <+> "AND expires >" <?> now
    result <- fetchMaybe qaDecode
    runQuery_ $ "UPDATE" <+> raw (tblName qaTable) <+> ("SET expires = GREATEST(expires," <?> now <+> "+ interval '" <> qaExpirationDelay <> "')")
        <+> "WHERE" <+> qaIndexField <+> "=" <?> aid <+> "AND expires >" <?> now
        <+> "AND (expires -" <?> now <> ") < 0.9 * interval '" <> qaExpirationDelay <> "'"
    return result

data GetExpiredActions idx t con n = GetExpiredActions (Action idx t con n) UTCTime
instance MonadDB m => DBQuery m (GetExpiredActions idx t con n) [t] where
  query (GetExpiredActions Action{..} time) = do
    runQuery_ $ "SELECT" <+> sqlConcatComma qaSelectFields
        <+> "FROM" <+> raw (tblName qaTable) <+> "WHERE expires <=" <?> time
        <+> "FOR UPDATE"
    fetchMany qaDecode

data NewAction idx t con n = NewAction (Action idx t con n) UTCTime con
instance (MonadDB m, MonadThrow m, Typeable t) => DBUpdate m (NewAction idx t con n) t where
  update (NewAction Action{..} expires con) = do
    runQuery_ $ sqlInsert (raw $ tblName qaTable) $ do
      sqlSet "expires" expires
      qaSetFields con
      sqlResult $ sqlConcatComma qaSelectFields
    fetchOne qaDecode

data UpdateAction idx t con n = UpdateAction (Action idx t con n) t
instance (MonadDB m, MonadThrow m) => DBUpdate m (UpdateAction idx t con n) Bool where
  update (UpdateAction Action{..} obj) = runQuery01 $ qaUpdateSQL obj

data DeleteAction idx t con n = DeleteAction (Action idx t con n) idx
instance (Show idx, ToSQL idx, MonadDB m, MonadThrow m) => DBUpdate m (DeleteAction idx t con n) Bool where
  update (DeleteAction Action{..} aid) =
    runQuery01 $ "DELETE FROM" <+> raw (tblName qaTable) <+> "WHERE" <+> qaIndexField <+> "=" <?> aid
