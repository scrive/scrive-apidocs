module ActionQueue.Core where

import Data.List
import Data.Monoid
import Data.Typeable

import DB
import MinutesTime

data QueueAction idx t con n = QueueAction {
    qaTable           :: Table
  , qaFields          :: con -> [ColumnValue]
  , qaSelectFields    :: [String]
  , qaIndexField      :: String
  , qaExpirationDelay :: String
  , qaDecode          :: MonadDB m => DBEnv m [t]
  , qaUpdateSQL       :: t -> SQL
  , qaEvaluateExpired :: t -> n ()
  }

data GetAction idx t con n = GetAction (QueueAction idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBQuery m (GetAction idx t con n) (Maybe t) where
  query (GetAction QueueAction{..} aid) = do
    _ <- kRun $ mconcat [
        SQL ("UPDATE " ++ tblName qaTable ++ " SET expires = GREATEST(expires, now() + interval '" ++ qaExpirationDelay ++ "')") []
      , SQL ("WHERE " ++ qaIndexField ++ " = ? AND expires > now() ") [toSql aid]
      , SQL ("RETURNING " ++ intercalate ", " qaSelectFields) []
      ]
    qaDecode >>= oneObjectReturnedGuard

data GetExpiredActions idx t con n = GetExpiredActions (QueueAction idx t con n)
instance MonadDB m => DBQuery m (GetExpiredActions idx t con n) [t] where
  query (GetExpiredActions QueueAction{..}) = do
    _ <- kRun $ mconcat [
        SQL "SELECT FOR UPDATE " []
      , SQL (intercalate ", " qaSelectFields) []
      , SQL " WHERE expires <= now()" []
      ]
    qaDecode

data NewAction idx t con n = NewAction (QueueAction idx t con n) MinutesTime con
instance (MonadDB m, Typeable t) => DBUpdate m (NewAction idx t con n) t where
  update (NewAction QueueAction{..} expires con) = do
    _ <- kRun $ mkSQL INSERT qaTable (sql "expires" expires : qaFields con)
      `mappend` SQL ("RETURNING " ++ intercalate ", " qaSelectFields) []
    qaDecode >>= exactlyOneObjectReturnedGuard

data UpdateAction idx t con n = UpdateAction (QueueAction idx t con n) t
instance MonadDB m => DBUpdate m (UpdateAction idx t con n) Bool where
  update (UpdateAction QueueAction{..} obj) = kRun01 $ qaUpdateSQL obj

data DeleteAction idx t con n = DeleteAction (QueueAction idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBUpdate m (DeleteAction idx t con n) Bool where
  update (DeleteAction QueueAction{..} aid) =
    kRun01 $ SQL ("DELETE FROM " ++ tblName qaTable ++ " WHERE " ++ qaIndexField ++ " = ?") [toSql aid]
