module ActionQueue.Core where

import Data.List
import Data.Monoid
import Data.Typeable

import DB
import MinutesTime

data Action idx t con n = Action {
    qaTable           :: Table
  , qaFields          :: con -> [ColumnValue]
  , qaSelectFields    :: [String]
  , qaIndexField      :: String
  , qaExpirationDelay :: String
  , qaDecode          :: MonadDB m => DBEnv m [t]
  , qaUpdateSQL       :: t -> SQL
  , qaEvaluateExpired :: t -> n ()
  }

data GetAction idx t con n = GetAction (Action idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBQuery m (GetAction idx t con n) (Maybe t) where
  query (GetAction Action{..} aid) = do
    _ <- kRun $ mconcat [
        SQL ("UPDATE " ++ tblName qaTable ++ " SET expires = GREATEST(expires, now() + interval '" ++ qaExpirationDelay ++ "')") []
      , SQL ("WHERE " ++ qaIndexField ++ " = ? AND expires > now() ") [toSql aid]
      , SQL ("RETURNING " ++ intercalate ", " qaSelectFields) []
      ]
    qaDecode >>= oneObjectReturnedGuard

data GetExpiredActions idx t con n = GetExpiredActions (Action idx t con n)
instance MonadDB m => DBQuery m (GetExpiredActions idx t con n) [t] where
  query (GetExpiredActions Action{..}) = do
    _ <- kRun $ mconcat [
        SQL ("SELECT " ++ intercalate ", " qaSelectFields) []
      , SQL (" FROM " ++ tblName qaTable ++ " WHERE expires <= now()") []
      , SQL " FOR UPDATE" []
      ]
    qaDecode

data NewAction idx t con n = NewAction (Action idx t con n) MinutesTime con
instance (MonadDB m, Typeable t) => DBUpdate m (NewAction idx t con n) t where
  update (NewAction Action{..} expires con) = do
    _ <- kRun $ mkSQL INSERT qaTable (sql "expires" expires : qaFields con)
      `mappend` SQL ("RETURNING " ++ intercalate ", " qaSelectFields) []
    qaDecode >>= exactlyOneObjectReturnedGuard

data UpdateAction idx t con n = UpdateAction (Action idx t con n) t
instance MonadDB m => DBUpdate m (UpdateAction idx t con n) Bool where
  update (UpdateAction Action{..} obj) = kRun01 $ qaUpdateSQL obj

data DeleteAction idx t con n = DeleteAction (Action idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBUpdate m (DeleteAction idx t con n) Bool where
  update (DeleteAction Action{..} aid) =
    kRun01 $ SQL ("DELETE FROM " ++ tblName qaTable ++ " WHERE " ++ qaIndexField ++ " = ?") [toSql aid]
