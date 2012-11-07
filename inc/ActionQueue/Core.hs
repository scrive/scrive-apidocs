module ActionQueue.Core (
    Action(..)
  , GetAction(..)
  , GetExpiredActions(..)
  , NewAction(..)
  , UpdateAction(..)
  , DeleteAction(..)
  ) where

import Data.Monoid
import Data.Typeable

import DB
import MinutesTime

data Action idx t con n = Action {
    qaTable           :: Table
  , qaFields          :: con -> [ColumnValue]
  , qaSelectFields    :: [SQL]
  , qaIndexField      :: SQL
  , qaExpirationDelay :: RawSQL
  , qaDecode          :: MonadDB m => DBEnv m [t]
  , qaUpdateSQL       :: t -> SQL
  , qaEvaluateExpired :: t -> n ()
  }

data GetAction idx t con n = GetAction (Action idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBQuery m (GetAction idx t con n) (Maybe t) where
  query (GetAction Action{..} aid) = do
    kRun_ $ "UPDATE" <+> raw (tblName qaTable) <+> ("SET expires = GREATEST(expires, now() + interval '" <> raw qaExpirationDelay <> "')")
        <+> "WHERE" <+> qaIndexField <+> "=" <?> aid <+> "AND expires > now()"
        <+> "RETURNING" <+> sqlConcatComma qaSelectFields
    qaDecode >>= oneObjectReturnedGuard

data GetExpiredActions idx t con n = GetExpiredActions (Action idx t con n) MinutesTime
instance MonadDB m => DBQuery m (GetExpiredActions idx t con n) [t] where
  query (GetExpiredActions Action{..} time) = do
    kRun_ $ "SELECT" <+> sqlConcatComma qaSelectFields
        <+> "FROM" <+> raw (tblName qaTable) <+> "WHERE expires <=" <?> time
        <+> "FOR UPDATE"
    qaDecode

data NewAction idx t con n = NewAction (Action idx t con n) MinutesTime con
instance (MonadDB m, Typeable t) => DBUpdate m (NewAction idx t con n) t where
  update (NewAction Action{..} expires con) = do
    kRun_ $ mkSQL INSERT qaTable (sql "expires" expires : qaFields con)
        <+> "RETURNING" <+> sqlConcatComma qaSelectFields
    qaDecode >>= exactlyOneObjectReturnedGuard

data UpdateAction idx t con n = UpdateAction (Action idx t con n) t
instance MonadDB m => DBUpdate m (UpdateAction idx t con n) Bool where
  update (UpdateAction Action{..} obj) = kRun01 $ qaUpdateSQL obj

data DeleteAction idx t con n = DeleteAction (Action idx t con n) idx
instance (Convertible idx SqlValue, MonadDB m) => DBUpdate m (DeleteAction idx t con n) Bool where
  update (DeleteAction Action{..} aid) =
    kRun01 $ "DELETE FROM" <+> raw (tblName qaTable) <+> "WHERE" <+> qaIndexField <+> "=" <?> aid
