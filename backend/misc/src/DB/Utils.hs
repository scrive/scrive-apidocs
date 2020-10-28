module DB.Utils
  ( explainAnalyze
  , throwDBExtra
  ) where

import Control.Monad.Catch
import Data.String
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.SQL.Builder

explainAnalyze :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m) => sql -> m String
explainAnalyze sql = do
  runQuery_ $ "EXPLAIN ANALYZE VERBOSE" <+> sql
  unlines <$> fetchMany runIdentity

throwDBExtra :: (MonadThrow m, DBExtraException e) => e -> m a
throwDBExtra = throwM . SomeDBExtraException
