module DB.Utils (
    explainAnalyze
  ) where

import Data.String
import Database.PostgreSQL.PQTypes

explainAnalyze :: (IsSQL sql, IsString sql, Monoid sql, MonadDB m) => sql -> m String
explainAnalyze sql = do
  runQuery_ $ "EXPLAIN ANALYZE VERBOSE" <+> sql
  unlines <$> fetchMany runIdentity
