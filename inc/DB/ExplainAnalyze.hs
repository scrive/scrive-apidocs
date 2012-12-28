

module DB.ExplainAnalyze where

import DB.Fetcher
import DB.Core
import DB.Env
import DB.SQL2
import DB.SQL
import DB.Functions

kExplainAnalyze :: (SqlTurnIntoSelect s, MonadDB m) => s -> DBEnv m String
kExplainAnalyze cmd = do
  kRun_ ("EXPLAIN ANALYZE" <+> toSQLCommand (sqlTurnIntoSelect cmd))
  resultLines <- foldDB (flip (:)) []
  return (unlines resultLines)
