

module DB.ExplainAnalyze where

import DB.Core
import DB.Fetcher
import DB.Functions
import DB.SQL2

import DB.SQL

kExplainAnalyze :: (SqlTurnIntoSelect s, MonadDB m) => s -> m String
kExplainAnalyze cmd = do
  kRun_ ("EXPLAIN ANALYZE" <+> toSQLCommand (sqlTurnIntoSelect cmd))
  resultLines <- kFold (flip (:)) []
  return (unlines resultLines)
