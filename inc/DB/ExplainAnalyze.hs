

module DB.ExplainAnalyze where

import Prelude

import DB.Core
import DB.Fetcher
import DB.Functions
import DB.SQL
import DB.SQL2

kExplainAnalyze :: (SqlTurnIntoSelect s, MonadDB m) => s -> m String
kExplainAnalyze cmd = do
  kRun_ ("EXPLAIN ANALYZE" <+> toSQLCommand (sqlTurnIntoSelect cmd))
  resultLines <- kFold (flip (:)) []
  return (unlines resultLines)
