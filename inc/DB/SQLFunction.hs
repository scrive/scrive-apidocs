module DB.SQLFunction where

import Data.Foldable as F

import DB.Core
import DB.Env
import DB.Functions
import DB.SQL

-- | Basic SQL functions handling. Note that for now is won't work
-- with set of functions that depend on seach other. It also doesn't
-- properly handle removing old version if numer/type of arguments
-- changed. If need arises, it can be extended to work in such cases.

data SQLFunction = SQLFunction {
    sqlFunHeader :: String
  , sqlFunDef    :: SQL
}

define :: MonadDB m => SQLFunction -> DBEnv m ()
define SQLFunction{..} = do
  kRunRaw $ "DROP FUNCTION IF EXISTS " ++ sqlFunHeader
  kRun_ sqlFunDef

defineMany :: (Foldable f, MonadDB m) => f SQLFunction -> DBEnv m ()
defineMany = F.mapM_ define
