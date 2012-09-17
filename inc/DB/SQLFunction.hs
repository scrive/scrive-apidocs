module DB.SQLFunction where

import Data.Foldable as F

import DB.Core
import DB.Env
import DB.Functions
import DB.SQL

-- | Basic SQL functions handling

newtype SQLFunction = SQLFunction { sqlFunDef :: SQL }

define :: MonadDB m => SQLFunction -> DBEnv m ()
define SQLFunction{..} = kRun_ sqlFunDef

defineMany :: (Foldable f, MonadDB m) => f SQLFunction -> DBEnv m ()
defineMany = F.mapM_ define
