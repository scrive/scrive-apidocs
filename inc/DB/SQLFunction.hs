module DB.SQLFunction where

import Data.Foldable as F
import Database.PostgreSQL.PQTypes

-- | Basic SQL functions handling

newtype SQLFunction = SQLFunction { sqlFunDef :: RawSQL () }

define :: MonadDB m => SQLFunction -> m ()
define SQLFunction{..} = runQuery_ sqlFunDef

defineMany :: (Foldable f, MonadDB m) => f SQLFunction -> m ()
defineMany = F.mapM_ define
