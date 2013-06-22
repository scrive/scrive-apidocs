module DB.Functions (
    kCommit
  , kRollback
  , kClone
  , kFinish
  , kRunRaw
  , kRun_
  , kRun
  , kRun01
  , kRun01_
  , kGetTables
  , kDescribeTable
  ) where

import Control.Monad.State.Strict hiding (state)

import DB.Core
import DB.Exception
import DB.SQL (RawSQL, raw, IsSQL, toSQLCommand)


kRunRaw :: MonadDB m => RawSQL -> m ()
kRunRaw s = kRun_ (raw s)

kRun_ :: (MonadDB m, IsSQL sql) => sql -> m ()
kRun_ presql = kRun presql >> return ()

kRun :: (MonadDB m, IsSQL sql) => sql -> m Integer
kRun presql = do
  let sql' = toSQLCommand presql
  kRunSQL sql'

kRun01 :: (MonadDB m, IsSQL sql) => sql -> m Bool
kRun01 presql = do
  let sql' = toSQLCommand presql
  result <- kRunSQL sql'
  when (result > 1) $
    kThrow TooManyObjects {
        originalQuery = sql'
      , tmoExpected = 1
      , tmoGiven = result
    }
  return $ result == 1

kRun01_ :: (MonadDB m, IsSQL sql) => sql -> m ()
kRun01_ presql = kRun01 presql >> return ()
