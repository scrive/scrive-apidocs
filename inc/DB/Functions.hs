{-# LANGUAGE OverloadedStrings #-}
module DB.Functions (
    dbCommit
  , dbRollback
  , dbClone
  , kPrepare
  , kExecute
  , kExecute1
  , kExecute01
  , kExecute1P
  , kFinish
  , kRunRaw
  , kRun_
  , kRun
  , kRun01
  , kGetTables
  , kDescribeTable
  , ColumnValue
  , sql
  , (.=)
  , SQLType(..)
  , mkSQL
  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (state)
import Data.Convertible (Convertible)
import Data.Maybe
import Data.String (fromString)
import Database.HDBC hiding (originalQuery)
import qualified Control.Exception as E
import qualified Database.HDBC as HDBC

import DB.Core
import DB.Env
import DB.Exception
import DB.Nexus
import DB.SQL (RawSQL, SQL(..), raw, unRawSQL, (<+>), (<?>), sqlConcatComma, parenthesize, sqlParam)
import DB.Model (Table(..))

infix 7 .=

dbCommit :: MonadDB m => m ()
dbCommit = getNexus >>= liftIO . commit

dbRollback :: MonadDB m => m ()
dbRollback = getNexus >>= liftIO . rollback

dbClone :: MonadDB m => m Nexus
dbClone = getNexus >>= liftIO . clone

-- | Prepares new SQL query given as string. If there was another
-- query prepared in this monad, it will be finished first.
kPrepare :: MonadDB m => RawSQL -> DBEnv m ()
kPrepare q = do
  kFinish
  withDBEnvSt $ \s -> do
    let sqlquery = SQL q []
    conn <- getNexus
    st <- protIO sqlquery $ prepare conn (unRawSQL q)
    return ((), s { dbStatement = Just st })

-- | Execute recently prepared query. Values given as param serve as
-- positional arguments for SQL query binding. See 'execute' for more
-- details.
kExecute :: MonadDB m => [SqlValue] -> DBEnv m Integer
kExecute values = withDBEnvSt $ \s@DBEnvSt{dbStatement} -> do
  let sqlquery = SQL (getQuery dbStatement) values
  (, s { dbValues = values }) `liftM` case dbStatement of
    Nothing -> liftIO $ E.throwIO NoStatementPrepared
    Just st -> protIO sqlquery $ execute st values

-- | Execute recently prepared query and check if it returned exactly
-- 1 row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1 :: MonadDB m => [SqlValue] -> DBEnv m ()
kExecute1 values = do
  result <- kExecute values
  when (result /= 1) $ withDBEnv $ \DBEnvSt{dbStatement} ->
    liftIO $ E.throwIO TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }

-- | Execute recently prepared query and check if it returned 0 or 1
-- row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute01 :: MonadDB m => [SqlValue] -> DBEnv m Bool
kExecute01 values = do
  result <- kExecute values
  when (result > 1) $ withDBEnv $ \DBEnvSt{dbStatement} ->
    liftIO $ E.throwIO TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return $ result == 1

-- | Execute recently prepared query and check if it returned 1 or
-- more rows affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1P :: MonadDB m => [SqlValue] -> DBEnv m Integer
kExecute1P values = do
  result <- kExecute values
  when (result < 1) $ withDBEnv $ \DBEnvSt{dbStatement} ->
    liftIO $ E.throwIO TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return result

-- | Finish current statement. 'kPrepare' and 'kExecute' call
-- 'kFinish' before they alter query.
kFinish :: MonadDB m => DBEnv m ()
kFinish = withDBEnvSt $ \s@DBEnvSt{..} -> do
  case dbStatement of
    Just st -> (, DBEnvSt { dbStatement = Nothing, dbValues = [] })
      `liftM` protIO (SQL (getQuery dbStatement) dbValues) (finish st)
    Nothing -> return ((), s)

kRunRaw :: MonadDB m => String -> DBEnv m ()
kRunRaw s = withDBEnv $ \_ -> getNexus >>= \c -> liftIO (runRaw c s)

kRun_ :: MonadDB m => SQL -> DBEnv m ()
kRun_ s = kRun s >> return ()

kRun :: MonadDB m => SQL -> DBEnv m Integer
kRun (SQL s values) = kPrepare s >> kExecute values

kRun01 :: MonadDB m => SQL -> DBEnv m Bool
kRun01 (SQL s values) = kPrepare s >> kExecute01 values

kGetTables :: MonadDB m => DBEnv m [String]
kGetTables = withDBEnv $ \_ -> getNexus >>= liftIO . getTables

kDescribeTable :: MonadDB m => RawSQL -> DBEnv m [(String, SqlColDesc)]
kDescribeTable table = withDBEnv $ \_ -> getNexus >>= \c -> liftIO (describeTable c (unRawSQL table))

-- for INSERT/UPDATE statements generation

type ColumnValue = (RawSQL, SqlValue)

sql :: Convertible a SqlValue => RawSQL -> a -> ColumnValue
sql column value = (column, toSql value)

(.=) :: Convertible a SqlValue => RawSQL -> a -> SQL
r .= v = raw r <+> "=" <?> v

data SQLType = INSERT | UPDATE

mkSQL :: SQLType -> Table -> [ColumnValue] -> SQL
mkSQL qtype Table{tblName} values = case qtype of
  INSERT -> "INSERT INTO" <+> raw tblName <+> parenthesize (sqlConcatComma (map raw columns))
        <+> "SELECT" <+> sqlConcatComma (map sqlParam vals)
  UPDATE -> "UPDATE" <+> raw tblName
        <+> "SET" <+> sqlConcatComma (map (uncurry (.=)) values)
  where
    (columns, vals) = unzip values

-- internals

getQuery :: Maybe Statement -> RawSQL
getQuery = fromString . fromMaybe "" . fmap HDBC.originalQuery

-- | Protected 'liftIO'. Properly catches 'SqlError' and converts it
-- to 'DBException'. Adds 'HDBC.originalQuery' that should help a lot.
protIO :: MonadDB m => SQL -> IO a -> m a
protIO s m = do
  conn <- getNexus
  liftIO $ m `E.catch` (\e -> do
        -- for some unknown reason we need to do rollback here
        -- ourselves otherwise something underneath will try to issue
        -- some commands and those will fail with 'transaction
        -- aborted, ignoring commands till the end of the block'
        liftIO $ rollback conn
        liftIO $ E.throwIO $ SQLError s e)
