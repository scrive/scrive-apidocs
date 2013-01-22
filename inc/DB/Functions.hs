module DB.Functions (
    kCommit
  , kRollback
  , kClone
  , kFinish
  , kRunRaw
  , kRun_
  , kRun
  , kRun01
  , kGetTables
  , kDescribeTable
  , ColumnValue
  , sql
  , sqlGeneric
  , SQLType(..)
  , mkSQL
  ) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (state)
import Data.Convertible (Convertible)
import Data.Maybe
import Database.HDBC hiding (originalQuery)
import qualified Control.Exception as E
import qualified Database.HDBC as HDBC

import DB.Core
import DB.Env
import DB.Exception
import DB.Nexus
import DB.SQL (RawSQL, SQL(..), raw, unRawSQL, (<+>), sqlConcatComma, parenthesize, IsSQL, toSQLCommand, unsafeFromString)
import DB.Model (Table(..))

kCommit :: MonadDB m => m ()
kCommit = getNexus >>= liftIO . commit

kRollback :: MonadDB m => m ()
kRollback = getNexus >>= liftIO . rollback

kClone :: MonadDB m => m Nexus
kClone = getNexus >>= liftIO . clone

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

-- | Finish current statement. 'kPrepare' and 'kExecute' call
-- 'kFinish' before they alter query.
kFinish :: MonadDB m => DBEnv m ()
kFinish = withDBEnvSt $ \s@DBEnvSt{..} -> do
  case dbStatement of
    Just st -> (, DBEnvSt { dbStatement = Nothing, dbValues = [] })
      `liftM` protIO (SQL (getQuery dbStatement) dbValues) (finish st)
    Nothing -> return ((), s)

kRunRaw :: MonadDB m => RawSQL -> DBEnv m ()
kRunRaw s = withDBEnv $ \_ -> getNexus >>= \c -> liftIO (runRaw c (unRawSQL s))

kRun_ :: (MonadDB m, IsSQL sql) => sql -> DBEnv m ()
kRun_ presql = kRun presql >> return ()

kRun :: (MonadDB m, IsSQL sql) => sql -> DBEnv m Integer
kRun presql = kPrepare s >> kExecute values
  where
    (SQL s values) = toSQLCommand presql

kRun01 :: (MonadDB m, IsSQL sql) => sql -> DBEnv m Bool
kRun01 presql = kPrepare s >> kExecute01 values
  where
    (SQL s values) = toSQLCommand presql

kGetTables :: MonadDB m => DBEnv m [String]
kGetTables = withDBEnv $ \_ -> getNexus >>= liftIO . getTables

kDescribeTable :: MonadDB m => RawSQL -> DBEnv m [(String, SqlColDesc)]
kDescribeTable table = withDBEnv $ \_ -> getNexus >>= \c -> liftIO (describeTable c (unRawSQL table))

-- | Represents the binding of an SQL expression to a column in an INSERT or
--   UPDATE statement.
data ColumnValue = ColumnValue RawSQL SQL

-- | Behaves like `sql` but accepts generic SQL.
--   It's useful when inserting data straight from another table and thus don't
--   have an extra parameter to pass in.
sqlGeneric :: RawSQL -> SQL -> ColumnValue
sqlGeneric column expr = ColumnValue column expr

sql :: Convertible a SqlValue => RawSQL -> a -> ColumnValue
sql column value =
  ColumnValue column (SQL "?" [toSql value])

data SQLType = INSERT | UPDATE

mkSQL :: SQLType -> Table -> [ColumnValue] -> SQL
mkSQL qtype Table{tblName} columnvalues = case qtype of
  INSERT ->
    "INSERT INTO" <+> raw tblName <+>
    parenthesize (sqlConcatComma $ map raw columns) <+>
    "SELECT" <+> sqlConcatComma values
  UPDATE ->
    "UPDATE" <+> raw tblName <+> "SET" <+>
    sqlConcatComma (zipWith (\c v -> raw c <+> "=" <+> v) columns values)
  where
    (columns, values) =
      unzip [(col, val) | ColumnValue col val <- columnvalues]

-- internals

getQuery :: Maybe Statement -> RawSQL
getQuery = unsafeFromString . fromMaybe "" . fmap HDBC.originalQuery

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
