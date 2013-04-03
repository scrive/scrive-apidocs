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

import Control.Monad.State.Strict hiding (state)
import Data.Convertible (Convertible)
import Database.HDBC hiding (originalQuery)

import DB.Core
import DB.Exception
import DB.SQL (RawSQL, SQL(..), raw, (<+>), sqlConcatComma, parenthesize, IsSQL, toSQLCommand)
import DB.Model (Table(..))



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
