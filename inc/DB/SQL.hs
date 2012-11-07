{-# LANGUAGE OverloadedStrings #-}
module DB.SQL (
    SQL(..)
  , ColumnValue
  , sql'
  , sql
  , sqlNoBind
  , sqlParam
  , (<+>)
  , (<?>)
  , SQLType(..)
  , mkSQL
  , AscDesc(..)
  , IsSQL(..)
  , sqlOR
  , sqlJoinWith
  , sqlJoinWithAND
  , sqlConcatComma
  , sqlConcatAND
  , sqlConcatOR
  , parenthesize
  ) where

import Data.Convertible
import Data.List
import Data.Monoid
import Data.String (IsString, fromString)
import Database.HDBC

import DB.Model

infixl 6 <?>, <+>

data SQL = SQL String [SqlValue]
  deriving (Eq, Show)

instance Monoid SQL where
  mempty = SQL [] []
  (SQL q v) `mappend` (SQL q' v') = SQL (q ++ q') (v ++ v')

-- | Represents the binding of an SQL expression to a column in an INSERT or
--   UPDATE statement.
data ColumnValue = ColumnValue String SQL

class IsSQL a where
  toSQLCommand :: a -> SQL

instance IsSQL SQL where
  toSQLCommand = id

instance IsSQL String where
  toSQLCommand cmd = SQL cmd []

instance IsString SQL where
  fromString s = SQL s []

sqlParam :: Convertible a SqlValue => a -> SQL
sqlParam p = SQL "?" [toSql p]

(<+>) :: SQL -> SQL -> SQL
s1 <+> s2 = s1 <> " " <> s2

(<?>) :: Convertible a SqlValue => SQL -> a -> SQL
s <?> p = s <+> sqlParam p

sql' :: Convertible a SqlValue => String -> String -> a -> ColumnValue
sql' column placeholder value =
    ColumnValue column (SQL placeholder [toSql value])

sql :: Convertible a SqlValue => String -> a -> ColumnValue
sql column value = sql' column "?" value

-- | Behaves like `sql'` without binding a parameter.
--   It's useful when inserting data straight from another table and thus don't
--   have an extra parameter to pass in.
sqlNoBind :: String -> String -> ColumnValue
sqlNoBind column expr = ColumnValue column (SQL expr [])

-- for INSERT/UPDATE statements generation

data SQLType = INSERT | UPDATE

mkSQL :: SQLType -> Table -> [ColumnValue] -> SQL
mkSQL qtype Table{tblName} columnvalues = case qtype of
  INSERT ->
    SQL ("INSERT INTO " ++ tblName ++
         " (" ++ (intercalate ", " columns) ++ ")")
        [] <+>
    SQL "SELECT" [] <+> mconcat (intersperse ", " values)
  UPDATE ->
    SQL ("UPDATE " ++ tblName ++ " SET")
        [] <+>
    mconcat (intersperse "," $ zipWith (\c p -> SQL (c ++ " = ") [] <+> p)
                                       columns
                                       values)
  where
    (columns, values) =
      unzip [(col, val) | ColumnValue col val <- columnvalues]

-- | 'AscDesc' marks ORDER BY order as ascending or descending.
-- Conversion to SQL adds DESC marker to descending and no marker
-- to ascending order.
data AscDesc a = Asc a | Desc a

sqlOR :: SQL -> SQL -> SQL
sqlOR s1 s2 = mconcat [parenthesize s1, SQL " OR " [], parenthesize s2]

sqlJoinWith :: SQL -> [SQL] -> SQL
sqlJoinWith comm list = mconcat $ intersperse comm $ map parenthesize list

sqlJoinWithAND :: [SQL] -> SQL
sqlJoinWithAND = sqlJoinWith (SQL " AND " [])

sqlConcatComma :: [SQL] -> SQL
sqlConcatComma sqls =
  mconcat $ intersperse (SQL ", " []) sqls

sqlConcatAND :: [SQL] -> SQL
sqlConcatAND sqls =
  mconcat $ intercalate [SQL " AND " []] (map (\s -> [SQL "(" [], s, SQL ")" [] ]) sqls)

sqlConcatOR :: [SQL] -> SQL
sqlConcatOR sqls =
  mconcat $ intercalate [SQL " OR " []] (map (\s -> [SQL "(" [], s, SQL ")" [] ]) sqls)

parenthesize :: SQL -> SQL
parenthesize (SQL command values) = SQL ("(" ++ command ++ ")") values
