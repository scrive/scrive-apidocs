{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.SQL (
    SQL(..)
  , sql'
  , sql
  , SQLType(..)
  , mkSQL
  , AscDesc(..)
  ) where

import Data.Convertible
import Data.List
import Data.Monoid
import Database.HDBC

import DB.Model

data SQL = SQL String [SqlValue]
  deriving (Eq, Show)

instance Monoid SQL where
  mempty = SQL [] []
  (SQL q v) `mappend` (SQL q' v') = SQL (q ++ q') (v ++ v')

sql' :: Convertible a SqlValue => String -> String -> a -> (String, String, SqlValue)
sql' column placeholder value = (column, placeholder, toSql value)

sql :: Convertible a SqlValue => String -> a -> (String, String, SqlValue)
sql column value = sql' column "?" value

-- for INSERT/UPDATE statements generation

data SQLType = INSERT | UPDATE

mkSQL :: SQLType -> Table -> [(String, String, SqlValue)] -> SQL
mkSQL qtype Table{tblName} values = case qtype of
  INSERT -> SQL ("INSERT INTO " ++ tblName
    ++ " (" ++ (intercalate ", " columns) ++ ")"
    ++ " SELECT " ++ (intercalate ", " placeholders)
    ++ " ") vals
  UPDATE -> SQL ("UPDATE " ++ tblName ++ " SET "
    ++ (intercalate ", " $ zipWith (\c p -> c ++ " = " ++ p) columns placeholders)
    ++ " ") vals
  where
    (columns, placeholders, vals) = unzip3 values

-- | 'AscDesc' marks ORDER BY order as ascending or descending.
-- Conversion to SQL adds DESC marker to descending and no marker
-- to ascending order.
data AscDesc a = Asc a | Desc a
