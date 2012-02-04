module DB.SQL (
    SQL(..)
  , SQLType(..)
  , sql'
  , sql
  , mkSQL
  , (<++>)
  ) where

import Data.Convertible
import Data.List
import Data.Monoid
import Database.HDBC

import DB.Model
import Misc ((<++>))

data SQL = SQL String [SqlValue]
  deriving (Eq, Show)

instance Monoid SQL where
  mempty = SQL [] []
  (SQL q v) `mappend` (SQL q' v') = SQL (q ++ q') (v ++ v')

data SQLType = INSERT | UPDATE

sql' :: Convertible a SqlValue => String -> String -> a -> (String, String, SqlValue)
sql' column placeholder value = (column, placeholder, toSql value)

sql :: Convertible a SqlValue => String -> a -> (String, String, SqlValue)
sql column value = sql' column "?" value

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
