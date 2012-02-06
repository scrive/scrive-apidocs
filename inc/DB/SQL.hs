{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.SQL (
    SQL(..)
  , sql'
  , sql
  ) where

import Data.Convertible
import Data.Monoid
import Database.HDBC

-- | Instance provided for convenience
instance Convertible SqlValue SqlValue where
    safeConvert = Right

data SQL = SQL String [SqlValue]
  deriving (Eq, Show)

instance Monoid SQL where
  mempty = SQL [] []
  (SQL q v) `mappend` (SQL q' v') = SQL (q ++ q') (v ++ v')

sql' :: Convertible a SqlValue => String -> String -> a -> (String, String, SqlValue)
sql' column placeholder value = (column, placeholder, toSql value)

sql :: Convertible a SqlValue => String -> a -> (String, String, SqlValue)
sql column value = sql' column "?" value
