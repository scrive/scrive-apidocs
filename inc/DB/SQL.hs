module DB.SQL (
    RawSQL
  , unRawSQL
  , SQL(..)
  , raw
  , sqlParam
  , (<+>)
  , (<?>)
  , (<>)
  , AscDesc(..)
  , IsSQL(..)
  , sqlOR
  , sqlConcatComma
  , sqlConcatAND
  , sqlConcatOR
  , parenthesize
  , intersperse
  , unsafeFromString
  ) where

import Data.Convertible
import qualified Data.List
import Data.Monoid
import Data.String (IsString, fromString)
import Database.HDBC

infixl 7 <?>
infixr 6 <+>

-- | Raw SQL fragments and statements (without parameters).  The
-- standard way to construct 'RawSQL' is by using the
-- OverloadedStrings extension together with string literals.  Avoid
-- explicit use of 'fromString' at all, since it can be used to
-- convert any dynamic value to SQL, opening the door to SQL injection
-- attacks. If absolutely necessary, use 'unsafeFromString', to
-- indicate that you guarantee that SQL injection cannot happen.
newtype RawSQL = RawSQL { unRawSQL :: String }
  deriving (Eq, Ord)

instance Show RawSQL where
    showsPrec n x = showsPrec n (unRawSQL x)

instance IsString RawSQL where
  fromString = RawSQL

unsafeFromString :: String -> RawSQL
unsafeFromString = fromString

instance Convertible RawSQL SqlValue where
  safeConvert = safeConvert . unRawSQL

-- | Parameterized SQL fragments and statements
data SQL = SQL RawSQL [SqlValue]
  deriving (Eq)

instance Show SQL where
    showsPrec _ (SQL rawsql values) = ((++) "SQL ") . shows rawsql . ((++) " [") . ((++) (Data.List.intercalate "," $ map fromSql values)) . ((++) "]")

-- | Convenience class for things that can be turned into 'SQL'.
-- There is intentionally no instance for String, use 'SQL' in a
-- default declaration instead.
class IsSQL a where
  toSQLCommand :: a -> SQL

instance IsSQL SQL where
  toSQLCommand = id

raw :: RawSQL -> SQL
raw r = SQL r []

instance Monoid RawSQL where
  mempty = RawSQL ""
  (RawSQL q) `mappend` (RawSQL q') = RawSQL (q ++ q')

instance Monoid SQL where
  mempty = raw mempty
  (SQL q v) `mappend` (SQL q' v') = SQL (q <> q') (v ++ v')

instance IsString SQL where
  fromString s = SQL (fromString s) []

sqlParam :: Convertible a SqlValue => a -> SQL
sqlParam p = SQL "?" [toSql p]

-- | Concatenate fragments with a space in between
(<+>) :: SQL -> SQL -> SQL
s1 <+> s2 = s1 <> " " <> s2

-- | Append a parameter to a SQL fragment
(<?>) :: Convertible a SqlValue => SQL -> a -> SQL
s <?> p = s <+> sqlParam p

-- | 'AscDesc' marks ORDER BY order as ascending or descending.
-- Conversion to SQL adds DESC marker to descending and no marker
-- to ascending order.
data AscDesc a = Asc a | Desc a

sqlOR :: SQL -> SQL -> SQL
sqlOR s1 s2 = sqlConcatOR [s1, s2]

sqlConcatComma :: [SQL] -> SQL
sqlConcatComma = intersperse ","

sqlConcatAND :: [SQL] -> SQL
sqlConcatAND = intersperse "AND" . map parenthesize

sqlConcatOR :: [SQL] -> SQL
sqlConcatOR = intersperse "OR" . map parenthesize

parenthesize :: SQL -> SQL
parenthesize s = "(" <+> s <+> ")"

intersperse :: RawSQL -> [SQL] -> SQL
intersperse i = foldr (<+>) mempty . Data.List.intersperse (SQL i [])
