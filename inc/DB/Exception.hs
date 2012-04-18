module DB.Exception (
    SQLError(..)
  , DBEnvError(..)
  ) where

import Data.Convertible
import Data.Typeable
import qualified Database.HDBC as HDBC
import qualified Control.Exception as E

import DB.SQL

-- Exceptions

data SQLError =
    SQLError {
    originalQuery :: SQL
  , sqlError :: HDBC.SqlError
  }
  | TooManyObjects {
    originalQuery :: SQL
  , tmoExpected :: Integer
  , tmoGiven :: Integer
  }
  | RowLengthMismatch {
    originalQuery :: SQL
  , expected :: Int
  , delivered :: Int
  }
  | CannotConvertSqlValue {
    originalQuery :: SQL
  , position :: Int
  , columnName :: String
  , convertError :: ConvertError
  } deriving Typeable

data DBEnvError = NoObject String | NoStatementPrepared
  deriving Typeable

instance E.Exception SQLError
instance E.Exception DBEnvError

instance Show SQLError where
  show SQLError{sqlError, originalQuery} = "SQL error: " ++ HDBC.seErrorMsg sqlError ++ " in " ++ show originalQuery
  show RowLengthMismatch{originalQuery, expected,delivered} = "Expected row length of " ++ show expected ++ " got " ++ show delivered ++ " in " ++ show originalQuery
  show CannotConvertSqlValue{originalQuery,position,convertError,columnName} = "Cannot convert param " ++ show position ++ " '" ++ columnName ++ "' because of " ++ show convertError ++ " in " ++ show originalQuery
  show TooManyObjects{originalQuery, tmoExpected, tmoGiven} =
    "Query result error: Too many objects returned/affected by query (" ++ show tmoExpected ++ " expected, " ++ show tmoGiven ++ " given) in " ++ show originalQuery

instance Show DBEnvError where
  show (NoObject t) = "No object of type " ++ t ++ " returned when there should have been one"
  show NoStatementPrepared = "Call to kExecute when there was no statement prepared"
