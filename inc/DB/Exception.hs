
module DB.Exception
  ( DBException(..)
  ) where

import Data.Convertible
import Data.Typeable
import qualified Database.HDBC as HDBC
import qualified Control.Exception as E

import DB.SQL

-- Exceptions

data DBException 
  = SQLError 
    { originalQuery :: SQL
    , sqlError :: HDBC.SqlError
    }
  | NoObject
    { originalQuery :: SQL
    }
  | TooManyObjects 
    { originalQuery :: SQL
    , tmoExpected :: Integer
    , tmoGiven :: Integer
    }
  | RowLengthMismatch
    { originalQuery :: SQL
    , expected :: Int
    , delivered :: Int
    }
  | CannotConvertSqlValue
    { originalQuery :: SQL
    , position :: Int
    , columnName :: String
    , convertError :: ConvertError
    }
  | CannotParseRow
    { originalQuery :: SQL
    , message :: String
    }
  | NoStatementPrepared
    { originalQuery :: SQL
    }
    deriving Typeable

instance E.Exception DBException

instance Show DBException where
  show SQLError{sqlError, originalQuery} = "SQL error: " ++ HDBC.seErrorMsg sqlError ++ " in " ++ show originalQuery
  show NoObject{originalQuery} = "Query result error: No object returned when there had to be one in " ++ show originalQuery
  show RowLengthMismatch{originalQuery, expected,delivered} = "Expected row length of " ++ show expected ++ " got " ++ show delivered ++ " in " ++ show originalQuery
  show CannotConvertSqlValue{originalQuery,position,convertError,columnName} = "Cannot convert param " ++ show position ++ " '" ++ columnName ++ "' because of " ++ show convertError ++ " in " ++ show originalQuery
  show CannotParseRow{originalQuery, message} = message ++ " in " ++ show originalQuery
  show TooManyObjects{originalQuery, tmoExpected, tmoGiven} =
    "Query result error: Too many objects returned/affected by query (" ++ show tmoExpected ++ " expected, " ++ show tmoGiven ++ " given) in " ++ show originalQuery
  show (NoStatementPrepared _) = "Call to kExecute when there was no statement prepared"
