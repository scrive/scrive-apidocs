
module DB.Exception
  ( DBException(..)
  ) where

import Data.Convertible
import Data.Typeable
import qualified Database.HDBC as HDBC
import qualified Control.Exception as E


-- Exceptions

data DBException 
  = SQLError 
    { originalQuery :: String
    , sqlError :: HDBC.SqlError
    }
  | NoObject
    { originalQuery :: String
    }
  | TooManyObjects 
    { originalQuery :: String
    , tmoExpected :: Integer
    , tmoGiven :: Integer
    }
  | RowLengthMismatch
    { originalQuery :: String
    , expected :: Int
    , delivered :: Int
    }
  | CannotConvertSqlValue
    { originalQuery :: String
    , position :: Int
    , convertError :: ConvertError
    }
  | CannotParseRow
    { originalQuery :: String
    , message :: String
    }
    deriving Typeable

instance E.Exception DBException

instance Show DBException where
  show SQLError{sqlError, originalQuery} = "SQL error: " ++ HDBC.seErrorMsg sqlError ++ " in " ++ originalQuery
  show NoObject{originalQuery} = "Query result error: No object returned when there had to be one in " ++ originalQuery
  show RowLengthMismatch{originalQuery, expected,delivered} = "Expected row length of " ++ show expected ++ " got " ++ show delivered ++ " in " ++ originalQuery
  show CannotConvertSqlValue{originalQuery,position,convertError} = "Cannot convert param " ++ show position ++ " because of " ++ show convertError ++ " in " ++ originalQuery
  show CannotParseRow{originalQuery, message} = message ++ " in " ++ originalQuery
  show TooManyObjects{originalQuery, tmoExpected, tmoGiven} =
    "Query result error: Too many objects returned/affected by query (" ++ show tmoExpected ++ " expected, " ++ show tmoGiven ++ " given) in " ++ originalQuery
