module DB.Utils (
    module DB.SQL
  , getOne
  , getUniqueID
  , getUniqueIDField
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , checkIfAnyReturned
  , kRun
  , kRun01
  , SQLType(..)
  , mkSQL
  ) where

import Data.Convertible
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Database.HDBC as HDBC
import Crypto.RNG (random)
import qualified Control.Exception as E

import DB.Classes as DB
import DB.Fetcher2
import DB.Model
import DB.SQL


getUniqueID :: Table -> DB Int64
getUniqueID table = getUniqueIDField table "id"

getUniqueIDField :: Table -> String -> DB Int64
getUniqueIDField table fieldname = do
  muid <- do
    uid <- random
    kPrepare $ "SELECT " ++ fieldname ++ " FROM " ++ tblName table ++ " WHERE " ++ fieldname ++ " = ?"
    _ <- kExecute [toSql uid]
    foldDB (\_ (_::SqlValue) -> Nothing :: Maybe Int64) (Just uid)
  maybe (getUniqueIDField table fieldname) return muid

oneRowAffectedGuard :: Monad m => Integer -> m Bool
oneRowAffectedGuard 0 = return False
oneRowAffectedGuard 1 = return True
oneRowAffectedGuard n = E.throw TooManyObjects {
    DB.originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = n
  }

oneObjectReturnedGuard :: Monad m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  = E.throw TooManyObjects {
    DB.originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = fromIntegral $ length xs
  }

checkIfOneObjectReturned :: Monad m => [a] -> m Bool
checkIfOneObjectReturned xs =
  oneObjectReturnedGuard xs
    >>= return . maybe False (const True)

getOne :: Convertible SqlValue a => String -> [SqlValue] -> DB (Maybe a)
getOne query values = do
  kPrepare query
  _ <- kExecute values
  foldDB (\acc v -> v : acc) []
    >>= oneObjectReturnedGuard

checkIfAnyReturned :: String -> [SqlValue] -> DB Bool
checkIfAnyReturned query values =
  (getOne query values :: DB (Maybe SqlValue))
    >>= checkIfOneObjectReturned . maybeToList

kRun :: SQL -> DB Integer
kRun (SQL query values) = kPrepare query >> kExecute values

kRun01 :: SQL -> DB Bool
kRun01 (SQL query values) = kPrepare query >> kExecute01 values

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

