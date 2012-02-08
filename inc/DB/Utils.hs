module DB.Utils (
    module DB.SQL
  , getOne
  , getUniqueID
  , getUniqueIDField
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , GetUniqueID
  , kRun
  , kRun01
  , kQuickQuery
  , SQLType(..)
  , mkSQL
  ) where

import Data.Int
import Data.List
import Data.Monoid
import Database.HDBC as HDBC
import System.Random (randomRIO)
import qualified Control.Exception as E

import DB.Classes as DB
import DB.Model
import DB.SQL
import Data.Convertible
import Control.Monad

class GetUniqueID m where
    getUniqueIDField :: Table -> String -> m Int64

getUniqueID :: (GetUniqueID m) => Table -> m Int64
getUniqueID table = getUniqueIDField table "id"

instance GetUniqueID DB where
    getUniqueIDField table fieldname = do
      muid <- wrapDB $ \conn -> do
        uid <- randomRIO (0, maxBound)
        st <- prepare conn $ "SELECT " ++ fieldname ++ " FROM " ++ tblName table ++ " WHERE " ++ fieldname ++ " = ?"
        _ <- execute st [toSql uid]
        fetchRow st >>= return . maybe (Just uid) (const Nothing)
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
getOne query values = wrapDB $ \c ->
  quickQuery' c query values
    >>= oneObjectReturnedGuard . join
    >>= return . fmap fromSql

kRun :: SQL -> DB Integer
kRun (SQL query values) = kPrepare query >> kExecute values

kRun01 :: SQL -> DB Bool
kRun01 (SQL query values) = kPrepare query >> kExecute01 values

kQuickQuery :: SQL -> DB [[SqlValue]]
kQuickQuery (SQL query values) = wrapDB $ \c -> quickQuery' c query values


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
