module DB.Utils
  ( getUniqueID
  , getUniqueIDField
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , fetchValues
  , GetUniqueID
  ) where

import Data.Int
import Database.HDBC as HDBC
import System.Random
import qualified Control.Exception as E

import DB.Classes as DB
import DB.Model
import Happstack.State()
import DB.Fetcher

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
oneRowAffectedGuard n =
  E.throw TooManyObjects 
     { DB.originalQuery = ""
     , tmoExpected = 1
     , tmoGiven = n
     }

oneObjectReturnedGuard :: Monad m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  =
  E.throw TooManyObjects 
     { DB.originalQuery = ""
     , tmoExpected = 1
     , tmoGiven = fromIntegral $ length xs
     }

checkIfOneObjectReturned :: Monad m => [a] -> m Bool
checkIfOneObjectReturned xs =
  oneObjectReturnedGuard xs
    >>= return . maybe False (const True)

