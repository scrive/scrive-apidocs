module DB.Utils (
    getOne
  , getUniqueID
  , getUniqueIDField
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , GetUniqueID
  ) where

import Data.Int
import Database.HDBC as HDBC
import Crypto.RNG (random)
import qualified Control.Exception as E

import DB.Classes as DB
import DB.Model
import Data.Convertible
import Control.Monad

-- | Create unique database identifiers.  To play it safe, we use
-- 'Crypto.RNG.random' to pick a cryptographically secure random one,
-- instead of using sequences.  As Gracjan put it: "one more number to
-- guess" for an attacker.
class GetUniqueID m where
    getUniqueIDField :: Table -> String -> m Int64

getUniqueID :: (GetUniqueID m) => Table -> m Int64
getUniqueID table = getUniqueIDField table "id"

instance GetUniqueID DB where
    getUniqueIDField table fieldname = do
      uid <- random
      muid <- wrapDB $ \conn -> do
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
     , queryParams = []
     , tmoExpected = 1
     , tmoGiven = n
     }

oneObjectReturnedGuard :: Monad m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  =
  E.throw TooManyObjects 
     { DB.originalQuery = ""
     , queryParams = []
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
    