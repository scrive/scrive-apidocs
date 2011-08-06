module DB.Utils where

import Data.Int
import Database.HDBC
import System.Random
import qualified Control.Exception as E

import DB.Classes

getUniqueID :: String -> DB Int64
getUniqueID table = do
  muid <- wrapDB $ \conn -> do
    uid <- randomRIO (0, 0x7ffffffffffffff::Int64)
    st <- prepare conn $ "SELECT id FROM " ++ table ++ " WHERE id = ?"
    fetchRow st >>= return . maybe (Just uid) (const Nothing)
  maybe (getUniqueID table) return muid

oneRowAffectedGuard :: Monad m => Integer -> m ()
oneRowAffectedGuard 0 = E.throw NoObject
oneRowAffectedGuard 1 = return ()
oneRowAffectedGuard n =
  E.throw TooManyObjects {
      tmoExpected = 1
    , tmoGiven = n
  }

oneObjectReturnedGuard :: Monad m => [a] -> m a
oneObjectReturnedGuard []  = E.throw NoObject
oneObjectReturnedGuard [x] = return x
oneObjectReturnedGuard xs  =
  E.throw TooManyObjects {
      tmoExpected = 1
    , tmoGiven = fromIntegral $ length xs
  }
