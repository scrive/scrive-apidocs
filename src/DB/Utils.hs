module DB.Utils (
    getOne
  , getUniqueID
  , getUniqueIDField
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , fetchValues
  ) where

import Data.Int
import Database.HDBC as HDBC
import System.Random
import qualified Control.Exception as E

import DB.Classes as DB
import DB.Model
import Happstack.State()
import Control.Monad.IO.Class
import Control.Monad
import Data.Convertible

getOne :: Convertible SqlValue a => String -> [SqlValue] -> DB (Maybe a)
getOne query values = wrapDB $ \c ->
  quickQuery' c query values
    >>= oneObjectReturnedGuard . join
    >>= return . fmap fromSql

getUniqueID :: Table -> DB Int64
getUniqueID table = do
  muid <- wrapDB $ \conn -> do
    uid <- randomRIO (0, maxBound)
    st <- prepare conn $ "SELECT id FROM " ++ tblName table ++ " WHERE id = ?"
    fetchRow st >>= return . maybe (Just uid) (const Nothing)
  maybe (getUniqueID table) return muid

getUniqueIDField :: Table -> String -> DB Int64
getUniqueIDField table fieldname = do
  muid <- wrapDB $ \conn -> do
    uid <- randomRIO (0, maxBound)
    st <- prepare conn $ "SELECT " ++ fieldname ++ " FROM " ++ tblName table ++ " WHERE " ++ fieldname ++ " = ?"
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

class Fetcher a where
    type FetchResult a :: *
    fetchWorker :: Int -> a -> [SqlValue] -> Either DBException (FetchResult a)
    fetchArity :: a -> Int

instance Fetcher (Either DBException r) where
    type FetchResult (Either DBException r) = r

    fetchWorker _ action [] = action
    fetchWorker n _ xs = 
      Left $ RowLengthMismatch "" n (n + length xs)

    fetchArity _ = 0

instance (Fetcher b, Convertible SqlValue t) => Fetcher (t -> b) where
    type FetchResult (t -> b) = FetchResult b

    fetchWorker n action (x:xs) = 
      case safeFromSql x of
        Right value -> fetchWorker (n+1) (action value) xs
        Left cnvError -> Left $ CannotConvertSqlValue "" n cnvError

    fetchWorker n action _ = do
      Left $ RowLengthMismatch "" (n + fetchArity action) n

    fetchArity action = 1 + fetchArity (action undefined) 

fetchValues :: (MonadIO m, Fetcher fetcher) => Statement -> fetcher -> m [FetchResult fetcher]
fetchValues st decoder = liftM reverse (worker [])
  where
    worker acc = do
      mrow <- liftIO $ fetchRow st
      case mrow of
        Nothing -> return acc
        Just row -> 
          case fetchWorker 0 decoder row of
            Right value -> worker (value : acc)
            Left left -> do
                   liftIO $ finish st
                   liftIO $ E.throwIO $ left { DB.originalQuery = HDBC.originalQuery st }
