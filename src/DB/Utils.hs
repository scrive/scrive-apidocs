module DB.Utils
  ( getUniqueID
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


class FetcherArity a where
    type FetcherResult a :: *
    executeFetcher' :: Int -> a -> [SqlValue] -> Either DBException (FetcherResult a)
    fetcherArity :: a -> Int

instance FetcherArity (Either DBException r) where
    type FetcherResult (Either DBException r) = r

    executeFetcher' _ action [] = action
    executeFetcher' n _ xs = 
      Left $ RowLengthMismatch "" n (n + length xs)

    fetcherArity _ = 0

instance (FetcherArity b, Convertible SqlValue t) => FetcherArity (t -> b) where
    type FetcherResult (t -> b) = FetcherResult b

    executeFetcher' n action (x:xs) = 
      case safeFromSql x of
        Right value -> executeFetcher' (n+1) (action value) xs
        Left cnvError -> Left $ CannotConvertSqlValue "" n cnvError

    executeFetcher' n action _ = do
      Left $ RowLengthMismatch "" (n + fetcherArity action) n

    fetcherArity action = 1 + fetcherArity (action undefined) 

fetchValues :: (MonadIO m, FetcherArity fetcher) => Statement -> fetcher -> m [FetcherResult fetcher]
fetchValues st decoder = liftM reverse (worker [])
  where
    worker acc = do
      mrow <- liftIO $ fetchRow st
      case mrow of
        Nothing -> return acc
        Just row -> 
          case executeFetcher' 0 decoder row of
            Right value -> worker (value : acc)
            Left left -> do
                   liftIO $ finish st
                   liftIO $ E.throwIO $ left { DB.originalQuery = HDBC.originalQuery st }
