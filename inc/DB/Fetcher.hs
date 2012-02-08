module DB.Fetcher
  ( Fetcher(..)
  , fetchValues
  ) where

import Data.Monoid
import Database.HDBC as HDBC
import qualified Control.Exception as E

import DB.Exception as DB
import DB.SQL
import Control.Monad.IO.Class
import Control.Monad
import Data.Convertible

class Fetcher a where
    type FetchResult a :: *
    fetchWorker :: Int -> a -> [SqlValue] -> Either DBException (FetchResult a)
    fetchArity :: a -> Int

instance Fetcher (Either DBException r) where
    type FetchResult (Either DBException r) = r

    fetchWorker _ action [] = action
    fetchWorker n _ xs = 
      Left $ RowLengthMismatch mempty n (n + length xs)

    fetchArity _ = 0

instance (Fetcher b, Convertible SqlValue t) => Fetcher (t -> b) where
    type FetchResult (t -> b) = FetchResult b

    fetchWorker n action (x:xs) = 
      case safeFromSql x of
        Right value -> fetchWorker (n+1) (action value) xs
        Left cnvError -> Left $ CannotConvertSqlValue mempty n "" cnvError

    fetchWorker n action _ = do
      Left $ RowLengthMismatch mempty (n + fetchArity action) n

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
            Left left@CannotConvertSqlValue{position = pos} -> do
                   -- getColumnNames is not defined... I cannot find
                   -- why, but lets catch this exception here and
                   -- ignore it for now
                   columns <- liftIO $ (getColumnNames st `E.catch` \(_ :: E.SomeException) -> return [])
                   let column = if pos<0 || pos>= length columns
                                then ""
                                else columns !! pos
                   liftIO $ finish st
                   liftIO $ E.throwIO $ CannotConvertSqlValue
                            { DB.originalQuery = SQL (HDBC.originalQuery st) []
                            , DB.columnName    = column
                            , DB.position      = DB.position left
                            , DB.convertError  = DB.convertError left
                            }
            Left left -> do
                   liftIO $ finish st
                   liftIO $ E.throwIO $ left { DB.originalQuery = SQL (HDBC.originalQuery st) [] }
