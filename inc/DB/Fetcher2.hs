module DB.Fetcher2 (
    Fetcher
  , foldDB
  ) where

import Control.Monad.IO.Class
import Data.Convertible
import Data.Maybe
import Data.Monoid
import Database.HDBC hiding (originalQuery)
import qualified Control.Exception as E
import qualified Database.HDBC as HDBC

import DB.Classes
import DB.SQL

class Fetcher a r where
  apply :: Int -> [SqlValue] -> a -> Either DBException r
  arity :: a -> r -> Int

instance (Fetcher b r, Convertible SqlValue t) => Fetcher (t -> b) r where
  apply n (x:xs) f = either
    (Left . CannotConvertSqlValue mempty n "")
    (apply (n+1) xs . f)
    (safeFromSql x)
  apply n _ f = Left RowLengthMismatch {
      originalQuery = mempty
    , expected = n + arity f (undefined :: r)
    , delivered = n
  }

  arity f r = 1 + arity (f undefined) r

instance Fetcher r r where
  apply _ [] r = Right r
  apply n xs _ = Left RowLengthMismatch {
      originalQuery = mempty
    , expected = n
    , delivered = n + length xs
  }

  arity _ _ = 0

-- | Fold over executed statement to get results.
--
-- Note: decoder is a function that takes current accumulator value and
-- then arbitrary numer of arguments (it has to match number of columns we
-- are expected to get from a query, otherwise an exception will be thrown)
-- and returns updated accumulator (just like foldl does). Also, keep in mind
-- that you need to annotate types of all arguments this function takes
-- (usually it's not neccessary since compiler will be able to infer them,
-- but if it won't, you have to expect quite ugly error message about
-- missing instances).
foldDB :: Fetcher a b => (b -> a) -> b -> DB b
foldDB decoder !init_acc = do
  DBState{..} <- getDBState
  case dbStatement of
    Nothing -> return init_acc
    Just st -> do
      res <- liftIO $ worker st init_acc
      case res of
        Right acc -> return acc
        Left err -> kFinish >> E.throw err {
            originalQuery = SQL (HDBC.originalQuery st) dbValues
          }
  where
    worker st !acc = do
      mrow <- fetchRow st
      case mrow of
        Nothing -> return $ Right acc
        Just row ->
          case apply 0 row (decoder acc) of
            Right acc' -> worker st acc'
            Left err@CannotConvertSqlValue{position = pos} -> do
              column <- liftIO $ getColumnNames st
                >>= return . fromMaybe "" . listToMaybe . drop pos
              return $ Left CannotConvertSqlValue {
                  originalQuery = mempty
                , columnName    = column
                , position      = position err
                , convertError  = convertError err
                }
            Left err -> return $ Left err
