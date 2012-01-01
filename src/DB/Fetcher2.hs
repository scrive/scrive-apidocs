module DB.Fetcher2 (
  foldDB
  ) where

import Database.HDBC as HDBC
import qualified Control.Exception as E

import DB.Exception as DB
import Control.Monad.IO.Class
import Control.Monad
import Data.Convertible

class Fetcher a r where
  apply :: Int -> [SqlValue] -> a -> Either DBException r
  arity :: a -> r -> Int

instance (Fetcher b r, Convertible SqlValue t) => Fetcher (t -> b) r where
  apply n (x:xs) f = either
    (Left . CannotConvertSqlValue "" n)
    (apply (n+1) xs . f)
    (safeFromSql x)
  apply n _ f = Left $ RowLengthMismatch {
      DB.originalQuery = ""
    , expected = n + arity f (undefined :: r)
    , delivered = n
  }

  arity f r = 1 + arity (f undefined) r

instance Fetcher r r where
  apply _ [] r = Right r
  apply n xs _ = Left RowLengthMismatch {
      DB.originalQuery = ""
    , expected = n
    , delivered = n + length xs
  }

  arity _ _ = 0

foldDB :: (MonadIO m, Fetcher a b) => Statement -> (b -> a) -> b -> m b
foldDB st decoder = liftIO . worker
  where
    worker acc = do
      mrow <- fetchRow st
      case mrow of
        Nothing -> return acc
        Just row ->
          case apply 0 row (decoder acc) of
            Right acc' -> worker acc'
            Left err -> do
              finish st
              E.throwIO err { DB.originalQuery = HDBC.originalQuery st }
