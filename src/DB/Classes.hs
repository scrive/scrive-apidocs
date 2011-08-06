{-# LANGUAGE FunctionalDependencies #-}
module DB.Classes (
    DBQuery(..)
  , DBUpdate(..)
  , DBMonad(..)
  , DB
  , wrapDB
  , ioRunDB
  , runDB
  , runDBQuery
  , runDBUpdate
  , QueryResultException(..)
  , catchDB
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Typeable
import qualified Control.Exception as E

-- query typeclasses
class DBQuery q r | q -> r where
  dbQuery :: q -> DB r

class DBUpdate q r | q -> r where
  dbUpdate :: q -> DB r

class (Functor m, MonadIO m) => DBMonad m where
  getConnection :: m Connection

-- wrapper for calling db related functions in controlled environment
-- (DB/unDB is not exposed so functions may enter DB wrapper, but
-- they can't escape it in any other way than by runDB function)
newtype DB a = DB { unDB :: ReaderT Connection IO a }
  deriving (Applicative, Functor, Monad, MonadPlus, MonadIO)

instance DBMonad DB where
  getConnection = DB ask

-- | Wraps IO action in DB
wrapDB :: (Connection -> IO a) -> DB a
wrapDB f = getConnection >>= liftIO . f

-- | Runs DB action in single transaction
ioRunDB :: Connection -> DB a -> IO (Either String a)
ioRunDB conn f =
  (Right <$> withTransaction conn (runReaderT (unDB f))) `catchSql` handleSQL `E.catch` handleSome
  where
    handleSQL = return . Left . (++) "SQL Error: " . seErrorMsg
    handleSome :: E.SomeException -> IO (Either String a)
    handleSome = return . Left . show

-- | Runs DB action in a db monad (no explicit connection passing)
runDB :: DBMonad m => DB a -> m (Either String a)
runDB f = getConnection >>= liftIO . flip ioRunDB f

-- | Runs single db query, provided for convenience.
runDBQuery :: (DBMonad m, DBQuery q r) => q -> m (Either String r)
runDBQuery = runDB . dbQuery

-- | Runs single db update, provided for convenience.
runDBUpdate :: (DBMonad m, DBUpdate q r) => q -> m (Either String r)
runDBUpdate = runDB . dbUpdate

-- Exceptions

data QueryResultException =
    NoObject
  | TooManyObjects {
      tmoExpected :: Integer
    , tmoGiven :: Integer
  }
    deriving Typeable

instance E.Exception QueryResultException

instance Show QueryResultException where
  show NoObject = "No such object"
  show TooManyObjects{tmoExpected, tmoGiven} =
    "Too many objects returned/affected by query (" ++ show tmoExpected ++ " expected, " ++ show tmoGiven ++ " given)"

catchDB :: E.Exception e => DB a -> (e -> DB a) -> DB a
catchDB f exhandler = do
  conn <- getConnection
  let handler e = runReaderT (unDB $ exhandler e) conn
  liftIO $ runReaderT (unDB f) conn `E.catch` handler
