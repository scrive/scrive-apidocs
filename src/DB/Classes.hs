{-# LANGUAGE FunctionalDependencies #-}
module DB.Classes (
    DBQuery(..)
  , DBUpdate(..)
  , DBMonad(..)
  , DB
  , wrapDB
  , ioRunDB
  , runDB
  , runDBOrFail
  , runDBQuery
  , runDBUpdate
  , DBException(..)
  , catchDB
  , tryDB
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Typeable
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Control.Exception as E
import Util.MonadUtils
import Data.Convertible

-- query typeclasses
class DBQuery q r | q -> r where
  dbQuery :: q -> DB r

class DBUpdate q r | q -> r where
  dbUpdate :: q -> DB r

-- | DBMonad class. getConnection function is used for getting connection
-- from underlying state object (obviously). If it comes to handleDBError,
-- it's for handling hard db errors like lost connection, sql errors of
-- all kind or the fact that query did something that should never happen.
-- In such case we want to interrupt what we're currently doing and exit
-- gracefully, most likely logging an error is some way. Since a way of
-- handling such case may vary in different monads, hence this function.
class (Functor m, MonadIO m) => DBMonad m where
  getConnection :: m Connection
  -- | From the point of view of the implementor of instances of
  -- 'DBMonad': handle a database error.  However, code that uses
  -- 'handleDBError' throws an exception - compare with 'throw', or
  -- 'fail'.
  handleDBError :: DBException -> m a

instance (Monad m, Functor m, MonadIO m) => DBMonad (ReaderT Connection m) where
  getConnection = ask
  handleDBError e = error $ "instance (Monad m) => DBMonad (ReaderT Connection m) lacks handleDBError " ++ show e

-- | Wrapper for calling db related functions in controlled environment
-- (DB/unDB is not exposed so functions may enter DB wrapper, but they
-- can't escape it in any other way than by runDB function).
newtype DB a = DB { unDB :: ReaderT Connection IO a }
  deriving (Applicative, Functor, Monad, MonadPlus, MonadIO, DBMonad)

-- | Wraps IO action in DB
wrapDB :: (Connection -> IO a) -> DB a
wrapDB f = DB ask >>= liftIO . f

-- | Runs DB action in single transaction (IO monad). Note that since
-- it runs in IO, you need to pass Connecion object explicitly. Also,
-- sql releated exceptions are not handled, so you probably need to do
-- it yourself. Use this function ONLY if there is no way to use runDB.
ioRunDB :: MonadIO m => Connection -> DB a -> m a
ioRunDB conn f = liftIO $ withTransaction conn (runReaderT (unDB f))

-- | Runs DB action in a DB compatible monad
runDB :: DBMonad m => DB a -> m a
runDB f = do
  conn <- getConnection
  res <- liftIO $ (Right <$> withTransaction conn (runReaderT (unDB f))) `E.catches` handlers
  case res of
    Right r -> return r
    Left e  -> handleDBError e
  where
    -- we catch only SqlError/DBException here
    handlers = [
        E.Handler (return . Left . SQLError "")
      , E.Handler (return . Left)
      ]

-- | Runs DB action and mzeroes if it returned Nothing
runDBOrFail :: (DBMonad m, MonadPlus m) => DB (Maybe r) -> m r
runDBOrFail f = runDB f >>= guardJust

-- | Runs single db query (provided for convenience).
runDBQuery :: (DBMonad m, DBQuery q r) => q -> m r
runDBQuery = runDB . dbQuery

-- | Runs single db update (provided for convenience).
runDBUpdate :: (DBMonad m, DBUpdate q r) => q -> m r
runDBUpdate = runDB . dbUpdate




-- Exceptions

data DBException 
  = SQLError 
    { originalQuery :: String
    , sqlError :: SqlError
    }
  | NoObject
    { originalQuery :: String
    }
  | TooManyObjects 
    { originalQuery :: String
    , tmoExpected :: Integer
    , tmoGiven :: Integer
    }
  | RowLengthMismatch
    { originalQuery :: String
    , expected :: Int
    , delivered :: Int
    }
  | CannotConvertSqlValue
    { originalQuery :: String
    , position :: Int
    , convertError :: ConvertError
    }
  | CannotParseRow
    { originalQuery :: String
    , message :: String
    }
    deriving Typeable

instance E.Exception DBException

instance Show DBException where
  show SQLError{sqlError} = "SQL error: " ++ seErrorMsg sqlError
  show NoObject{} = "Query result error: No object returned when there had to be one"
  show RowLengthMismatch{expected,delivered} = "Expected row length of " ++ show expected ++ " got " ++ show delivered
  show CannotConvertSqlValue{position,convertError} = "Cannot convert param " ++ show position ++ " because of " ++ show convertError
  show CannotParseRow{message} = message
  show TooManyObjects{tmoExpected, tmoGiven} =
    "Query result error: Too many objects returned/affected by query (" ++ show tmoExpected ++ " expected, " ++ show tmoGiven ++ " given)"

-- | Catch in DB monad
catchDB :: E.Exception e => DB a -> (e -> DB a) -> DB a
catchDB f exhandler = wrapDB $ \conn -> do
  let handler e = runReaderT (unDB $ exhandler e) conn
  liftIO $ runReaderT (unDB f) conn `E.catch` handler

-- | Try in DB monad
tryDB :: E.Exception e => DB a -> DB (Either e a)
tryDB f = wrapDB $ liftIO . E.try . runReaderT (unDB f)
