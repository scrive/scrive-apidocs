{-# LANGUAGE FunctionalDependencies #-}

-- | This module defines a nice monad for SQL statement execution. This has the following properties:
-- 
-- * keeps connection hidden in the monad
--
-- * keeps current statement in the monad
--
-- * if any command throws SqlError exception it will be wrapped in
-- 'DBException' as 'SQLError' and have 'originalQuery' set with
-- information. Great help in debugging.
--
-- * automatically converts from and to ['SqlValue'] rows
--
-- * properly does 'rollback' when exception is thrown (a bug in HDBC)
--
-- * provides for less noisy style
--
-- Example:
--
-- > runDBInDB $ do
-- >    kPrepare "SELECT title FROM documents WHERE id = ?"
-- >    r <- kExecute [toSql docid]
-- >    Just doc <- kFetchRow decodeRowAsDocument
-- >    return doc
--

module DB.Classes (
    module DB.Core
  , module DB.Exception
  , DBEnvSt(..)
  , DBEnv
  , mapDBEnv
  , runDBEnv
  , withDBEnvSt
  , DBQuery(..)
  , DBUpdate(..)
  , dbQuery
  , dbUpdate
  , withPostgreSQL
  , kPrepare
  , kExecute
  , kExecute1
  , kExecute01
  , kExecute1P
  , kFinish
  , kRunRaw
  , kDescribeTable
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (state)
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Monoid
import Database.HDBC hiding (originalQuery)
import qualified Control.Exception as E
import qualified Control.Exception.Lifted as EL
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as PG

import Control.Monad.Trans.Control.Util
import DB.Core
import DB.Exception
import DB.Nexus
import DB.SQL hiding (sql)

data DBEnvSt = DBEnvSt {
    dbStatement :: !(Maybe Statement)
  , dbValues    :: ![SqlValue]
  }

type InnerDBEnv = StateT DBEnvSt

-- | 'DBEnv' is a monad transformer that adds state management of executed
-- database action, handle exceptions correctly, closes statement when it
-- is not useful anymore, etc.
newtype DBEnv m a = DBEnv { unDBEnv :: InnerDBEnv m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadPlus, MonadTrans)

instance MonadBase b m => MonadBase b (DBEnv m) where
  liftBase = liftBaseDefault

instance MonadTransControl DBEnv where
  newtype StT DBEnv a = StDBEnv { unStDBEnv :: StT InnerDBEnv a }
  liftWith = defaultLiftWith DBEnv unDBEnv StDBEnv
  restoreT = defaultRestoreT DBEnv unStDBEnv

instance MonadBaseControl b m => MonadBaseControl b (DBEnv m) where
  newtype StM (DBEnv m) a = StMDBEnv { unStMDBEnv :: ComposeSt DBEnv m a }
  liftBaseWith = defaultLiftBaseWith StMDBEnv
  restoreM     = defaultRestoreM unStMDBEnv

mapDBEnv :: (m (a, DBEnvSt) -> n (b, DBEnvSt)) -> DBEnv m a -> DBEnv n b
mapDBEnv f m = withDBEnvSt $ f . runStateT (unDBEnv m)

runDBEnv :: Monad m => DBEnv m a -> m a
runDBEnv f = evalStateT (unDBEnv f) (DBEnvSt Nothing [])

withDBEnvSt :: (DBEnvSt -> m (a, DBEnvSt)) -> DBEnv m a
withDBEnvSt = DBEnv . StateT

-- query typeclasses
class MonadDB m => DBQuery m q r | q -> r where
  query :: q -> DBEnv m r

class MonadDB m => DBUpdate m q r | q -> r where
  update :: q -> DBEnv m r

dbQuery :: DBQuery m q r => q -> m r
dbQuery = runDBEnv . query

dbUpdate :: DBUpdate m q r => q -> m r
dbUpdate = runDBEnv . update

-- | Prepares new SQL query given as string. If there was another
-- query prepared in this monad, it will be finished first.
kPrepare :: MonadDB m => String -> DBEnv m ()
kPrepare q = do
  kFinish
  withDBEnvSt $ \s -> do
    let sqlquery = SQL q []
    conn <- getNexus
    st <- protIO sqlquery $ prepare conn q
    return ((), s { dbStatement = Just st })

-- | Execute recently prepared query. Values given as param serve as
-- positional arguments for SQL query binding. See 'execute' for more
-- details.
kExecute :: MonadDB m => [SqlValue] -> DBEnv m Integer
kExecute values = withDBEnvSt $ \s@DBEnvSt{dbStatement} -> do
  let sqlquery = SQL (getQuery dbStatement) values
  (, s { dbValues = values }) `liftM` case dbStatement of
    Nothing -> liftIO . E.throwIO $ NoStatementPrepared mempty
    Just st -> protIO sqlquery $ execute st values

-- | Execute recently prepared query and check if it returned exactly
-- 1 row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1 :: MonadDB m => [SqlValue] -> DBEnv m ()
kExecute1 values = do
  result <- kExecute values
  when (result /= 1) $ withDBEnvSt $ \DBEnvSt{dbStatement} ->
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }

-- | Execute recently prepared query and check if it returned 0 or 1
-- row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute01 :: MonadDB m => [SqlValue] -> DBEnv m Bool
kExecute01 values = do
  result <- kExecute values
  when (result > 1) $ withDBEnvSt $ \DBEnvSt{dbStatement} ->
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return $ result == 1

-- | Execute recently prepared query and check if it returned 1 or
-- more rows affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1P :: MonadDB m => [SqlValue] -> DBEnv m Integer
kExecute1P values = do
  result <- kExecute values
  when (result < 1) $ withDBEnvSt $ \DBEnvSt{dbStatement} ->
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery dbStatement) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return result

-- | Finish current statement. 'kPrepare' and 'kExecute' call
-- 'kFinish' before they alter query.
kFinish :: MonadDB m => DBEnv m ()
kFinish = withDBEnvSt $ \s@DBEnvSt{..} -> do
  case dbStatement of
    Just st -> (, DBEnvSt { dbStatement = Nothing, dbValues = [] })
      `liftM` protIO (SQL (getQuery dbStatement) dbValues) (finish st)
    Nothing -> return ((), s)

kRunRaw :: MonadDB m => String -> DBEnv m ()
kRunRaw sql = DBEnv $ do
  conn <- getNexus
  protIO mempty $ runRaw conn sql

kDescribeTable :: MonadDB m => String -> DBEnv m [(String, SqlColDesc)]
kDescribeTable table = DBEnv $ getNexus >>= \c -> liftIO (describeTable c table)

withPostgreSQL :: (MonadBaseControl IO m, MonadIO m) => String -> DBT m a -> m a
withPostgreSQL conf m =
  EL.bracket (liftIO $ PG.connectPostgreSQL conf) (liftIO . disconnect) $ \conn -> do
    nex <- mkNexus conn
    res <- runDBT nex m
    liftIO $ commit conn
    return res

-- internals

getQuery :: Maybe Statement -> String
getQuery = fromMaybe "" . fmap HDBC.originalQuery

-- | Protected 'liftIO'. Properly catches 'SqlError' and converts it
-- to 'DBException'. Adds 'HDBC.originalQuery' that should help a lot.
protIO :: MonadDB m => SQL -> IO a -> m a
protIO sql m = liftIO $ m `E.catch` (liftIO . E.throwIO . SQLError sql)
