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

module DB.Classes
  ( module DB.Core
  , DBQuery(..)
  , DBUpdate(..)
  , DBState(..)
  , DB
  , mkDBEnv
  , cloneDBEnv
  , ioRunDB
  , withPostgreSQL_
  , withPostgreSQLDB
  , withPostgreSQLDB'
  , runDB
  , runDBQuery
  , runDBUpdate
  , getDBState
  , DBException(..)
  , catchDB
  , tryDB
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
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.RWS
import Crypto.RNG (CryptoRNG, CryptoRNGState, getCryptoRNGState)
import Data.Maybe
import Database.HDBC hiding (originalQuery)
import Database.HDBC.PostgreSQL
import qualified Control.Exception as E
import qualified Control.Exception.Lifted as EE
import qualified Database.HDBC as HDBC

import DB.Core (DBEnv(..), DBT, MonadDB(..), runDBT)
import DB.Exception
import DB.Nexus
import DB.SQL

type DBInside a = RWST DBEnv () DBState IO a

data DBState = DBState {
    dbStatement :: Maybe Statement
  , dbValues    :: [SqlValue]
  }

mkDBEnv :: (MonadIO m, IConnection conn) => conn -> CryptoRNGState -> m DBEnv
mkDBEnv conn rng = DBEnv rng `liftM` mkNexus conn

cloneDBEnv :: DBEnv -> IO DBEnv
cloneDBEnv env = DBEnv (envRNG env) `liftM` HDBC.clone (envNexus env)

-- | 'DB' is a monad that wraps access to database. It hold onto
-- Connection and current statement. Manages to handle exceptions
-- correctly, closes statement when it is not useful anymore, etc.
--
-- To keep privacy internals of this monad are not exported. There
-- should not be MonadIO instance either. We can think about
-- MonadRandom instance though.
--
-- Wrapper for calling db related functions in controlled environment
-- (DB/unDB is not exposed so functions may enter DB wrapper, but they
-- can't escape it in any other way than by runDB function).
--
-- In future we are going to remove the MonadIO instance so that DB
-- does not cause troublesome side effects.
--
-- Note: Do NOT try to provide DBMonad instance for DB. DBMonad instance
-- allows you to call runDB function, which executes a statement within
-- transaction. But, since we are in DB monad, we are already in such
-- transaction! And because there is no such thing as nested transactions,
-- this basically kills its 'transactional' character.
newtype DB a = DB { unDB :: DBInside a }
  deriving (Applicative, Functor, Monad, MonadIO)


-- | Get current statement (if any is prepared)
getDBState :: DB DBState
getDBState = DB get

instance CryptoRNG DB where
  getCryptoRNGState = DB $ asks envRNG

-- | Prepares new SQL query given as string. If there was another
-- query prepared in this monad, it will be finished first.
kPrepare :: String -> DB ()
kPrepare query = DB $ do
  let sqlquery = SQL query []
  conn <- asks envNexus
  statement <- protIO sqlquery $ prepare conn query
  oldstatement <- dbStatement <$> get
  modify $ \s -> s { dbStatement = Just statement }
  case oldstatement of
    Just st -> protIO sqlquery $ finish st
    Nothing -> return ()

-- | Execute recently prepared query. Values given as param serve as
-- positional arguments for SQL query binding. See 'execute' for more
-- details.
kExecute :: [SqlValue] -> DB Integer
kExecute values = DB $ do
  state@DBState{dbStatement} <- get
  let sqlquery = SQL (getQuery dbStatement) values
  put $ state { dbValues = values }
  case dbStatement of
    Nothing -> return $ -1 -- no statement prepared
    Just st -> protIO sqlquery $ execute st values

-- | Execute recently prepared query and check if it returned exactly
-- 1 row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1 :: [SqlValue] -> DB ()
kExecute1 values = do
  result <- kExecute values
  when (result /= 1) $ DB $ do
    mst <- dbStatement <$> get
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery mst) values
      , tmoExpected = 1
      , tmoGiven = result
    }

-- | Execute recently prepared query and check if it returned 0 or 1
-- row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute01 :: [SqlValue] -> DB Bool
kExecute01 values = do
  result <- kExecute values
  when (result > 1) $ DB $ do
    mst <- dbStatement <$> get
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery mst) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return $ result == 1

-- | Execute recently prepared query and check if it returned 1 or
-- more rows affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1P :: [SqlValue] -> DB Integer
kExecute1P values = do
  result <- kExecute values
  when (result < 1) $ DB $ do
    mst <- dbStatement <$> get
    E.throw TooManyObjects {
        originalQuery = SQL (getQuery mst) values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return result

-- | Finish current statement. 'kPrepare' and 'kExecute' call
-- 'kFinish' before they alter query.
kFinish :: DB ()
kFinish = DB $ do
  DBState{..} <- get
  case dbStatement of
    Just st -> do
      put $ DBState { dbStatement = Nothing, dbValues = [] }
      protIO (SQL (getQuery dbStatement) dbValues) $ finish st
    Nothing -> return ()

kRunRaw :: String -> DB ()
kRunRaw query = DB $ do
  conn <- asks envNexus
  protIO (SQL query []) $ runRaw conn query

getQuery :: Maybe Statement -> String
getQuery mst = fromMaybe "" $ HDBC.originalQuery `fmap` mst

-- | Protected 'liftIO'. Properly catches 'SqlError' and converts it
-- to 'DBException'. Adds 'HDBC.originalQuery' that should help a lot.
protIO :: SQL -> IO a -> DBInside a
protIO query action = do
  conn <- asks envNexus
  liftIO $ actionWithCatch conn
  where
    actionWithCatch conn = do
      action `E.catch` \e -> do
        -- for some unknown reason we need to do rollback here
        -- ourselves otherwise something underneath will try to issue
        -- some commands and those will fail with 'transaction
        -- aborted, ignoring commands till the end of the block'
        rollback conn
        E.throwIO $ SQLError query e

-- query typeclasses
class DBQuery q r | q -> r where
  dbQuery :: q -> DB r

class DBUpdate q r | q -> r where
  dbUpdate :: q -> DB r

-- | DBMonad class. getDBEnv function is used for getting connection
-- from underlying state object (obviously). If it comes to handleDBError,
-- it's for handling hard db errors like lost connection, sql errors of
-- all kind or the fact that query did something that should never happen.
-- In such case we want to interrupt what we're currently doing and exit
-- gracefully, most likely logging an error is some way. Since a way of
-- handling such case may vary in different monads, hence this function.

runit :: MonadIO m => DB a -> DBEnv -> m a
runit action env = do
  let actionWithFinish = (action >>= \result -> kFinish >> return result)
  (a, _, _) <- liftIO $ runRWST (unDB actionWithFinish) env
    DBState { dbStatement = Nothing, dbValues = [] }
  return a

-- | Runs DB action in single transaction (IO monad). Note that since
-- it runs in IO, you need to pass Connecion object explicitly. Also,
-- sql releated exceptions are not handled, so you probably need to do
-- it yourself. Use this function ONLY if there is no way to use runDB.
ioRunDB :: MonadIO m => DBEnv -> DB a -> m a
ioRunDB env f = runit f env

withPostgreSQL_ :: (MonadBaseControl IO m, MonadIO m) => String -> CryptoRNGState -> DBT m a -> m a
withPostgreSQL_ conf rng m =
  EE.bracket (liftIO $ connectPostgreSQL conf) (liftIO . disconnect) $ \conn -> do
    (res, env) <- mkDBEnv conn rng >>= \env -> runDBT env m
    liftIO $ commit $ envNexus env
    return res

withPostgreSQLDB' :: String -> CryptoRNGState -> (DBEnv -> IO a) -> IO a
withPostgreSQLDB' cs rng f = withPostgreSQL cs $ \conn ->
  mkDBEnv conn rng >>= f

withPostgreSQLDB :: String -> CryptoRNGState -> DB a -> IO a
withPostgreSQLDB cs rng f = withPostgreSQLDB' cs rng (flip ioRunDB f)


-- | Runs DB action in a DB compatible monad
runDB :: MonadDB m => DB a -> m a
runDB f = do
  env <- getDBEnv
  res <- liftIO $ (Right <$> ioRunDB env f) `E.catches` handlers
  case res of
    Right r -> return r
    Left e  -> liftIO $ E.throwIO e
  where
    -- we catch only SqlError/DBException here
    handlers = [
        E.Handler (return . Left . SQLError (SQL "<unknown SQL query>" []))
      , E.Handler (return . Left)
      ]

-- | Runs single db query (provided for convenience).
runDBQuery :: (MonadDB m, DBQuery q r) => q -> m r
runDBQuery = runDB . dbQuery

-- | Runs single db update (provided for convenience).
runDBUpdate :: (MonadDB m, DBUpdate q r) => q -> m r
runDBUpdate = runDB . dbUpdate

-- | Catch in DB monad
catchDB :: E.Exception e => DB a -> (e -> DB a) -> DB a
catchDB f exhandler = DB $ do
  env <- ask
  let handler e = runit (exhandler e) env
  liftIO $ runit f env `E.catch` handler

-- | Try in DB monad
tryDB :: E.Exception e => DB a -> DB (Either e a)
tryDB f = DB $ ask >>= liftIO . E.try . runit f

kDescribeTable :: String -> DB [(String, SqlColDesc)]
kDescribeTable table = DB (asks envNexus) >>= \c -> liftIO (describeTable c table)
