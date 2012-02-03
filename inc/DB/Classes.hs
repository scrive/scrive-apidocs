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
  ( DBQuery(..)
  , DBUpdate(..)
  , DBMonad(..)
  , DB
  , wrapDB
  , ioRunDB
  , runDB
  , runDBQuery
  , runDBUpdate
  , DBException(..)
  , catchDB
  , tryDB
  , getStatement
  , kPrepare
  , kExecute
  , kExecute1
  , kExecute01
  , kExecute1P
  , kFinish
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Maybe
import Database.HDBC hiding (originalQuery)
import qualified Control.Exception as E
import qualified Database.HDBC as HDBC

import DB.Exception
import DB.Nexus

type DBInside a = RWST Nexus () (Maybe Statement) IO a

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
getStatement :: DB (Maybe Statement)
getStatement = DB get

-- | Prepares new SQL query given as string. If there was another
-- query prepared in this monad, it will be finished first.
kPrepare :: String -> DB ()
kPrepare command = DB $ do
  conn <- ask
  statement <- protIO command $ prepare conn command
  oldstatement <- get
  put (Just statement)
  case oldstatement of
    Just st -> protIO (HDBC.originalQuery st) $ finish st
    Nothing -> return ()

-- | Execute recently prepared query. Values given as param serve as
-- positional arguments for SQL query binding. See 'execute' for more
-- details.
kExecute :: [SqlValue] -> DB Integer
kExecute values = DB $ do
  mstatement <- get
  case mstatement of
    Nothing -> return $ -1 -- no statment prepared
    Just statement -> protIO (HDBC.originalQuery statement) $ execute statement values

-- | Execute recently prepared query and check if it returned exactly
-- 1 row affected result. Useful for INSERT, DELETE or UPDATE
-- statements. Watch out for RETURNING clauses though: they make
-- everything return 0.
kExecute1 :: [SqlValue] -> DB ()
kExecute1 values = do
  result <- kExecute values
  when (result /= 1) $ DB $ do
    mst <- get
    E.throw TooManyObjects {
        originalQuery = fromMaybe "" (fmap HDBC.originalQuery mst)
      , queryParams = values
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
    mst <- get
    E.throw TooManyObjects {
        originalQuery = fromMaybe "" (fmap HDBC.originalQuery mst)
      , queryParams = values
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
    mst <- get
    E.throw TooManyObjects {
        originalQuery = fromMaybe "" (fmap HDBC.originalQuery mst)
      , queryParams = values
      , tmoExpected = 1
      , tmoGiven = result
    }
  return result

-- | Finish current statement. 'kPrepare' and 'kExecute' call
-- 'kFinish' before they alter query.
kFinish :: DB ()
kFinish = DB $ do
  oldstatement <- get
  case oldstatement of
    Just st -> do
      put Nothing
      protIO (HDBC.originalQuery st) $ finish st
    Nothing -> return ()

-- | Protected 'liftIO'. Properly catches 'SqlError' and converts it
-- to 'DBException'. Adds 'HDBC.originalQuery' that should help a lot.
protIO :: String -> IO a -> DBInside a
protIO query action = do
  conn <- ask
  liftIO $ actionWithCatch conn
  where
    setOriginalQuery e = SQLError query [] e
    actionWithCatch conn = do
      action `E.catch` \e -> do
        -- for some unknown reason we need to do rollback here
        -- ourselves otherwise something underneath will try to issue
        -- some commands and those will fail with 'transaction
        -- aborted, ignoring commands till the end of the block'
        rollback conn
        E.throwIO (setOriginalQuery e)

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
  getConnection :: m Nexus
  -- | From the point of view of the implementor of instances of
  -- 'DBMonad': handle a database error.  However, code that uses
  -- 'handleDBError' throws an exception - compare with 'throw', or
  -- 'fail'.
  handleDBError :: DBException -> m a

-- | Wraps IO action in DB
wrapDB :: (Nexus -> IO a) -> DB a
wrapDB f = DB ask >>= liftIO . f

runit :: (MonadIO m) => DB a -> Nexus -> m a
runit action conn = do
  let actionWithFinish = (action >>= \result -> kFinish >> return result)
  (a,_,_) <- liftIO $ runRWST (unDB actionWithFinish) conn Nothing
  return a

-- | Runs DB action in single transaction (IO monad). Note that since
-- it runs in IO, you need to pass Connecion object explicitly. Also,
-- sql releated exceptions are not handled, so you probably need to do
-- it yourself. Use this function ONLY if there is no way to use runDB.
ioRunDB :: MonadIO m => Nexus -> DB a -> m a
ioRunDB conn = liftIO . withTransaction conn . runit

-- | Runs DB action in a DB compatible monad
runDB :: DBMonad m => DB a -> m a
runDB f = do
  conn <- getConnection
  res <- liftIO $ (Right <$> withTransaction conn (runit f)) `E.catches` handlers
  case res of
    Right r -> return r
    Left e  -> handleDBError e
  where
    -- we catch only SqlError/DBException here
    handlers = [
        E.Handler (return . Left . SQLError "<unknown SQL query>" [])
      , E.Handler (return . Left)
      ]

-- | Runs single db query (provided for convenience).
runDBQuery :: (DBMonad m, DBQuery q r) => q -> m r
runDBQuery = runDB . dbQuery

-- | Runs single db update (provided for convenience).
runDBUpdate :: (DBMonad m, DBUpdate q r) => q -> m r
runDBUpdate = runDB . dbUpdate

-- | Catch in DB monad
catchDB :: E.Exception e => DB a -> (e -> DB a) -> DB a
catchDB f exhandler = wrapDB $ \conn -> do
  let handler e = runit (exhandler e) conn
  liftIO $ runit f conn `E.catch` handler

-- | Try in DB monad
tryDB :: E.Exception e => DB a -> DB (Either e a)
tryDB f = wrapDB $ liftIO . E.try . runit f
