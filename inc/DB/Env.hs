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
module DB.Env (
    DBEnvSt(..)
  , DBEnv
  , mapDBEnv
  , runDBEnv
  , withDBEnv
  , withDBEnvSt
  , DBQuery(..)
  , DBUpdate(..)
  , dbQuery
  , dbUpdate
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (state)
import Control.Monad.Trans.Control
import Database.HDBC hiding (originalQuery)

import Control.Monad.Trans.Control.Util
import DB.Core

data DBEnvSt = DBEnvSt {
    dbStatement :: !(Maybe Statement)
  , dbValues    :: ![SqlValue]
  }

type InnerDBEnv = StateT DBEnvSt

-- | 'DBEnv' is a monad transformer that adds state management of executed
-- database action, handle exceptions correctly, closes statement when it
-- is not useful anymore, etc.
newtype DBEnv m a = DBEnv { unDBEnv :: InnerDBEnv m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadPlus, MonadTrans)

instance MonadTransControl DBEnv where
  newtype StT DBEnv a = StDBEnv { unStDBEnv :: StT InnerDBEnv a }
  liftWith = defaultLiftWith DBEnv unDBEnv StDBEnv
  restoreT = defaultRestoreT DBEnv unStDBEnv
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (DBEnv m) where
  newtype StM (DBEnv m) a = StMDBEnv { unStMDBEnv :: ComposeSt DBEnv m a }
  liftBaseWith = defaultLiftBaseWith StMDBEnv
  restoreM     = defaultRestoreM unStMDBEnv
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

mapDBEnv :: (m (a, DBEnvSt) -> n (b, DBEnvSt)) -> DBEnv m a -> DBEnv n b
mapDBEnv f m = withDBEnvSt $ f . runStateT (unDBEnv m)

runDBEnv :: Monad m => DBEnv m a -> m a
runDBEnv f = evalStateT (unDBEnv f) (DBEnvSt Nothing [])

withDBEnv :: Monad m => (DBEnvSt -> m a) -> DBEnv m a
withDBEnv m = withDBEnvSt $ \s -> (, s) `liftM` m s

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
