{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.Core (
    DBT(..)
  , mapDBT
  , runDBT
  , MonadDB(..)
  , DBEnvSt(..)
  , protIO
  , DBQuery(..)
  , DBUpdate(..)
  , dbQuery
  , dbUpdate
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Control.Monad.RWS.Strict
import Happstack.Server
import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW
import qualified Text.StringTemplates.Templates as T

import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB.Nexus
import Log
import Database.HDBC
import qualified Control.Exception.Lifted as E
import DB.SQL (RawSQL, SQL(..), unRawSQL, unsafeFromString)
import DB.Exception
import Data.Maybe
import System.Time (getClockTime)
import MinutesTime

data DBEnvSt = DBEnvSt {
    dbStatement :: !(Maybe Statement)
  , dbValues    :: ![SqlValue]
  }

type InnerDBT = RWST Nexus () DBEnvSt

newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadPlus, MonadTrans, MonadLog)



mapDBT :: (Monad m, Monad n) => (m (a, DBEnvSt) -> n (b, DBEnvSt)) -> DBT m a -> DBT n b
mapDBT f m = DBT $ RWST $ \nexus st -> do
  deshuffle $ f $ enshuffle $ runRWST (unDBT m) nexus st
  where
    enshuffle = liftM (\(a,st',_) -> (a,st'))
    deshuffle = liftM (\(b,st'') -> (b,st'',()))

runDBT :: (Monad m) => Nexus -> DBEnvSt -> DBT m a -> m a
runDBT env st f = evalRWST (unDBT f) env st >>= return . fst

instance MonadTransControl DBT where
  newtype StT DBT a = StDBT { unStDBT :: StT InnerDBT a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

{-
instance MonadError e m => MonadError e (DBT m) where
  throwError     = lift . throwError
  catchError m h = DBT $ RWST (\nexus st -> catchError (runDBT nexus st m) (runDBT nexus st . h)
-}

instance MonadReader r m => MonadReader r (DBT m) where
  ask     = lift ask
  local f = mapDBT $ local f

instance MonadState s m => MonadState s (DBT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (DBT m) where
  tell   = lift . tell
  listen = DBT . helper . unDBT
           where
             shuffle ((a',s',w'),q) = ((a',q),s',w')
             helper k = RWST $ \r s -> do
                         liftM shuffle $ listen $ runRWST k r s

  pass   = DBT . helper . unDBT
           where
             shuffle ((a',q),s',w') = ((a',s',w'),q)
             helper k = RWST $ \r s -> do
                         pass $ liftM shuffle $ runRWST k r s


-- Happstack specific instances, to be moved somewhere else

instance FilterMonad f m => FilterMonad f (DBT m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter     = DBT . helper . unDBT
           where
             shuffle ((a',s',w'),q) = ((a',q),s',w')
             helper k = RWST $ \r s -> liftM shuffle . getFilter $ runRWST k r s

instance (HasRqData m, Monad m) => HasRqData (DBT m) where
  askRqEnv     = lift askRqEnv
  localRqEnv f = mapDBT $ localRqEnv f
  rqDataError  = lift . rqDataError

instance ServerMonad m => ServerMonad (DBT m) where
  askRq     = lift askRq
  localRq f = mapDBT $ localRq f

instance WebMonad r m => WebMonad r (DBT m) where
  finishWith = lift . finishWith


-- | Class that allows access to database.
--
-- All functions that want to access database should be in a monad of
-- this type. Good basis is 'DBT' as it manages mutable and unmutable
-- state of the database.
--
-- Functions for running SQL queries are 'kRunSQL', 'kFinish',
-- 'kFold2'.
--
-- Data management is in 'kGetTables' and 'kDescribeTable'.
--
-- The 'kCommit' and 'kRollback' should end up in separate classes on
-- their own because we do not want a function to mess up transactions
-- just when they want. Transaction management should be done at top
-- level and only sometimes at very specific points.
--
-- 'kThrow' is there so that throwIO is lifted into MonadDB instances.
--
-- The 'getNexus' and 'localNexus' are bits of wart of history. The
-- point of this monad class is to not allow direct access to
-- connection object so those two functions defeat this
-- purpose. Please do not use them. The only place when they are used
-- is in connection with 'forkIO' when monad stack needs to be
-- preserved with nexus instance swapped for new connection. That
-- place in code is in dire need of better abstraction anyways.
class (Functor m, {- MonadIO m, MonadBase IO m, -} MonadLog m) => MonadDB m where
  getNexus   :: m Nexus
  localNexus :: (Nexus -> Nexus) -> m a -> m a
  kCommit    :: m ()
  kRollback  :: m ()
  kClone     :: m Nexus
  kRunSQL    :: SQL -> m Integer
  kFinish    :: m ()
  kGetTables :: m [String]
  kDescribeTable :: RawSQL -> m [(String, SqlColDesc)]
  kFold2     :: (a -> [SqlValue] -> Either SQLError a) -> a -> m a
  kThrow     :: (E.Exception e) => e -> m a
  getMinutesTime :: m MinutesTime

getQuery :: Statement -> RawSQL
getQuery = unsafeFromString . Database.HDBC.originalQuery

instance (Functor m, MonadIO m, MonadBase IO m, MonadLog m) => MonadDB (DBT m) where
  getNexus     = DBT ask
  localNexus f = DBT . local f . unDBT
  kCommit      = DBT $ ask >>= liftIO . commit
  kRollback    = DBT $ ask >>= liftIO . rollback
  kClone       = DBT $ ask >>= liftIO . clone
  getMinutesTime = DBT $ liftIO $ (return . fromClockTime) =<< getClockTime
  kRunSQL sqlquery@(SQL query' values) =
    (kFinish >>) $ DBT $ do
      conn <- ask
      (st,rs) <- protIO conn sqlquery $ do
        st <- prepare conn (unRawSQL query')
        -- FIXME: if execute throws something we need to finish
        -- statement after failed execute
        rs <- execute st values
        return (st,rs)
      modify (\s -> s { dbStatement = Just st, dbValues = values })
      return rs

  kFinish      = DBT $ do
    DBEnvSt {dbStatement,dbValues} <- get
    case dbStatement of
      Just st -> do
        conn <- ask
        put $ DBEnvSt { dbStatement = Nothing, dbValues = [] }
        protIO conn (SQL (getQuery st) dbValues) (finish st)
      Nothing -> return ()
  kGetTables = DBT $ ask >>= liftIO . getTables
  kDescribeTable table = DBT $ ask >>= \c -> liftIO (describeTable c (unRawSQL table))
  kThrow = DBT . E.throwIO
  kFold2 decoder !init_acc = DBT $ do
    DBEnvSt {dbStatement,dbValues} <- get
    case dbStatement of
      Nothing -> return $ init_acc
      Just st -> do
        res <- liftIO (worker st init_acc)
        unDBT $ kFinish
        case res of
          Right acc -> do
            return acc
          Left err -> do
            liftIO $ E.throwIO err { DB.Exception.originalQuery = SQL (getQuery st) dbValues }
    where
     worker st !acc = do
      mrow <- fetchRow st
      case mrow of
        Nothing -> return $ Right acc
        Just row ->
          case decoder acc row of
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


instance MonadDB m => MonadDB (CryptoRNGT m) where
  getNexus     = lift getNexus
  localNexus f = mapCryptoRNGT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (MonadDB m) => MonadDB (ReaderT r m) where
  getNexus     = lift getNexus
  localNexus f = mapReaderT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (ContT r m) where
  getNexus     = lift getNexus
  localNexus f = mapContT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (Error e, MonadDB m) => MonadDB (ErrorT e m) where
  getNexus     = lift getNexus
  localNexus f = mapErrorT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (IdentityT m) where
  getNexus     = lift getNexus
  localNexus f = mapIdentityT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (ListT m) where
  getNexus     = lift getNexus
  localNexus f = mapListT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (MaybeT m) where
  getNexus     = lift getNexus
  localNexus f = mapMaybeT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (SS.StateT s m) where
  getNexus     = lift getNexus
  localNexus f = SS.mapStateT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance MonadDB m => MonadDB (LS.StateT s m) where
  getNexus     = lift getNexus
  localNexus f = LS.mapStateT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (MonadDB m, Monoid w) => MonadDB (LW.WriterT w m) where
  getNexus     = lift getNexus
  localNexus f = LW.mapWriterT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (MonadDB m, Monoid w) => MonadDB (SW.WriterT w m) where
  getNexus     = lift getNexus
  localNexus f = SW.mapWriterT $ localNexus f
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (MonadBase IO (T.TemplatesT m), MonadDB m) => MonadDB (T.TemplatesT m) where
  getNexus = lift getNexus
  localNexus f (T.TemplatesT m) = T.TemplatesT $ ReaderT $ localNexus f . runReaderT m
  kCommit      = lift kCommit
  kRollback    = lift kRollback
  kClone       = lift kClone
  kRunSQL      = lift . kRunSQL
  kFinish      = lift kFinish
  kThrow       = lift . kThrow
  kDescribeTable  = lift . kDescribeTable
  kGetTables      = lift kGetTables
  kFold2 decoder init_acc = lift (kFold2 decoder init_acc)
  getMinutesTime = lift getMinutesTime

instance (MonadLog m) => MonadLog (T.TemplatesT m) where
  logM a b c = lift $ logM a b c

-- | Protected 'liftIO'. Properly catches 'SqlError' and converts it
-- to 'DBException'. Adds 'HDBC.originalQuery' that should help a lot.
protIO :: (MonadIO m) => Nexus -> SQL -> IO a -> m a
protIO conn s m = do
  liftIO $ m `E.catch` (\e -> do
        -- for some unknown reason we need to do rollback here
        -- ourselves otherwise something underneath will try to issue
        -- some commands and those will fail with 'transaction
        -- aborted, ignoring commands till the end of the block'
        rollback conn
        E.throwIO $ SQLError s e)

-- query typeclasses
class MonadDB m => DBQuery m q r | q -> r where
  query :: q -> m r

class MonadDB m => DBUpdate m q r | q -> r where
  update :: q -> m r

dbQuery :: DBQuery m q r => q -> m r
dbQuery = query

dbUpdate :: DBUpdate m q r => q -> m r
dbUpdate = update
