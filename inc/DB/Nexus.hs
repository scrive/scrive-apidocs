{-# LANGUAGE ExistentialQuantification #-}

-- | Module 'Nexus' is designed to aid in performance assurance, in
-- statistics gathering and in debugging of connections. Basically it
-- is a wrapper around 'IConnection' objects.
--
-- After you are done with database you may look up how many resources
-- you have eaten. Currently we measure:
--
-- - Statements executed. Remember that statement execution is
-- synchronous, so costly in latency.
--
-- - Parameters given. Usually this is safe to ignore, but maybe of
-- value to somebody.
--
-- - Rows fetched. Minimize this one at all cost!
--
-- - Values fetched. If you got rows fetched to minimum, maybe it is
-- time to cut out some columns?
--
-- Usual scenario:
--
-- > withPostgreSQL connstring $ \conn -> do
-- >   nexus <- mkNexus
-- >   ioRunDB nexus $ ....
-- >   stats <- getNexusStats nexus
--
-- 'Nexus' is another word for @Connection@ and implements 'IConnection' interface.
--
-- See 'NexusStats' and 'getNexusStats' for details.
--
-- TODO:
--
-- * Make that Nexus hold a connection object, but does the true
-- connecting only when really using it for the first time. That
-- should keep us from connecting just to close the connection a
-- moment later.
--
-- * Wrap every method in 'bracket' and count exceptions thrown.
--
-- * Calculate time spend in database related activities. Watch out
-- with this one: measuring time around miliseconds can be to costly
-- (timer query is costly).
module DB.Nexus (
    Nexus
  , NexusStats(..)
  , mkNexus
  , getNexusStats
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Database.HDBC
import Database.HDBC.Statement

-- | Statistics that a 'Nexus' can gather.
data NexusStats = NexusStats
  { nexusQueries  :: !Int      -- ^ Executed queries. All 'execute',
                               -- 'executeRaw' and 'executeMany' count
                               -- as one as the last one is supposed
                               -- to be optimized.
  , nexusRows     :: !Int      -- ^ All rows that were fetched from
                               -- result set. Does not count rows that
                               -- were delivered but not fetched.
  , nexusValues   :: !Int      -- ^ Sums length of all fetched rows
  , nexusParams   :: !Int      -- ^ Count of all parameters given to
                               -- queries
  }
  deriving (Eq, Ord)

instance Show NexusStats where
    show (NexusStats q r v p) = "queries: " ++ show q ++
                                ", params: " ++ show p ++
                                ", rows: " ++ show r ++
                                ", values: " ++ show v


-- | All zero initial 'NexusStats'.
emptyStats :: NexusStats
emptyStats = NexusStats 0 0 0 0

-- | 'Nexus' is another word for @Connection@ and 'Nexus' should be
-- used as a wrapper around existing connection object. 'Nexus's magic
-- property is that is gathers statistics while it is running.
-- 
-- 'Nexus' is an instance of 'IConnection'.
data Nexus = forall conn . IConnection conn => 
           Nexus { nexusConnection :: conn
                 , nexusStats      :: MVar NexusStats
                 }

-- | Wrap an existing 'IConnection' object into a 'Nexus' statistics
-- counting mechanism'
mkNexus :: (MonadIO m, IConnection conn) => conn -> m Nexus
mkNexus conn = do
  stats <- liftIO $ newMVar emptyStats
  return (Nexus conn stats)

-- | Retrieve current 'NexusStats' from a 'Nexus'. Does not clear
-- stats, so you may use this many times a day.
getNexusStats :: MonadIO m => Nexus -> m NexusStats
getNexusStats = liftIO . readMVar . nexusStats

instance IConnection Nexus where
    disconnect Nexus{nexusConnection=conn} = disconnect conn

    commit Nexus{nexusConnection=conn} = commit conn
    rollback Nexus{nexusConnection=conn} = rollback conn

    run nexus@Nexus{nexusConnection=conn} command values = do
      modifyMVar_ (nexusStats nexus) $ \stats ->
        return $! stats { nexusParams = nexusParams stats + length values }
      run conn command values
 
    prepare nexus@Nexus{nexusConnection=conn} command = do
      st <- prepare conn command
      return $ Statement 
             { execute = \values -> do
                           modifyMVar_ (nexusStats nexus) $ \stats ->
                             return $! stats { nexusParams  = nexusParams stats + length values
                                   , nexusQueries = nexusQueries stats + 1
                                   }
                           execute st values
             , executeMany = \values -> do
                           modifyMVar_ (nexusStats nexus) $ \stats ->
                             return $! stats { nexusParams  = nexusParams stats + sum (map length values)
                                   , nexusQueries = nexusQueries stats + 1
                                   }
                           executeMany st values
             , executeRaw = do
                           modifyMVar_ (nexusStats nexus) $ \stats ->
                             return $! stats { nexusQueries  = nexusQueries stats + 1
                                   }
                           executeRaw st
             , finish = finish st
             , fetchRow = do
                 mrow <- fetchRow st
                 case mrow of
                   Just row -> 
                     modifyMVar_ (nexusStats nexus) $ \stats ->
                       return $! stats { nexusValues = nexusValues stats + length row
                             , nexusRows   = nexusRows stats + 1
                             }
                   Nothing -> return ()
                 return mrow
             , getColumnNames = getColumnNames st
             , originalQuery = originalQuery st
             , describeResult = describeResult st
             }

    clone Nexus{nexusConnection=conn} = do
      c2 <- clone conn
      s2 <- newMVar emptyStats
      return (Nexus c2 s2)

    hdbcDriverName Nexus{nexusConnection=conn} = hdbcDriverName conn
    hdbcClientVer Nexus{nexusConnection=conn} = hdbcClientVer conn
    proxiedClientName Nexus{nexusConnection=conn} = proxiedClientName conn
    proxiedClientVer Nexus{nexusConnection=conn} = proxiedClientVer conn
    dbServerVer Nexus{nexusConnection=conn} = dbServerVer conn
    dbTransactionSupport Nexus{nexusConnection=conn} = dbTransactionSupport conn
    getTables Nexus{nexusConnection=conn} = getTables conn
    describeTable Nexus{nexusConnection=conn} table = describeTable conn table
