module Control.Concurrent.Thread.Group.Lifted
  ( new, nrOfRunning, wait, fork, forkOS, forkOn,
    forkIOWithUnmask, forkOnWithUnmask,
    TG.ThreadGroup
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.Thread (Result)
import Control.Monad.Base
import Control.Monad.Trans.Control
import qualified Control.Concurrent.Thread.Group as TG

new :: (MonadBase IO m) => m TG.ThreadGroup
new = liftBase TG.new

nrOfRunning :: TG.ThreadGroup -> STM Int
nrOfRunning = TG.nrOfRunning

wait :: (MonadBase IO m) => TG.ThreadGroup -> m ()
wait tg = liftBase (TG.wait tg)


fork :: forall m α . (MonadBaseControl IO m)
     => TG.ThreadGroup -> m α
     -> m (ThreadId, m (Result α))
fork tg action = liftBaseWith (\runInBase -> TG.forkIO tg (runInBase action) >>= fixTypes)
  where
    fixTypes :: (ThreadId, IO (Result (StM m α))) -> IO (ThreadId, m (Result α))
    fixTypes = return . fmap f
    f :: IO (Result (StM m α)) -> m (Result α)
    f c = liftBase c >>= mapMEither restoreM

forkOS :: forall m α . (MonadBaseControl IO m) => TG.ThreadGroup -> m α -> m (ThreadId, m (Result α))
forkOS tg action = liftBaseWith (\runInBase -> TG.forkOS tg (runInBase action) >>= fixTypes)
  where
    fixTypes :: (ThreadId, IO (Result (StM m α))) -> IO (ThreadId, m (Result α))
    fixTypes = return . fmap f
    f :: IO (Result (StM m α)) -> m (Result α)
    f c = liftBase c >>= mapMEither restoreM

forkOn :: forall m α . (MonadBaseControl IO m) => Int -> TG.ThreadGroup -> m α -> m (ThreadId, m (Result α))
forkOn i tg action = liftBaseWith (\runInBase -> TG.forkOn i tg (runInBase action) >>= fixTypes)
  where
    fixTypes :: (ThreadId, IO (Result (StM m α))) -> IO (ThreadId, m (Result α))
    fixTypes = return . fmap f
    f :: IO (Result (StM m α)) -> m (Result α)
    f c = liftBase c >>= mapMEither restoreM

forkOnWithUnmask :: forall m α . (MonadBaseControl IO m)
                 => Int -> TG.ThreadGroup -> ((forall β. m β -> m β) -> m α) -> m (ThreadId, m (Result α))
forkOnWithUnmask i tg action = liftBaseWith (\runInBase -> TG.forkOnWithUnmask i tg (\unmask -> runInBase (action (liftBaseOp_ unmask))) >>= fixTypes)
  where
    fixTypes :: (ThreadId, IO (Result (StM m α))) -> IO (ThreadId, m (Result α))
    fixTypes = return . fmap f
    f :: IO (Result (StM m α)) -> m (Result α)
    f c = liftBase c >>= mapMEither restoreM

forkIOWithUnmask :: forall m α . (MonadBaseControl IO m)
                 => TG.ThreadGroup -> ((forall β. m β -> m β) -> m α) -> m (ThreadId, m (Result α))
forkIOWithUnmask tg action = liftBaseWith (\runInBase -> TG.forkIOWithUnmask tg (\unmask -> runInBase (action (liftBaseOp_ unmask))) >>= fixTypes)
  where
    fixTypes :: (ThreadId, IO (Result (StM m α))) -> IO (ThreadId, m (Result α))
    fixTypes = return . fmap f
    f :: IO (Result (StM m α)) -> m (Result α)
    f c = liftBase c >>= mapMEither restoreM


-- mapMEither is the missing instance of Traversable for Either
mapMEither :: (Monad m) => (a -> m b) -> Either c a -> m (Either c b)
mapMEither f (Right r) = f r >>= \v -> return (Right v)
mapMEither _ (Left v) = return (Left v)
