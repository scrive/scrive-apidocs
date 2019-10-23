module MemCache (
    MemCache
  , new
  , fetch
  , fetch_
  , invalidate
  ) where

import Control.Concurrent.Lifted
import Control.DeepSeq
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Hashable
import Data.Word
import qualified Data.HashMap.Strict as HM
import qualified Data.HashPSQ as Q

data MemCache_ k v = MemCache_ {
    mcSizeFun     :: !(v -> Int)
  , mcSizeLimit   :: !Int
  , mcCurrentSize :: !Int
  , mcTick        :: !Word64
  , mcInProgress  :: !(HM.HashMap k (MVar (Either SomeException v)))
  , mcCache       :: !(Q.HashPSQ k Word64 v)
  }

-- | In-memory LRU cache suitable for storage of immutable objects.
newtype MemCache k v = MemCache (MVar (MemCache_ k v))

-- | Create new memory cache. Supply a memory limit (in bytes) and a sizing
-- function. Key type should have 'Ord' and 'Hashable' instances.
new :: MonadBase IO m => (v -> Int) -> Int -> m (MemCache k v)
new sizeFun sizeLimit = MemCache <$> newMVar MemCache_ { mcSizeFun     = sizeFun
                                                       , mcSizeLimit   = sizeLimit
                                                       , mcCurrentSize = 0
                                                       , mcTick        = 0
                                                       , mcInProgress  = HM.empty
                                                       , mcCache       = Q.empty
                                                       }

-- | Fetch an object from cache or construct it if it's not there. Returned Bool
-- signifies whether constructing action was run.
fetch
  :: forall k v m
   . (Ord k, Hashable k, NFData v, MonadBaseControl IO m)
  => MemCache k v
  -> k
  -> m v
  -> m (v, Bool)
fetch (MemCache mv) key construct = mask $ \release -> do
  -- Note: asynchronous exceptions need to be masked in the following places:
  -- (1) When eel is Left and mvAlreadyThere is False: if asynchronous exception
  -- arrives between first modifyMVar returns and onException handler is
  -- estabilished, we're left with dangling MVar. (2) When constructing action
  -- throws (so the corresponding MVar contains Left), if asynchronous exception
  -- arrives between second modifyMVar returns and onException block is exited,
  -- it may happen that another thread already inserted a different MVar under
  -- the same key, which would've been then removed by onException handler.
  --
  -- Attempt to get a value corresponding to the key from cache. If it's there,
  -- bump its priority and return it. Otherwise either check whether the value
  -- is already being computed and return associated MVar or create a new one.
  eel <- modifyMVar mv $ \mc -> release $ do
    case Q.alter (lookupAndIncreasePriorityTo $ mcTick mc) key $ mcCache mc of
      (Just el, updatedCache) -> do
        let !mc' = mc { mcTick = mcTick mc + 1, mcCache = updatedCache }
        return (mc', Right el)
      (Nothing, _) -> case key `HM.lookup` mcInProgress mc of
        Just mvEl -> return (mc, Left (mvEl, True))
        Nothing   -> do
          mvEl <- newEmptyMVar
          let !mc' = mc { mcInProgress = HM.insert key mvEl $ mcInProgress mc }
          return (mc', Left (mvEl, False))

  case eel of
    Right el                     -> return (el, False)
    Left  (mvEl, mvAlreadyThere) -> do
      -- If we got existing MVar, wait for its result. Otherwise run
      -- constructing action and update cache accordingly.
      if mvAlreadyThere
        then release $ fmap (, False) . throwOrReturn =<< readMVar mvEl
        else do
          evalue <- (`onException` removeInProgress) $ do
            -- Spawn a new thread to avoid catching asynchronous exceptions thrown
            -- to the current thread. After that, construct the value and update
            -- cache accordingly. If at any point during construction the
            -- exception is thrown, use mvEl to propagate it to any other caller
            -- that waits for its result. If at any point in time the current
            -- thread throws (or receives asynchronous exception), remove mvEl
            -- from mcInProgress as at this point it's a dangling MVar.
            void . fork $ putMVar mvEl =<< try (release $ evaluate . force =<< construct)
            evalue <- readMVar mvEl
            modifyMVar_ mv $ \mc -> release $ case evalue of
              Left _ -> do
                return $! mc { mcInProgress = HM.delete key $ mcInProgress mc }
              Right value -> do
                return $! tidyCache $! mc
                  { mcCurrentSize = mcCurrentSize mc + mcSizeFun mc value
                  , mcTick        = mcTick mc + 1
                  , mcInProgress  = HM.delete key $ mcInProgress mc
                  , mcCache       = Q.insert key (mcTick mc) value $ mcCache mc
                  }
            return evalue
          (, True) <$> throwOrReturn evalue
  where
    lookupAndIncreasePriorityTo prio = \case
      Nothing        -> (Nothing, Nothing)
      Just (_, mvEl) -> (Just mvEl, Just (prio, mvEl))

    throwOrReturn = either throwIO return

    removeInProgress :: m ()
    removeInProgress = modifyMVar_ mv
      $ \mc -> return $! mc { mcInProgress = HM.delete key $ mcInProgress mc }

    tidyCache :: MemCache_ k v -> MemCache_ k v
    tidyCache mc = go (mcCache mc) (mcCurrentSize mc)
      where
        go cache size
          | size <= mcSizeLimit mc = mc { mcCurrentSize = size, mcCache = cache }
          | otherwise = case Q.minView cache of
            Nothing ->
              unexpectedError "tidyCache: size limit exceeded even though cache is empty"
            Just (_, _, minEl, tidiedCache) -> go tidiedCache $ size - mcSizeFun mc minEl

-- | Fetch an object from cache or construct it if it's not there.
fetch_
  :: forall k v m
   . (Ord k, Hashable k, NFData v, MonadBaseControl IO m)
  => MemCache k v
  -> k
  -> m v
  -> m v
fetch_ mc key = fmap fst . fetch mc key

-- | Invalidate a key in cache.
invalidate :: (Ord k, Hashable k, MonadBaseControl IO m) => MemCache k v -> k -> m ()
invalidate (MemCache mv) key = modifyMVar_ mv $ \mc ->
  return $! case Q.deleteView key $ mcCache mc of
    Nothing -> mc
    Just (_, value, newCache) ->
      mc { mcCurrentSize = mcCurrentSize mc - mcSizeFun mc value, mcCache = newCache }
