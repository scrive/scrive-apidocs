

-- | MemCache is poor man's memory cache.
--
-- MemCache works in IO monad. When creating MemCache supply sizing
-- function and size limit.  Size of cache is sum of all sizes of
-- objects inside cache.
--
-- Function 'get' returns object in cache when found under key.
--
-- Function 'put' pus an object in cache. If resulting cache size
-- would be larger than limit some object (oldest) in purged from cache before
-- putting the new one in.

module MemCache (MemCache, new, put, get, size)
where

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad.Trans
import System.Time (getClockTime, ClockTime)

data MemCache' k v = MemCache' (v -> Int) Int Int (Map.Map k (ClockTime,v))

newtype MemCache k v = MemCache (MVar (MemCache' k v))

-- | Create new memory cache. Supply a memory limit (in bytes) and a
-- sizing function. Key type should have 'Ord' and 'Eq' instances.
new :: MonadIO m =>  (v -> Int) -> Int -> m (MemCache k v)
new sizefun sizelimit =
    liftIO $ do fmap MemCache $ newMVar (MemCache' sizefun sizelimit 0 Map.empty)

-- | Put a key value pair into cache. Cache will take care of its own
-- size and never cross total size limit of values.
put :: (MonadIO m, Ord k, Show k, Show v) => k -> v -> MemCache k v -> m ()
put k v (MemCache mc) = do
  liftIO $ modifyMVar_ mc $ \(MemCache' sizefun sizelimit csize mmap) ->
      do
        now <- getClockTime
        let (mmap', csize') = if (csize + sizefun v > sizelimit && sizefun v > 0)
                                then cleanMap sizefun sizelimit (mmap,csize)
                                else (mmap,csize)
        let mc' = MemCache' sizefun sizelimit (csize' + sizefun v) ( Map.insert k (now,v) mmap')
        return mc'
    where
        cleanMap sizefun sizelimit (m,ms) =
            case (ms + sizefun v > sizelimit `div` 2, Map.foldrWithKey (getOnetoBeRemoved sizefun) Nothing m) of
                 (True,                Just (rk,_,rv)) -> cleanMap sizefun sizelimit  (Map.delete rk m, ms - sizefun rv)
                 _                                   -> (m,ms)
        getOnetoBeRemoved sizefun ok (ot,ov) c@(Just (_,ct,_)) =
            if (sizefun ov > 0 && ot < ct)
                 then Just (ok,ot,ov)
                 else c
        getOnetoBeRemoved sizefun ok (ot,ov) Nothing =
            if (sizefun ov > 0)
                 then Just (ok,ot,ov)
                 else Nothing

-- | Get a value under a key in cache. This may return 'Nothing' if
-- not found.
get :: (MonadIO m, Ord k) => k -> MemCache k v -> m (Maybe v)
get k (MemCache mc) = liftIO $ do
  modifyMVar mc $ \(MemCache' sf sl cs mmap) -> do
      case Map.lookup k mmap of
           Nothing -> return (MemCache' sf sl cs mmap, Nothing)
           Just (_,v) -> do
               now <- getClockTime
               return (MemCache' sf sl cs (Map.insert k (now,v) mmap), Just v)

-- | Get current cache size. This is the sum of all objects inside
-- cache.
size :: MonadIO m => MemCache k v -> m Int
size (MemCache mc) = liftIO $ do
  withMVar mc $ \(MemCache' _ _ csize _) -> return csize

