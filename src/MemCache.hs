

-- | MemCache is poor man's memory cache.
--
-- MemCache works in IO monad. When creating MemCache supply sizing
-- function and size limit.  Size of cache is sum of all sizes of
-- objects inside cache.
--
-- Function 'get' returns object in cache when found under key.
--
-- Function 'put' pus an object in cache. If resulting cache size
-- would be larger than limit some object in purged from cache before
-- putting the new one in.

module MemCache (MemCache, new, put, get, size)
where

import Control.Concurrent.MVar
import System.Random (randomRIO)
import qualified Data.Map as Map

data MemCache' k v = MemCache' (v -> Int) Int Int (Map.Map k v)

newtype MemCache k v = MemCache (MVar (MemCache' k v))

-- | Create new memory cache. Supply a memory limit (in bytes) and a
-- sizing function. Key type should have 'Ord' and 'Eq' instances.
new :: (v -> Int) -> Int -> IO (MemCache k v)
new sizefun sizelimit =
    do fmap MemCache $ newMVar (MemCache' sizefun sizelimit 0 Map.empty)

-- | Put a key value pair into cache. Cache will take care of its own
-- size and never cross total size limit of values.
put :: (Ord k) => k -> v -> MemCache k v -> IO ()
put k v (MemCache mc) = do
  modifyMVar_ mc $ \(MemCache' sizefun sizelimit csize mmap) ->
      do
        let newsize = csize + sizefun v
        (mmap', newsize') <- if not (Map.null mmap) && newsize > sizelimit
                             then do
                                 -- now we need to kill one random thing in the cache
                                 -- and reduce cache size by that element size
                                 r <- randomRIO (0,Map.size mmap - 1)
                                 let (_key,e) = Map.elemAt r mmap
                                 return $ (Map.deleteAt r mmap, newsize - sizefun e)
                             else return (mmap, newsize)
        let mmap'' = Map.insert k v mmap'
        let mc' = MemCache' sizefun sizelimit newsize' mmap''

        return mc'


-- | Get a value under a key in cache. This may return 'Nothing' if
-- not found.
get :: (Ord k) => k -> MemCache k v -> IO (Maybe v)
get k (MemCache mc) = do
  withMVar mc $ \_mc@(MemCache' _sizefun _sizelimit _size mmap) -> do
    return $ Map.lookup k mmap

-- | Get current cache size. This is the sum of all objects inside
-- cache.
size :: MemCache k v -> IO Int
size (MemCache mc) = do
  withMVar mc $ \_mc@(MemCache' _sizefun _sizelimit csize _mmap) -> return csize

