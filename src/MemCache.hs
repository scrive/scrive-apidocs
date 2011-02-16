
module MemCache (MemCache, new, put, get) 
where

import qualified Data.Map as Map
import Control.Concurrent.MVar
import System.Random

data MemCache' k v = MemCache' (v -> Int) Int Int (Map.Map k v)

newtype MemCache k v = MemCache (MVar (MemCache' k v))

{- | Create new memory cache. Supply a memory limit (in bytes)
   and a sizing function. Key type should have 'Ord' and 'Eq' instances
 -}
new :: (v -> Int) -> Int -> IO (MemCache k v)
new sizefun sizelimit = do
  fmap MemCache $ newMVar (MemCache' sizefun sizelimit 0 Map.empty)

{- | Put a key value pair into cache. Cache will take care of its own size
   and never cross total size limit of values.
 -}
put :: (Ord k) => k -> v -> MemCache k v -> IO ()
put k v (MemCache mc) = do
  modifyMVar mc $ \(MemCache' sizefun sizelimit size mmap) -> 
      do
        let newsize = size + sizefun v
        mmap' <- if not (Map.null mmap) && newsize > sizelimit
                 then do
                   -- now we need to kill one random thing in the cache
                   r <- randomRIO (0,Map.size mmap - 1)
                   return $ Map.deleteAt r mmap
                 else return mmap
        let mmap'' = Map.insert k v mmap
        let mc' = MemCache' sizefun sizelimit newsize mmap''
        
        return (mc', ())


{- | Get a value under a key in cache. This might return 'Nothing'.
 -}
get :: (Ord k) => k -> MemCache k v -> IO (Maybe v)
get k (MemCache mc) = do
  withMVar mc $ \mc@(MemCache' sizefun sizelimit size mmap) -> do
             return $ Map.lookup k mmap

