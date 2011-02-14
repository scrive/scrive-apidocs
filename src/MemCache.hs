
module MemCache (MemCache, new, put, get) 
where

import qualified Data.Map as Map
import Control.Concurrent.MVar


data MemCache' k v = MemCache' (v -> Int) Int (Map.Map k v)

newtype MemCache k v = MemCache (MVar (MemCache' k v))

{- | Create new memory cache. Supply a memory limit (in bytes)
   and a sizing function. Key type should have 'Ord' and 'Eq' instances
 -}
new :: (v -> Int) -> IO (MemCache k v)
new sizefun = do
  fmap MemCache $ newMVar (MemCache' sizefun 0 Map.empty)

{- | Put a key value pair into cache. Cache will take care of its own size
   and never cross total size limit of values.
 -}
put :: (Ord k) => k -> v -> MemCache k v -> IO Bool
put k v (MemCache mc) = do
  modifyMVar mc $ \(MemCache' sizefun size mmap) -> do
               return ((MemCache' sizefun (size + sizefun v) (Map.insert k v mmap)), True)


{- | Get a value under a key in cache. This might return 'Nothing'.
 -}
get :: (Ord k) => k -> MemCache k v -> IO (Maybe v)
get k (MemCache mc) = do
  withMVar mc $ \mc@(MemCache' sizefun size mmap) -> do
             return $ Map.lookup k mmap

