module MemCache (
    MemCache
  , new
  , fetch
  ) where

import Control.Concurrent.Lifted
import Control.DeepSeq
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Hashable
import Data.Word
import qualified Control.Concurrent.Thread.Lifted as LT
import qualified Data.HashMap.Strict as HM
import qualified Data.HashPSQ as Q

import KontraPrelude

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
new sizeFun sizeLimit = MemCache <$> newMVar MemCache_ {
    mcSizeFun     = sizeFun
  , mcSizeLimit   = sizeLimit
  , mcCurrentSize = 0
  , mcTick        = 0
  , mcInProgress  = HM.empty
  , mcCache       = Q.empty
  }

-- | Fetch an object from cache or construct it if it's not there.
fetch
  :: forall k v m. (Ord k, Hashable k, NFData v, MonadBaseControl IO m)
  => MemCache k v
  -> k
  -> m v
  -> m v
fetch (MemCache mv) key construct = mask $ \release -> do
  -- Attempt to get a value corresponding to the key from cache. If it's there,
  -- bump its priority and return it. Otherwise either check whether the value
  -- is already being computed and return associated MVar or create a new one.
  eel <- modifyMVar mv $ \mc -> release $ do
    case Q.alter (lookupAndIncreasePriorityTo $ mcTick mc) key $ mcCache mc of
      (Just el, updatedCache) -> do
        let !mc' = mc {
                mcTick  = mcTick mc + 1
              , mcCache = updatedCache
              }
        return (mc', Right el)
      (Nothing, _) -> case key `HM.lookup` mcInProgress mc of
        Just mvEl -> return (mc, Left (mvEl, True))
        Nothing -> do
          mvEl <- newEmptyMVar
          let !mc' = mc {
                  mcInProgress = HM.insert key mvEl $ mcInProgress mc
                }
          return (mc', Left (mvEl, False))

  case eel of
    Right el -> return el
    Left (mvEl, mvAlreadyThere) -> do
      -- If we got existing MVar, wait for its result. Otherwise run constructing
      -- action and update cache accordingly.
      if mvAlreadyThere
        then release $ either throwIO return =<< readMVar mvEl
        else (`onException` removeInProgress) . release $ do
          -- Spawn new thread to avoid catching asynchronous exceptions thrown to a
          -- thread executing this function.
          evalue <- snd =<< LT.fork (evaluate . force =<< construct)
          modifyMVar_ mv $ \mc -> case evalue of
            Left _ -> do
              return $! mc {
                  mcInProgress = HM.delete key $ mcInProgress mc
                }
            Right value -> do
              return $! tidyCache $! mc {
                  mcCurrentSize = mcCurrentSize mc + mcSizeFun mc value
                , mcTick        = mcTick mc + 1
                , mcInProgress  = HM.delete key $ mcInProgress mc
                , mcCache       = Q.insert key (mcTick mc) value $ mcCache mc
                }
          putMVar mvEl evalue
          either throwIO return evalue
  where
    lookupAndIncreasePriorityTo prio = \case
      Nothing        -> (Nothing, Nothing)
      Just (_, mvEl) -> (Just mvEl, Just (prio, mvEl))

    removeInProgress :: m ()
    removeInProgress = modifyMVar_ mv $ \mc -> return $! mc {
        mcInProgress = HM.delete key $ mcInProgress mc
      }

    tidyCache :: MemCache_ k v -> MemCache_ k v
    tidyCache mc = go (mcCache mc) (mcCurrentSize mc)
      where
        go cache size
          | size <= mcSizeLimit mc = mc {
                mcCurrentSize = size
              , mcCache       = cache
              }
          | otherwise = case Q.minView cache of
              Nothing -> $unexpectedError "tidyCache: size limit exceeded even though cache is empty"
              Just (_, _, minEl, tidiedCache) -> go tidiedCache $ size - mcSizeFun mc minEl
