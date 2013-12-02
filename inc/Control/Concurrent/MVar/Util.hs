module Control.Concurrent.MVar.Util where

import Control.Concurrent.MVar (MVar, tryTakeMVar, putMVar)
import Control.Exception (mask_)

-- | A non-blocking version of 'readMVar'.
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar v = mask_ $ do
  ma <- tryTakeMVar v
  case ma of
    Nothing -> return Nothing
    Just a  -> do putMVar v a
                  return (Just a)
