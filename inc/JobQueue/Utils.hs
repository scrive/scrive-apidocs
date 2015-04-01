module JobQueue.Utils (
    ThrownFrom(..)
  , stopExecution
  , forkP
  , gforkP
  ) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Typeable
import qualified Control.Exception.Lifted as E

import KontraPrelude
import qualified Control.Concurrent.Thread.Group.Lifted as TG
import qualified Control.Concurrent.Thread.Lifted as T

-- | Exception thrown to a thread to stop its execution.
-- All exceptions other than 'StopExecution' thrown to
-- threads spawned by 'forkP' and 'gforkP' are propagated
-- back to the parent thread.
data StopExecution = StopExecution
  deriving (Show, Typeable)
instance Exception StopExecution

-- | Exception thrown from a child thread.
data ThrownFrom = ThrownFrom String SomeException
  deriving (Show, Typeable)
instance Exception ThrownFrom

-- | Stop execution of a thread.
stopExecution :: MonadBase IO m => ThreadId -> m ()
stopExecution = flip throwTo StopExecution

----------------------------------------

-- | Modified version of 'fork' that propagates
-- thrown exceptions to the parent thread.
forkP :: MonadBaseControl IO m => String -> m () -> m ThreadId
forkP = forkImpl fork

-- | Modified version of 'TG.fork' that propagates
-- thrown exceptions to the parent thread.
gforkP :: MonadBaseControl IO m
       => TG.ThreadGroup
       -> String
       -> m ()
       -> m (ThreadId, m (T.Result ()))
gforkP = forkImpl . TG.fork

----------------------------------------

forkImpl :: MonadBaseControl IO m
         => (m () -> m a)
         -> String
         -> m ()
         -> m a
forkImpl ffork tname m = E.mask $ \release -> do
  parent <- myThreadId
  ffork $ release m `E.catches` [
      E.Handler $ \StopExecution -> return ()
    , E.Handler $ (throwTo parent . ThrownFrom tname)
    ]
