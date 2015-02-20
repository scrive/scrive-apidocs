module JobQueue.Utils (
    StopExecution(..)
  , ThrownFrom(..)
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

import qualified Control.Concurrent.Thread.Group.Lifted as TG
import qualified Control.Concurrent.Thread.Lifted as T

data StopExecution = StopExecution
  deriving (Show, Typeable)
instance Exception StopExecution

data ThrownFrom = ThrownFrom String SomeException
  deriving (Show, Typeable)
instance Exception ThrownFrom

stopExecution :: MonadBase IO m => ThreadId -> m ()
stopExecution = flip throwTo StopExecution

----------------------------------------

forkP :: MonadBaseControl IO m => String -> m () -> m ThreadId
forkP = forkImpl fork

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
