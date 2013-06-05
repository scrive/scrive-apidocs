module ActionQueue.Monad (
    ActionQueue
  , ActionQueueT
  , runQueue
  , actionQueue
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as E

import ActionQueue.Core
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import qualified Log
import Amazon

type ActionQueue = ActionQueueT (AmazonMonadT (CryptoRNGT (DBT IO)))

type InnerAQ m qd = ReaderT qd m

newtype ActionQueueT m qd a = AQ { unAQ :: InnerAQ m qd a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadDB, MonadIO, MonadReader qd, Log.MonadLog, AmazonMonad)

deriving instance MonadBase IO m => MonadBase IO (ActionQueueT m qd)

instance (MonadBaseControl IO m, MonadBase IO (ActionQueueT m qd)) => MonadBaseControl IO (ActionQueueT m qd) where
  newtype StM (ActionQueueT m qd) a = StAQ { unStAQ :: StM (InnerAQ m qd) a }
  liftBaseWith = newtypeLiftBaseWith AQ unAQ StAQ
  restoreM = newtypeRestoreM AQ unStAQ
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runQueue :: qd -> ActionQueueT m qd a -> m a
runQueue qd queue =
  runReaderT (unAQ queue) qd

-- | Gets 'expired' actions and evaluates them
actionQueue :: (MonadDB m, MonadBaseControl IO m, Show t) => Action idx t con (ActionQueueT m qd) -> ActionQueueT m qd ()
actionQueue qa = getMinutesTime
  >>= dbQuery . GetExpiredActions qa
  >>= mapM_ (\a -> do
    res <- E.try $ qaEvaluateExpired qa a
    case res of
      Left (e::E.SomeException) -> do
        printError a e
        kRollback
      Right () -> do
        printSuccess a
        kCommit
    )
  where
    printSuccess a = Log.debug $ "Action " ++ show a ++ " evaluated successfully"
    printError a e = Log.error $ "Oops, qaEvaluateExpired with " ++ show a ++ " failed with error: " ++ show e
