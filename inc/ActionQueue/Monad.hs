module ActionQueue.Monad (
    ActionQueue
  , ActionQueueT
  , runQueue
  , actionQueue
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Log
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS

import ActionQueue.Core
import Amazon
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import KontraPrelude

type ActionQueue = ActionQueueT (AmazonMonadT (CryptoRNGT (DBT (LogT IO))))

type InnerAQ m qd = ReaderT qd m

newtype ActionQueueT m qd a = AQ { unAQ :: InnerAQ m qd a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadCatch, MonadDB, MonadIO, MonadMask, MonadReader qd, MonadThrow, MonadTime, AmazonMonad, MonadBase b, MonadLog)

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
actionQueue :: (MonadDB m, MonadBase IO m, MonadCatch m, MonadLog m, Show t)
            => Action idx t con (ActionQueueT m qd) -> ActionQueueT m qd ()
actionQueue qa = currentTime
  >>= dbQuery . GetExpiredActions qa
  >>= mapM_ (\a -> do
    res <- try $ qaEvaluateExpired qa a
    case res of
      Left (e::E.SomeException) -> do
        printError a e
        rollback
      Right () -> do
        printSuccess a
        commit
    )
  where
    tableName = BS.unpack . unRawSQL . tblName $ qaTable qa

    printSuccess a = logInfo "Action evaluated successfully" $ object [
        "action" .= show a
      , "table" .= tableName
      ]
    printError a e = logAttention "Actional evaluation failed" $ object [
        "action" .= show a
      , "table" .= tableName
      , "exception" .= show e
      ]
