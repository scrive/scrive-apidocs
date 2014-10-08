module ActionQueue.Monad (
    ActionQueue
  , ActionQueueT
  , runQueue
  , actionQueue
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import Amazon
import ActionQueue.Core
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import MinutesTime
import qualified Log
import Text.JSON.Gen

type ActionQueue = ActionQueueT (AmazonMonadT (CryptoRNGT (DBT IO)))

type InnerAQ m qd = ReaderT qd m

newtype ActionQueueT m qd a = AQ { unAQ :: InnerAQ m qd a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadCatch, MonadDB, MonadIO, MonadMask, MonadReader qd, MonadThrow, AmazonMonad, MonadBase b)

instance (MonadBase IO m) => Log.MonadLog (ActionQueueT m qd) where
  mixlogjs title js = liftBase (Log.mixlogjsIO title js)

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
actionQueue :: (MonadDB m, MonadBase IO m, MonadCatch m, Log.MonadLog m, Show t)
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
    printSuccess a = Log.mixlog "Action evaluated successfully" $ do
        value "action" (show a)
        value "table" (BS.unpack $ unRawSQL (tblName (qaTable qa)))
    printError a e = Log.attention "Actional evaluation failed" $ do
        value "action" (show a)
        value "table" (BS.unpack $ unRawSQL (tblName (qaTable qa)))
        value "exception" (show e)
