module ActionQueue.Monad where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as E

import ActionQueue.Core
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import DB.PostgreSQL
import MinutesTime
import qualified Log

type InnerAQ qd = ReaderT qd (CryptoRNGT (DBT IO))

newtype ActionQueue qd a = AQ { unAQ :: InnerAQ qd a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadBase IO, MonadDB, MonadIO, MonadReader qd)

instance MonadBaseControl IO (ActionQueue qd) where
  newtype StM (ActionQueue qd) a = StAQ { unStAQ :: StM (InnerAQ qd) a }
  liftBaseWith = newtypeLiftBaseWith AQ unAQ StAQ
  restoreM = newtypeRestoreM AQ unStAQ
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

-- Note: Do not define TemplatesMonad instance for ActionQueue, use
-- LocalTemplates instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

runQueue :: CryptoRNGState -> String -> qd -> ActionQueue qd () -> IO ()
runQueue rng dbconf qd queue =
  withPostgreSQL dbconf . runCryptoRNGT rng $ runReaderT (unAQ queue) qd

-- | Gets 'expired' actions and evaluates them
actionQueue :: Show t => Action idx t con (ActionQueue qd) -> ActionQueue qd ()
actionQueue qa = getMinutesTime
  >>= dbQuery . GetExpiredActions qa
  >>= mapM_ (\a -> do
    res <- E.try $ qaEvaluateExpired qa a
    case res of
      Left (e::E.SomeException) -> do
        printError a e
        dbRollback
      Right () -> do
        printSuccess a
        dbCommit
    )
  where
    printSuccess a = Log.debug $ "Action " ++ show a ++ " evaluated successfully"
    printError a e = Log.error $ "Oops, qaEvaluateExpired with " ++ show a ++ " failed with error: " ++ show e
