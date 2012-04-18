module Templates.Trans (
    TemplatesT
  , runTemplatesT
  ) where

import Control.Applicative
import Control.Monad.Reader

import Crypto.RNG
import DB
import Templates.Templates
import Templates.TemplatesLoader
import User.Locale

-- | Monad transformer for adding templates functionality to underlying monad
newtype TemplatesT m a =
  TemplatesT { unTT :: ReaderT (Locale, KontrakcjaGlobalTemplates) m a }
    deriving (Applicative, CryptoRNG, Functor, Monad, MonadDB, MonadIO, MonadTrans)

runTemplatesT :: (Functor m, Monad m) => (Locale, KontrakcjaGlobalTemplates) -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTT action) ts

instance (Functor m, Monad m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (locale, ts) <- ask
    return $ localizedVersion locale ts
  getLocalTemplates locale = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion locale ts
