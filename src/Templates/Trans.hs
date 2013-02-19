module Templates.Trans (
    TemplatesT
  , runTemplatesT
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Base

import Crypto.RNG
import DB
import Templates.Templates
import Templates.TemplatesLoader
import User.Lang

-- | Monad transformer for adding templates functionality to underlying monad
newtype TemplatesT m a =
  TemplatesT { unTemplatesT :: ReaderT (Lang, KontrakcjaGlobalTemplates) m a }
    deriving (Applicative, CryptoRNG, Functor, Monad, MonadIO, MonadTrans)

deriving instance (MonadBase IO m) => MonadBase IO (TemplatesT m)
deriving instance (MonadDB m) => MonadDB (TemplatesT m)

runTemplatesT :: (Functor m, Monad m) => (Lang, KontrakcjaGlobalTemplates) -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTemplatesT action) ts

instance (Functor m, Monad m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (lang, ts) <- ask
    return $ localizedVersion lang ts
  getLocalTemplates lang = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion lang ts
