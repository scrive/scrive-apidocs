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
import User.Lang

-- | Monad transformer for adding templates functionality to underlying monad
newtype TemplatesT m a =
  TemplatesT { unTT :: ReaderT (Lang, KontrakcjaGlobalTemplates) m a }
    deriving (Applicative, CryptoRNG, Functor, Monad, MonadDB, MonadIO, MonadTrans)

runTemplatesT :: (Functor m, Monad m) => (Lang, KontrakcjaGlobalTemplates) -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTT action) ts

instance (Functor m, Monad m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (lang, ts) <- ask
    return $ localizedVersion lang ts
  getLocalTemplates lang = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion lang ts
