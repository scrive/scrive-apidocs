module Templates.Trans (
      TemplatesT
    , runTemplatesT
    ) where

import Control.Monad.Reader

import Crypto.RNG(CryptoRNG)
import DB.Classes
import Templates.Templates
import Templates.TemplatesLoader
import User.Locale

-- | Monad transformer for adding templates functionality to underlying monad
newtype TemplatesT m a =
  TemplatesT { unTT :: ReaderT (Locale, KontrakcjaGlobalTemplates) m a }
    deriving (Functor, Monad, MonadDB, MonadIO, MonadTrans, CryptoRNG)

runTemplatesT :: (Functor m, Monad m) => (Locale, KontrakcjaGlobalTemplates) -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTT action) ts

instance (Functor m, Monad m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (locale, ts) <- ask
    return $ localizedVersion locale ts
  getLocalTemplates locale = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion locale ts

-- add more instances if neccessary...
