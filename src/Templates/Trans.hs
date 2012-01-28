module Templates.Trans (
      TemplatesT
    , runTemplatesT
    ) where

import Control.Monad.Reader

import Crypto.RNG(CryptoRNG)
import DB.Classes
import Templates.Templates
import User.Locale

-- | Monad transformer for adding templates functionality to underlying monad
newtype TemplatesT m a =
  TemplatesT { unTT :: ReaderT (Locale, KontrakcjaGlobalTemplates) m a }
    deriving (Functor, Monad, MonadIO, MonadTrans, CryptoRNG)

runTemplatesT :: (Functor m, MonadIO m) => (Locale, KontrakcjaGlobalTemplates) -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTT action) ts

instance (Functor m, MonadIO m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (locale, ts) <- ask
    return $ localizedVersion locale ts
  getLocalTemplates locale = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion locale ts

instance (CryptoRNG (TemplatesT m), DBMonad m) => DBMonad (TemplatesT m) where
  getConnection = lift getConnection
  handleDBError = lift . handleDBError

-- add more instances if neccessary...
