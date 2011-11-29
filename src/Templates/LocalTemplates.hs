module Templates.LocalTemplates (
      LocalTemplates (..)
    , runWithTemplates
    ) where

import Templates.Templates

import User.Locale
import Control.Monad.Reader


-- | This is for emulating TemplatesMonad in IO environment
newtype LocalTemplates a = LocalTemplates { unLT :: ReaderT (Locale, KontrakcjaGlobalTemplates) IO a }
    deriving (Functor, Monad, MonadIO, MonadReader (Locale, KontrakcjaGlobalTemplates))

runWithTemplates :: Locale -> KontrakcjaGlobalTemplates -> LocalTemplates a -> IO a
runWithTemplates defaultlocale templates action = runReaderT (unLT action) (defaultlocale, templates)

instance TemplatesMonad LocalTemplates where
    getTemplates = do
      (defaultlocale, ts) <- ask
      return $ localizedVersion defaultlocale ts
    getLocalTemplates locale = do
      (_defaultlocale, ts) <- ask
      return $ localizedVersion locale ts