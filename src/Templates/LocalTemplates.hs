module Templates.LocalTemplates (
      LocalTemplates
    , runLocalTemplates
    ) where

import Templates.Templates
import Control.Monad.Reader

-- | This is for emulating TemplatesMonad in IO environment
newtype LocalTemplates a = LocalTemplates { unLT :: ReaderT KontrakcjaTemplates IO a }
    deriving (Functor, Monad, MonadIO)

runLocalTemplates :: KontrakcjaTemplates -> LocalTemplates a -> IO a
runLocalTemplates templates action = runReaderT (unLT action) templates

instance TemplatesMonad LocalTemplates where
    getTemplates = LocalTemplates ask
    getLocalTemplates = const $ LocalTemplates ask