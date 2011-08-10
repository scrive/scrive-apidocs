module KontraMonad (
      Kontrakcja
    , KontraMonad(..)
    ) where

import Control.Applicative
import Control.Monad.State
import Happstack.Server

import Context
import Templates.Templates

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class (Applicative m, FilterMonad Response m, HasRqData m, KontraMonad m, MonadIO m, MonadPlus m, ServerMonad m, TemplatesMonad m, WebMonad Response m) => Kontrakcja m

class (Functor m, Monad m) => KontraMonad m where
    getContext    :: m Context
    modifyContext :: (Context -> Context) -> m ()
