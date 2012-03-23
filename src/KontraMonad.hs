module KontraMonad (
      KontraError
    , Kontrakcja
    , KontraMonad(..)
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error (MonadError)
import Happstack.Server

import Context
import DB.Classes
import KontraError (KontraError)
import Templates.Templates

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class ( Applicative m
      , DBMonad m
      , FilterMonad Response m
      , HasRqData m
      , KontraMonad m
      , MonadError KontraError m
      , MonadIO m
      , ServerMonad m
      , TemplatesMonad m
      , WebMonad Response m
      ) => Kontrakcja m

class (Functor m, Monad m) => KontraMonad m where
    getContext    :: m Context
    modifyContext :: (Context -> Context) -> m ()
