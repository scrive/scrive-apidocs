module KontraMonad (
      Kontrakcja
    , KontraMonad(..)
    ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Trans.Control
import Happstack.Server

import Context
import Crypto.RNG
import DB.Core
import Text.StringTemplates.Templates

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class ( Applicative m
      , CryptoRNG m
      , FilterMonad Response m
      , HasRqData m
      , KontraMonad m
      , MonadDB m
      , MonadBase IO m
      , MonadBaseControl IO m
      , MonadIO m
      , ServerMonad m
      , TemplatesMonad m
      ) => Kontrakcja m

class (Functor m, Monad m) => KontraMonad m where
  getContext    :: m Context
  modifyContext :: (Context -> Context) -> m ()

instance (Monad m, Functor m) => KontraMonad (StateT Context m) where
  getContext    = get
  modifyContext = modify
