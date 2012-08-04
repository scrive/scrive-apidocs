module KontraMonad (
      Kontrakcja
    , KontraMonad(..)
    ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Trans.Control
import Happstack.Server

import Acid.Monad
import AppState
import Context
import Crypto.RNG
import DB.Core
import Templates.Templates

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class ( Applicative m
      , CryptoRNG m
      , FilterMonad Response m
      , AcidStore AppState m
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
