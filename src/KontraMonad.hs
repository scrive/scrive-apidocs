module KontraMonad (
      Kontrakcja
    , KontraMonad(..)
    , withAnonymousContext
    ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.State
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Control
import Happstack.Server

import Context
import Crypto.RNG
import DB.Core
import GuardTime (GuardTimeConfMonad)
import MailContext (MailContextMonad)
import Text.StringTemplates.Templates
import qualified Amazon as AWS

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class ( Applicative m
      , CryptoRNG m
      , FilterMonad Response m
      , GuardTimeConfMonad m
      , HasRqData m
      , KontraMonad m
      , MailContextMonad m
      , MonadDB m
      , MonadBase IO m
      , MonadBaseControl IO m
      , MonadIO m
      , ServerMonad m
      , TemplatesMonad m
      , AWS.AmazonMonad m
      ) => Kontrakcja m

class (Functor m, Monad m) => KontraMonad m where
  getContext    :: m Context
  modifyContext :: (Context -> Context) -> m ()

instance (Monad m, Functor m) => KontraMonad (StateT Context m) where
  getContext    = get
  modifyContext = modify

instance KontraMonad m => KontraMonad (ReaderT a m) where
  getContext = lift $ getContext
  modifyContext = lift . modifyContext

instance KontraMonad m => KontraMonad (StateT a m) where
  getContext = lift $ getContext
  modifyContext = lift . modifyContext

withAnonymousContext :: KontraMonad m => m a -> m a
withAnonymousContext action = do
  ctx <- getContext
  let ctx' = ctx { ctxmaybeuser = Nothing
                 , ctxmaybepaduser = Nothing
                 }
  modifyContext $ const ctx'
  res <- action
  modifyContext $ const ctx
  return res
