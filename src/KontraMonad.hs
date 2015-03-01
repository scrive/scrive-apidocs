{-# LANGUAGE OverlappingInstances #-}
module KontraMonad (
      Kontrakcja
    , KontraMonad(..)
    , withAnonymousContext
    ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Control
import Happstack.Server
import Text.StringTemplates.Templates

import Context
import Crypto.RNG
import DB
import GuardTime (GuardTimeConfMonad)
import MailContext (MailContextMonad)
import MinutesTime.Class
import qualified Amazon as AWS
import qualified Log

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
type Kontrakcja m = (
    CryptoRNG m
  , FilterMonad Response m
  , GuardTimeConfMonad m
  , HasRqData m
  , KontraMonad m
  , MailContextMonad m
  , Log.MonadLog m
  , MonadDB m
  , MonadMask m
  , MonadBase IO m
  , MonadBaseControl IO m
  , MonadIO m
  , MonadTime m
  , ServerMonad m
  , TemplatesMonad m
  , AWS.AmazonMonad m
  )

class Monad m => KontraMonad m where
  getContext    :: m Context
  modifyContext :: (Context -> Context) -> m ()

-- | Generic, overlapping instance.
instance (
    KontraMonad m
  , Monad (t m)
  , MonadTrans t
  ) => KontraMonad (t m) where
    getContext = lift getContext
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
