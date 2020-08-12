module PdfToolsLambda.Monad
  ( PdfToolsLambdaT(..)
  , runPdfToolsLambdaT
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Control
  ( ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith
  , defaultLiftWith, defaultRestoreM, defaultRestoreT
  )
import Crypto.RNG
import Log

import PdfToolsLambda.Class
import PdfToolsLambda.Conf
import PdfToolsLambda.Control

newtype PdfToolsLambdaT m a =
  PdfToolsLambdaT { unPdfToolsLambdaT :: ReaderT PdfToolsLambdaEnv m a }
  deriving ( Alternative, Applicative, Functor, Monad
           , MonadPlus, MonadIO, MonadTrans, MonadBase b
           , MonadThrow, MonadCatch, MonadMask )

instance MonadBaseControl b m => MonadBaseControl b (PdfToolsLambdaT m) where
  type StM (PdfToolsLambdaT m) a = ComposeSt PdfToolsLambdaT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl PdfToolsLambdaT where
  type StT PdfToolsLambdaT m = StT (ReaderT PdfToolsLambdaEnv) m
  liftWith = defaultLiftWith PdfToolsLambdaT unPdfToolsLambdaT
  restoreT = defaultRestoreT PdfToolsLambdaT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

runPdfToolsLambdaT :: PdfToolsLambdaEnv -> PdfToolsLambdaT m a -> m a
runPdfToolsLambdaT ts m = runReaderT (unPdfToolsLambdaT m) ts

instance
  (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m
  ) => PdfToolsLambdaMonad (PdfToolsLambdaT m) where
  callPdfToolsSealing spec = PdfToolsLambdaT $ callPdfToolsSealingPrim spec =<< ask
  callPdfToolsPresealing spec = PdfToolsLambdaT $ callPdfToolsPresealingPrim spec =<< ask
  callPdfToolsAddImage spec = PdfToolsLambdaT $ callPdfToolsAddImagePrim spec =<< ask
  callPdfToolsPadesSign spec = PdfToolsLambdaT $ callPdfToolsPadesSignPrim spec =<< ask
  callPdfToolsCleaning spec = PdfToolsLambdaT $ callPdfToolsCleaningPrim spec =<< ask
  lambdaEnv = PdfToolsLambdaT ask
