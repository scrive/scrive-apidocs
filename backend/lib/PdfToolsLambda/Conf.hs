module PdfToolsLambda.Conf
  ( I.PdfToolsLambdaConf
  , I.PdfToolsLambdaEnv
  , I.pdfToolsLambdaEnvFromConf
  , PdfToolsLambdaMonad(..)
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

import qualified PdfToolsLambda.Conf.Internal as I

class Monad m => PdfToolsLambdaMonad m where
  getPdfToolsLambdaEnv :: m I.PdfToolsLambdaEnv

-- | Generic, overlapping instance.
instance {-# OVERLAPPABLE #-} (
    PdfToolsLambdaMonad m
  , Monad (t m)
  , MonadTrans t
  ) => PdfToolsLambdaMonad (t m) where
  getPdfToolsLambdaEnv = lift getPdfToolsLambdaEnv

newtype PdfToolsLambdaT m a =
  PdfToolsLambdaT { unPdfToolsLambdaT :: ReaderT I.PdfToolsLambdaEnv m a }
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
  type StT PdfToolsLambdaT m = StT (ReaderT I.PdfToolsLambdaEnv) m
  liftWith = defaultLiftWith PdfToolsLambdaT unPdfToolsLambdaT
  restoreT = defaultRestoreT PdfToolsLambdaT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance Monad m => PdfToolsLambdaMonad (PdfToolsLambdaT m) where
  getPdfToolsLambdaEnv = PdfToolsLambdaT ask

runPdfToolsLambdaT :: I.PdfToolsLambdaEnv -> PdfToolsLambdaT m a -> m a
runPdfToolsLambdaT ts m = runReaderT (unPdfToolsLambdaT m) ts
