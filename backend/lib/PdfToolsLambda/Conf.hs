module PdfToolsLambda.Conf (
      PdfToolsLambdaConf
    , PdfToolsLambdaEnv
    , pdfToolsLambdaEnvFromConf
    , module PdfToolsLambda.Conf.Labels
    , PdfToolsLambdaConfMonad(..)
    , runPdfToolsLambdaConfT
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)

import PdfToolsLambda.Conf.Internal
import PdfToolsLambda.Conf.Labels

class Monad m => PdfToolsLambdaConfMonad m where
  getPdfToolsLambdaConf :: m PdfToolsLambdaEnv

-- | Generic, overlapping instance.
instance {-# OVERLAPPABLE #-} (
    PdfToolsLambdaConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => PdfToolsLambdaConfMonad (t m) where
    getPdfToolsLambdaConf = lift getPdfToolsLambdaConf

newtype PdfToolsLambdaConfT m a =
  PdfToolsLambdaConfT { unPdfToolsLambdaConfT :: ReaderT PdfToolsLambdaEnv m a }
  deriving ( Alternative, Applicative, Functor, Monad
           , MonadPlus, MonadIO, MonadTrans, MonadBase b
           , MonadThrow, MonadCatch, MonadMask )

instance MonadBaseControl b m => MonadBaseControl b (PdfToolsLambdaConfT m) where
  type StM (PdfToolsLambdaConfT m) a = ComposeSt PdfToolsLambdaConfT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl PdfToolsLambdaConfT where
  type StT PdfToolsLambdaConfT m = StT (ReaderT PdfToolsLambdaEnv) m
  liftWith = defaultLiftWith PdfToolsLambdaConfT unPdfToolsLambdaConfT
  restoreT = defaultRestoreT PdfToolsLambdaConfT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance Monad m => PdfToolsLambdaConfMonad (PdfToolsLambdaConfT m) where
  getPdfToolsLambdaConf = PdfToolsLambdaConfT ask

runPdfToolsLambdaConfT :: PdfToolsLambdaEnv -> PdfToolsLambdaConfT m a -> m a
runPdfToolsLambdaConfT ts m = runReaderT (unPdfToolsLambdaConfT m) ts
