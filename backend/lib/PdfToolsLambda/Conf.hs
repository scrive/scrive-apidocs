{-# OPTIONS_GHC -fno-warn-orphans #-}

module PdfToolsLambda.Conf (
      PdfToolsLambdaConf
    , module PdfToolsLambda.Conf.Labels
    , PdfToolsLambdaConfMonad(..)
    , runPdfToolsLambdaConfT
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import Data.Unjson

import PdfToolsLambda.Conf.Internal
import PdfToolsLambda.Conf.Labels

unjsonPdfToolsLambdaConf :: UnjsonDef PdfToolsLambdaConf
unjsonPdfToolsLambdaConf = objectOf $ pure PdfToolsLambdaConf
  <*> field "gateway_url"
      _pdfToolsLambdaGatewayUrl
      "Pdf Tools Lambda Gateway Url"
  <*> field "api_key"
      _pdfToolsLambdaApiKey
      "Pdf Tools Lambda Api Key"
  <*> field "amazon_s3"
      _pdfToolsLambdaS3Config
      "Amazon bucket configuration"
  <*> fieldOpt "user_groups_with_extended_flattening"
      _pdfToolsUserGroupsWithExtendedFlattening
      "Pdfs of documents created by this user groups should be treat differently by pdftools due to backward compatiblility (CORE-783)"

instance Unjson PdfToolsLambdaConf where
  unjsonDef = unjsonPdfToolsLambdaConf

class Monad m => PdfToolsLambdaConfMonad m where
  getPdfToolsLambdaConf :: m PdfToolsLambdaConf

-- | Generic, overlapping instance.
instance (
    PdfToolsLambdaConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => PdfToolsLambdaConfMonad (t m) where
    getPdfToolsLambdaConf = lift getPdfToolsLambdaConf

newtype PdfToolsLambdaConfT m a = PdfToolsLambdaConfT { unPdfToolsLambdaConfT :: ReaderT PdfToolsLambdaConf m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

instance MonadBaseControl b m => MonadBaseControl b (PdfToolsLambdaConfT m) where
  type StM (PdfToolsLambdaConfT m) a = ComposeSt PdfToolsLambdaConfT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl PdfToolsLambdaConfT where
  type StT PdfToolsLambdaConfT m = StT (ReaderT PdfToolsLambdaConf) m
  liftWith = defaultLiftWith PdfToolsLambdaConfT unPdfToolsLambdaConfT
  restoreT = defaultRestoreT PdfToolsLambdaConfT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance {-# OVERLAPPING #-} Monad m => PdfToolsLambdaConfMonad (PdfToolsLambdaConfT m) where
  getPdfToolsLambdaConf = PdfToolsLambdaConfT ask

runPdfToolsLambdaConfT :: PdfToolsLambdaConf -> PdfToolsLambdaConfT m a -> m a
runPdfToolsLambdaConfT ts m = runReaderT (unPdfToolsLambdaConfT m) ts
