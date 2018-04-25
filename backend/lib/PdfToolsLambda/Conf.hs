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
import Data.Default
import Data.Unjson

import Amazon.Config
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
  <*> fieldBy "amazon_s3"
      _pdfToolsLambdaS3Config
      "Amazon bucket configuration"
      (objectOf $ pure (,,)
       <*> field "bucket"
         (\(x,_,_) -> x)
         "In which bucket stored files exist"
       <*> field "access_key"
         (\(_,x,_) -> x)
         "Amazon access key"
       <*> field "secret_key"
         (\(_,_,x) -> x)
         "Amazon secret key")
  <*> fieldDef "skip_lambda" False
      _pdfToolsLambdaSkip
      "Fallback to old version of pdftools. Should be used ONLY as emergency fallback on production during transition and will be dropped in few weeks"

instance Unjson PdfToolsLambdaConf where
  unjsonDef = unjsonPdfToolsLambdaConf

instance Default PdfToolsLambdaConf where
  def = PdfToolsLambdaConf "DEFAULT_GATEWAY" "DEFAULT_API_KEY" defPdfToolsLambdaAmazonBucket True

defPdfToolsLambdaAmazonBucket :: AmazonConfig
defPdfToolsLambdaAmazonBucket = ("DEFAULT_S3_BUCKET_FOR_LAMBDA","DEFAULT_S3_ACCESS_KEY_FOR_LAMBDA","DEFAULT_S3_SECRET_KEY_FOR_LAMBDA")

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
