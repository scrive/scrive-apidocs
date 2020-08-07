{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flow.KontraHandler
  ( FlowKontra
  , KontraT
  , handle
  , runKontraT
  )
where

import Control.Monad.Base
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.State hiding (fail)
import Control.Monad.Time
import Control.Monad.Trans.Control
import Crypto.RNG
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Log.Class
import Log.Monad
import Optics.State
import Text.StringTemplates.Templates
import qualified Servant.Server.Internal.Handler as Servant
import qualified Text.StringTemplates.TemplatesLoader as TL

import AppConf
import AppControl
import EventStream.Kinesis
import FileStorage
import Flow.OrphanInstances ()
import GuardTime (GuardTimeConfMonad(..))
import Happstack.Server.ReqHandler
import Kontra
import KontraMonad ()
import MailContext
import PdfToolsLambda.Class
import PdfToolsLambda.Control
import Session.Types

type FlowKontra
  = KontraT
      (FileStorageT (KinesisT (CryptoRNGT (ReqHandlerT (DBT (LogT Servant.Handler))))))

newtype KontraT m a = KontraT { unKontraT :: StateT Context m a }
  deriving ( Applicative, CryptoRNG, Functor, HasRqData, Monad
           , MonadBase b, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow
           , ServerMonad, MonadFileStorage, MonadEventStream, MonadLog, MonadFail, MonadTrans)

deriving instance FilterMonad Response m => FilterMonad Response (KontraT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (KontraT m)

instance Monad m => TemplatesMonad (KontraT m) where
  getTemplates = view #templates <$> KontraT get
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- view #globalTemplates <$> KontraT get
    return $ TL.localizedVersion langStr globaltemplates

instance {-# OVERLAPPING #-} Monad m => KontraMonad (KontraT m) where
  getContext    = KontraT get
  modifyContext = KontraT . modify

instance {-# OVERLAPPABLE #-} (Monad m) => GuardTimeConfMonad (KontraT m)  where
  getGuardTimeConf = view #gtConf <$> KontraT get

instance Monad m => MailContextMonad (KontraT m) where
  getMailContext = contextToMailContext <$> KontraT get

instance MonadFail Servant.Handler where
  fail = liftIO . fail

instance (Monad m) => MonadTime (KontraT m) where
  currentTime = view #time <$> KontraT get

instance (CryptoRNG m, MonadBase IO m, MonadCatch m, MonadLog m) => PdfToolsLambdaMonad (KontraT m) where
  callPdfToolsSealing spec =
    KontraT $ callPdfToolsSealingPrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsPresealing spec =
    KontraT $ callPdfToolsPresealingPrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsAddImage spec =
    KontraT $ callPdfToolsAddImagePrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsPadesSign spec =
    KontraT $ callPdfToolsPadesSignPrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsCleaning spec =
    KontraT $ callPdfToolsCleaningPrim spec =<< use #pdfToolsLambdaEnv
  lambdaEnv =
    KontraT $ use #pdfToolsLambdaEnv

runKontraT :: (Monad m) => Context -> KontraT m a -> m a
runKontraT ctx f = evalStateT (unKontraT f) ctx

handle
  :: ( MonadBaseControl IO m
     , MonadIO m
     , MonadLog m
     , MonadDB m
     , MonadThrow m
     , MonadFail m
     , MonadMask m
     )
  => AppConf
  -> AppGlobals
  -> KontraT (FileStorageT (KinesisT (CryptoRNGT (ReqHandlerT m)))) Response
  -> Request
  -> m Response
handle appConf appGlobals handler request =
  handleRequest request
    . runCryptoRNGT (cryptorng appGlobals)
    . runKinesisT (kinesisStream appConf)
    . runFileStorageT
        (amazons3env appGlobals, mrediscache appGlobals, filecache appGlobals)
    $ handlerWithContext
  where
    handlerWithContext = do
      -- TODO create a normal session?
      session <- emptySession
      ctx     <- createContext appConf appGlobals session
      runKontraT ctx handler
