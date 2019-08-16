module CronEnv (
    runCronEnv
  , CronEnv(..)
  , CronEnvM

  -- exported for testing only
  , CronEnvT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Log

import CronConf (CronConf, cronMailNoreplyAddress, cronSalesforceConf)
import DB
import FileStorage
import Salesforce.Conf
import Templates (KontrakcjaGlobalTemplates)

data CronEnv = CronEnv {
    ceSalesforceConf     :: Maybe SalesforceConf
  , ceTemplates          :: KontrakcjaGlobalTemplates
  , ceMailNoreplyAddress :: Text
  }

runCronEnv :: MonadBase IO m
           => CronConf
           -> KontrakcjaGlobalTemplates
           -> CronEnvT m CronEnv a
           -> m a
runCronEnv cronConf templates x = do
  let cronEnvData = CronEnv (cronSalesforceConf cronConf) templates
                            (cronMailNoreplyAddress cronConf)
  runReaderT (unCronEnvT x) cronEnvData

type CronEnvM = CronEnvT (DBT (FileStorageT (CryptoRNGT (LogT IO)))) CronEnv

-- hiding ReaderT prevents collision with ReaderT in TestEnvSt
newtype CronEnvT m sd a = CronEnvT { unCronEnvT :: ReaderT sd m a }
  deriving ( Applicative, CryptoRNG, Functor, Monad, MonadCatch, MonadDB, MonadIO
           , MonadMask, MonadReader sd, MonadThrow, MonadFileStorage
           , MonadBase b)

deriving newtype instance
            (Monad m, MonadTime m) => MonadTime (CronEnvT m sd)
deriving newtype instance
            (Monad m, MonadTime m, MonadLog m)
                                   => MonadLog  (CronEnvT m sd)

instance (MonadBaseControl IO m, MonadBase IO (CronEnvT m sd)) => MonadBaseControl IO (CronEnvT m sd) where
  type StM (CronEnvT m sd) a = StM (ReaderT sd m) a
  liftBaseWith f = CronEnvT $ liftBaseWith $ \run -> f $ run . unCronEnvT
  restoreM       = CronEnvT . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}
