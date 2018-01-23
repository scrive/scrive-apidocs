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
import qualified Data.ByteString as BS
import qualified Database.Redis as R

import Amazon
import CronConf (CronConf, cronAmazonConfig, cronMailNoreplyAddress, cronSalesforceConf)
import DB
import File.FileID (FileID)
import Salesforce.Conf
import Templates (KontrakcjaGlobalTemplates)
import qualified MemCache

data CronEnv = CronEnv {
    ceSalesforceConf     :: Maybe SalesforceConf
  , ceTemplates          :: KontrakcjaGlobalTemplates
  , ceMailNoreplyAddress :: String
  }

runCronEnv :: MonadBase IO m
             => CronConf
             -> MemCache.MemCache FileID BS.ByteString
             -> Maybe R.Connection
             -> KontrakcjaGlobalTemplates
             -> CronEnvT (AmazonMonadT m) CronEnv a
             -> m a
runCronEnv cronConf localCache globalCache templates x = do
  let amazoncfg     = AmazonConfig (cronAmazonConfig cronConf)
                      localCache globalCache
      cronEnvData   = CronEnv (cronSalesforceConf cronConf) templates
                      (cronMailNoreplyAddress cronConf)
  runAmazonMonadT amazoncfg $ runReaderT (unCronEnvT x) cronEnvData

type CronEnvM = CronEnvT (AmazonMonadT (DBT (CryptoRNGT (LogT IO)))) CronEnv

-- hiding ReaderT prevents collision with ReaderT in TestEnvSt
newtype CronEnvT m sd a = CronEnvT { unCronEnvT :: ReaderT sd m a }
  deriving (Applicative, CryptoRNG, Functor, Monad, MonadCatch, MonadDB, MonadIO, MonadMask, MonadReader sd, MonadThrow, MonadTime, AmazonMonad, MonadBase b, MonadLog)

instance (MonadBaseControl IO m, MonadBase IO (CronEnvT m sd)) => MonadBaseControl IO (CronEnvT m sd) where
  type StM (CronEnvT m sd) a = StM (ReaderT sd m) a
  liftBaseWith f = CronEnvT $ liftBaseWith $ \run -> f $ run . unCronEnvT
  restoreM       = CronEnvT . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}
