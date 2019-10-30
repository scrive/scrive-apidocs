{-# LANGUAGE TemplateHaskell #-}
module TestEnvSt.Internal where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Crypto.RNG
import Data.Time
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Log.Monad
import Optics.TH
import qualified Database.Redis as R

import DB.PostgreSQL
import FileStorage
import FileStorage.Amazon.S3Env
import MonthlyInvoice.Config (MonthlyInvoiceConf(..))
import PdfToolsLambda.Conf
import Templates

newtype RunLogger = RunLogger { unRunLogger :: (forall m r . LogT m r -> m r) }

data TestEnvSt = TestEnvSt {
    teConnSource         :: !BasicConnectionSource
  , teStaticConnSource   :: !BasicConnectionSource
  , teTransSettings      :: !TransactionSettings
  , teRNGState           :: !CryptoRNGState
  , teRunLogger          :: !RunLogger
  , teActiveTests        :: !(TVar (Bool, Int))
  , teGlobalTemplates    :: !KontrakcjaGlobalTemplates
  , teRejectedDocuments  :: !(MVar Int)
  , teOutputDirectory    :: !(Maybe String)
    -- ^ Put the test artifact output in this directory, if given.
  , teStagingTests       :: !Bool
  , tePdfToolsLambdaEnv  :: PdfToolsLambdaEnv
  , teAmazonS3Env        :: Maybe AmazonS3Env
  , teFileMemCache       :: FileMemCache
  , teRedisConn          :: Maybe R.Connection
  , teCronDBConfig       :: !Text
  , teCronMonthlyInvoice :: Maybe MonthlyInvoiceConf
  , teTestDurations      :: MVar [(NominalDiffTime, String)]
  }

data TestEnvStRW = TestEnvStRW {
    terwTimeDelay   :: !NominalDiffTime
    -- ^ Modifies currentTime, when taken from IO.
  , terwCurrentTime :: !(Maybe UTCTime)
    -- ^ When 'Nothing', currentTime is taken from IO.
  , terwRequestURI  :: !String
  }

makeFieldLabelsWith noPrefixFieldLabels ''TestEnvSt
makeFieldLabelsWith noPrefixFieldLabels ''TestEnvStRW
