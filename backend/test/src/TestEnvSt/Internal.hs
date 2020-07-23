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
import TestFileStorage

newtype RunLogger = RunLogger { unRunLogger :: forall m r . LogT m r -> m r }

data TestEnvSt = TestEnvSt
  { connSource         :: !BasicConnectionSource
  , staticConnSource   :: !BasicConnectionSource
  , transSettings      :: !TransactionSettings
  , rngState           :: !CryptoRNGState
  , runLogger          :: !RunLogger
  , activeTests        :: !(TVar (Bool, Int))
  , globalTemplates    :: !KontrakcjaGlobalTemplates
  , rejectedDocuments  :: !(MVar Int)
  , outputDirectory    :: !(Maybe String)
    -- ^ Put the test artifact output in this directory, if given.
  , stagingTests       :: !Bool
  , pdfToolsLambdaEnv  :: PdfToolsLambdaEnv
  , amazonS3Env        :: Maybe AmazonS3Env
  , fileMemCache       :: FileMemCache
  , redisConn          :: Maybe R.Connection
  , memoryStorage      :: TVar FakeFS
  , cronDBConfig       :: !Text
  , cronMonthlyInvoice :: Maybe MonthlyInvoiceConf
  , testDurations      :: MVar [(NominalDiffTime, String)]
  , flowPort :: Int
  }

data TestEnvStRW = TestEnvStRW
  { timeDelay   :: !NominalDiffTime
    -- ^ Modifies currentTime, when taken from IO.
  , currentTime :: !(Maybe UTCTime)
    -- ^ When 'Nothing', currentTime is taken from IO.
  , requestUri  :: !String
  }

makeFieldLabelsWith noPrefixFieldLabels ''TestEnvSt
makeFieldLabelsWith noPrefixFieldLabels ''TestEnvStRW
