module TestEnvSt.Internal where

import DB.PostgreSQL
import FileStorage
import FileStorage.Amazon.Config
import PdfToolsLambda.Conf
import Templates

import Control.Concurrent.STM.TVar
import Crypto.RNG
import Data.Time
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Log.Monad
import qualified Database.Redis as R

newtype RunLogger = RunLogger { unRunLogger :: (forall m r . LogT m r -> m r) }

data TestEnvSt = TestEnvSt {
    _teConnSource         :: !BasicConnectionSource
  , _teStaticConnSource   :: !BasicConnectionSource
  , _teTransSettings      :: !TransactionSettings
  , _teRNGState           :: !CryptoRNGState
  , _teRunLogger          :: !RunLogger
  , _teActiveTests        :: !(TVar (Bool, Int))
  , _teGlobalTemplates    :: !KontrakcjaGlobalTemplates
  , _teRejectedDocuments  :: !(TVar Int)
  , _teOutputDirectory    :: !(Maybe String)
    -- ^ Put the test artifact output in this directory, if given.
  , _teStagingTests       :: !Bool
  , _tePdfToolsLambdaConf :: PdfToolsLambdaConf
  , _teAmazonConfig       :: Maybe AmazonConfig
  , _teFileMemCache       :: FileMemCache
  , _teRedisConn          :: Maybe R.Connection
  }

data TestEnvStRW = TestEnvStRW {
    _terwTimeDelay   :: !NominalDiffTime
    -- ^ Modifies currentTime, when taken from IO.
  , _terwCurrentTime :: !(Maybe UTCTime)
    -- ^ When 'Nothing', currentTime is taken from IO.
  , _terwRequestURI  :: !String
  }
