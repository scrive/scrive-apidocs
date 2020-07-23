{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flow.TestKontraHandler
  ( FlowTestKontra
  , handle
  )
where

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Log.Class
import Log.Monad
import qualified Servant.Server.Internal.Handler as Servant

import BrandedDomain.Model
import DB.Query
import EventStream.Kinesis
import FileStorage
import Flow.KontraHandler (KontraT, runKontraT)
import Flow.OrphanInstances ()
import Happstack.Server.ReqHandler
import Kontra
import KontraMonad ()
import TestEnvSt
import TestFileStorage
import User.Lang
import qualified TestKontra

-- TODO do we want to use a different KontraT in tests?

type FlowTestKontra
  = KontraT
      ( TestFileStorageT
          (KinesisT (CryptoRNGT (ReqHandlerT (DBT (LogT Servant.Handler)))))
      )

handle
  :: ( MonadBaseControl IO m
     , MonadIO m
     , MonadLog m
     , MonadDB m
     , MonadThrow m
     , MonadFail m
     , MonadMask m
     )
  => TestEnvSt
  -> KontraT (TestFileStorageT (KinesisT (CryptoRNGT (ReqHandlerT m)))) Response
  -> Request
  -> m Response
handle st m req = do
  ctx <- mkContext st
  handleRequest req
    . runCryptoRNGT (st ^. #rngState)
    . runKinesisT Nothing
    . evalTestFileStorageT
        (st ^. #memoryStorage)
        ((, st ^. #redisConn, st ^. #fileMemCache) <$> st ^. #amazonS3Env)
    . runKontraT ctx
    $ m

mkContext
  :: (MonadBaseControl IO m, MonadDB m, MonadLog m, MonadThrow m, MonadTime m)
  => TestEnvSt
  -> m Context
mkContext st = do
  let pdfToolsLambdaEnv = st ^. #pdfToolsLambdaEnv
  let globalTemplates   = st ^. #globalTemplates
  now       <- currentTime
  bd        <- dbQuery GetMainBrandedDomain
  fileCache <- newFileMemCache 52428800
  pure $ TestKontra.mkContext' pdfToolsLambdaEnv
                               globalTemplates
                               now
                               bd
                               fileCache
                               defaultLang

