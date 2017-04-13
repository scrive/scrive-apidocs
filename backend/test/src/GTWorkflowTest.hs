{- Tests in this file require access to GT server that can sign/extend/verify PDF's -}
module GTWorkflowTest where

import Control.Monad.Base
import Control.Monad.Trans
import Data.Functor
import Test.Framework
import qualified Data.ByteString as BS

import ActionQueue.Monad (ActionQueueT)
import ActionQueue.Scheduler (SchedulerData(..))
import AppConf (AppConf(dbConfig))
import DB
import Doc.Action (findAndExtendDigitalSignatures)
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentMonad (theDocument, withDocumentID)
import Doc.DocUtils
import Doc.Model
import Doc.SealStatus (SealStatus(..))
import KontraPrelude
import MinutesTime
import Templates
import TestingUtil
import TestKontra
import Util.Actor
import qualified Amazon as AWS
import qualified CronEnv
import qualified MemCache

gtWorkflowTests :: TestEnvSt -> Test
gtWorkflowTests env = testGroup "GTWorkflowTest" [
  testThat "Document with extensible digital signature can be extended" env testExtendDigitalSignatures
  ]


testExtendDigitalSignatures :: TestEnv ()
testExtendDigitalSignatures = do
  author <- addNewRandomUser
  let filename = inTestDir "pdfs/extensible.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- addNewFile filename filecontent
  file1 <- addNewFile filename filecontent
  file2 <- addNewFile filename filecontent
  did <- documentid <$> addRandomDocumentWithAuthorAndConditionAndFile author (isSignable && isClosed) file
  withDocumentID did $ do
    now <- currentTime
    let actor = systemActor (2 `monthsBefore` now)
    -- Append a file to tweak the modification time
    dbUpdate $ AppendSealedFile file1 Guardtime{ extended = False, private = False } actor
    dbUpdate $ AppendExtendedSealedFile file2 Guardtime{ extended = False, private = False } actor
    runScheduler findAndExtendDigitalSignatures
  withDocumentID did $ do
    documentsealstatus <$> theDocument >>= \case
      Just (Guardtime{ extended = True }) -> assertSuccess
      s -> assertFailure $ "Unexpected extension status: " ++ show s

runScheduler :: MonadBase IO m => ActionQueueT (AWS.AmazonMonadT m) SchedulerData a -> m a
runScheduler m = do
  let appConf = def { dbConfig = "" }
  templates <- liftBase readGlobalTemplates
  filecache <- MemCache.new BS.length 52428800
  CronEnv.runScheduler appConf filecache Nothing templates m
