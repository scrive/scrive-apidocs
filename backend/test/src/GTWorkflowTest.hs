{- Tests in this file require access to GT server that can sign/extend/verify PDF's -}
module GTWorkflowTest (gtWorkflowTests) where

import Control.Monad.Base
import Control.Monad.Reader
import Happstack.Server (Method(..))
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Context
import DB
import Doc.API.V2.Calls.CallsTestUtils
import Doc.API.V2.Calls.SignatoryCalls (docApiV2SigSign)
import Doc.API.V2.Mock.TestUtils
import Doc.DigitalSignature (extendDigitalSignature)
import Doc.DocStateData
import Doc.DocumentMonad (theDocument, withDocumentID)
import Doc.Model
import Doc.SealStatus (SealStatus(..))
import File.Storage
import GuardTime
import MinutesTime
import Templates
import TestCron
import TestingUtil
import TestKontra
import User.Model
import Util.Actor

gtWorkflowTests :: TestEnvSt -> Test
gtWorkflowTests env = testGroup "GTWorkflowTest" [
    testThat "Document with extensible digital signature can be extended" env testExtendDigitalSignatures
  , testThat "When document is purged before extending, the extending job is not rescheduled." env testExtendingIsNotRescheduledForPurgedDocs
  ]

testExtendDigitalSignatures :: TestEnv ()
testExtendDigitalSignatures = do
  author <- addNewRandomUser
  let filename = inTestDir "pdfs/extensible.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file <- saveNewFile (T.pack filename) filecontent
  file1 <- saveNewFile (T.pack filename) filecontent
  file2 <- saveNewFile (T.pack filename) filecontent
  did <- documentid <$> addRandomDocumentWithFile file (rdaDefault author)
    { rdaTypes = Or [Signable]
    , rdaStatuses = Or [Closed]
    }

  withDocumentID did $ do
    now <- currentTime
    let actor = systemActor (2 `monthsBefore` now)
    -- Append a file to tweak the modification time
    dbUpdate $ AppendSealedFile file1 Guardtime{ extended = False, private = False } actor
    dbUpdate $ AppendExtendedSealedFile file2 Guardtime{ extended = False, private = False } actor

    -- Run extending
    templates <- liftBase readGlobalTemplates
    void
      . runTemplatesT (defaultLang, templates)
      . runGuardTimeConfT testGTConf
      $ extendDigitalSignature

  withDocumentID did $ do
    documentsealstatus <$> theDocument >>= \case
      Just (Guardtime{ extended = True }) -> assertSuccess
      s -> assertFailure $ "Unexpected extension status: " ++ show s

testExtendingIsNotRescheduledForPurgedDocs :: TestEnv ()
testExtendingIsNotRescheduledForPurgedDocs = do
  setTestTime unixEpoch
  user <- addNewRandomUser
  ctx <- (set ctxmaybeuser (Just user)) <$> mkContext defaultLang
  -- Create a document
  mockDoc <- testDocApiV2StartNew ctx
  let did = getMockDocId mockDoc
  let slid = getMockDocSigLinkId 1 mockDoc

  -- Sign document
  void $ mockDocTestRequestHelper ctx
    POST
      [ ("fields", inText "[]")
      , ("accepted_author_attachments", inText "[]")
      ]
    (docApiV2SigSign did slid) 200

  -- There is a document sealing job scheduled
  runSQL01_ $ "SELECT EXISTS (SELECT id FROM document_sealing_jobs WHERE id =" <?> did <> ")"
  fetchOne runIdentity >>= assertEqual "Document is scheduled for sealing" True

  -- Commit is not completely necessary here, because there is a commit in the middle
  -- of signing. See also the other runTestCronUntilIdle.
  commit

  -- Run cron job
  runTestCronUntilIdle ctx

  -- There is a document extending job scheduled
  runSQL01_ $ "SELECT EXISTS (SELECT id FROM document_extending_jobs WHERE id =" <?> did <> ")"
  fetchOne runIdentity >>= assertEqual "Document is scheduled for extending" True

  -- Purge document
  withDocumentID did $ void $ randomUpdate $ \t -> ArchiveDocument (userid user) (systemActor t)
  modifyTestTime (31 `daysAfter`)
  purgedcount <- dbUpdate $ PurgeDocuments 0
  assertEqual "Purged single document" 1 purgedcount

  -- Move time forward to the scheduled extending job (40 days to be sure)
  modifyTestTime (40 `daysAfter`)
  -- commit is necessary here, because cron runs new processes, which means a different DB transaction
  commit

  -- Run cron job
  runTestCronUntilIdle ctx

  -- There is no extending job scheduled for this document
  runSQL01_ $ "SELECT EXISTS (SELECT id FROM document_extending_jobs WHERE id =" <?> did <> ")"
  fetchOne runIdentity >>= assertEqual "Document is not scheduled for extending" False
