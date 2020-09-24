module PadesTest (padesTests) where

import Control.Monad.Base
import Control.Monad.IO.Class
import Optics (gview)
import Test.Framework
import qualified Data.ByteString as BS
import qualified Data.Text as T

import DB
import Doc.DigitalSignature
import Doc.DigitalSignatureStatus as DigitalSignatureStatus
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.Model
import File.Storage
import File.Types
import GuardTime
import MinutesTime
import PdfToolsLambda.Monad
import Templates
import TestingUtil
import TestKontra
import User.Lang
import Util.Actor

padesTests :: TestEnvSt -> Test
padesTests env =
  testGroup "PAdESTest" [testThat "Document can be sealed with PAdES" env testSealing]

testSealing :: TestEnv ()
testSealing = do
  author <- instantiateRandomPadesUser
  let filename = inTestDir "pdfs/simple.pdf"
  filecontent <- liftIO $ BS.readFile filename
  file        <- saveNewFile (T.pack filename) filecontent
  did         <- documentid <$> addRandomDocumentWithFile
    (fileid file)
    (rdaDefault author) { rdaTypes = OneOf [Signable], rdaStatuses = OneOf [Closed] }
  runSQL_ $ "UPDATE documents SET sealing_method = 2 WHERE id =" <?> did

  pdfSealLambdaEnv <- gview #pdfToolsLambdaEnv
  templates        <- liftBase readGlobalTemplates
  withDocumentID did $ do
    now <- currentTime
    let actor = systemActor now
    dbUpdate $ AppendClosedFileWithDigitalSignatureEvidence
      (DigitallySignedFile file Missing)
      actor
    void
      . runTemplatesT (defaultLang, templates)
      . runGuardTimeConfT testGtConf
      . runPdfToolsLambdaT pdfSealLambdaEnv
      $ addDigitalSignature

  withDocumentID did $ do
    status <- documentdigitalsignaturestatus <$> theDocument
    assertEqual "Document digital signature status"
                (Just DigitalSignatureStatus.Pades)
                status
