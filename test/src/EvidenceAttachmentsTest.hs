module EvidenceAttachmentsTest (evidenceAttachmentsTest) where

import Control.Applicative((<$>))
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import qualified Doc.EvidenceAttachments as E
import TestKontra

evidenceAttachmentsTest :: TestEnvSt -> Test
evidenceAttachmentsTest _ = testGroup "EvidenceAttachments"
  [ testCase "Sealed file has evidence attachments (version 1)" $ sealedFileHasEvidenceAttachmentsVersion1
  , testCase "Sealed file has evidence attachments (version 2)" $ sealedFileHasEvidenceAttachmentsVersion2
  ]

sealedFileHasEvidenceAttachmentsVersion1 :: Assertion
sealedFileHasEvidenceAttachmentsVersion1 = do
  attachments <- E.extract <$> BS.readFile "test/pdfs/sealed.pdf"
  assertEqual "mismatched evidence attachment names"
    [ "evidenceOfIntent.html", "evidencelog.html", "readme.html", "signatureverification.html" ]
    (map E.name $ sort attachments)

sealedFileHasEvidenceAttachmentsVersion2 :: Assertion
sealedFileHasEvidenceAttachmentsVersion2 = do
  attachments <- E.extract <$> BS.readFile "test/pdfs/sealed-2.pdf"
  assertEqual "mismatched evidence attachment names"
    [ "DigitalSignatureDocumentation.html", "EvidenceDocumentation.html", "EvidenceLog.html", "EvidenceofIntent.html" ]
    (map E.name $ sort attachments)
