module EvidenceAttachmentsTest (evidenceAttachmentsTest) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import qualified Data.ByteString.Char8 as BS

import KontraPrelude
import TestKontra
import qualified Doc.EvidenceAttachments as E

evidenceAttachmentsTest :: TestEnvSt -> Test
evidenceAttachmentsTest _ = testGroup "EvidenceAttachments"
  [ testCase "Sealed file has evidence attachments (version 1)" $ sealedFileHasEvidenceAttachmentsVersion1
  , testCase "Sealed file has evidence attachments (version 2)" $ sealedFileHasEvidenceAttachmentsVersion2
  , testCase "Sealed file has evidence attachments (version 3)" $ sealedFileHasEvidenceAttachmentsVersion3
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

sealedFileHasEvidenceAttachmentsVersion3 :: Assertion
sealedFileHasEvidenceAttachmentsVersion3 = do
  attachments <- E.extract <$> BS.readFile "test/pdfs/sealed-3.pdf"
  assertEqual "mismatched evidence attachment names"
    [ "Appendix 1 Evidence Quality Framework.html"
    , "Appendix 2 Service Description.html"
    , "Appendix 3 Evidence Log.html"
    -- TODO Add Evidence of Time when it is included.
    , "Appendix 5 Evidence of Intent.html"
    , "Appendix 6 Digital Signature Documentation.html"
    , "Evidence Quality of Scrive Esigned Documents.html"
    ]
    (map E.name $ sort attachments)
