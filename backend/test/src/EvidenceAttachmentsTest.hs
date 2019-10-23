module EvidenceAttachmentsTest (evidenceAttachmentsTest) where

import Test.Framework (Test, testGroup)

import TestingUtil
import TestKontra
import qualified Doc.EvidenceAttachments as E

evidenceAttachmentsTest :: TestEnvSt -> Test
evidenceAttachmentsTest env = testGroup
  "EvidenceAttachments"
  [ testThat "Sealed file has evidence attachments (version 1)"
             env
             sealedFileHasEvidenceAttachmentsVersion1
  , testThat "Sealed file has evidence attachments (version 2)"
             env
             sealedFileHasEvidenceAttachmentsVersion2
  , testThat "Sealed file has evidence attachments (version 3)"
             env
             sealedFileHasEvidenceAttachmentsVersion3
  ]

sealedFileHasEvidenceAttachmentsVersion1 :: TestEnv ()
sealedFileHasEvidenceAttachmentsVersion1 = do
  attachments <- E.extractAttachmentsListFromFileContent
    =<< readTestFileAsBS "pdfs/sealed.pdf"
  assertEqual
    "mismatched evidence attachment names"
    [ "evidenceOfIntent.html"
    , "evidencelog.html"
    , "readme.html"
    , "signatureverification.html"
    ]
    (sort attachments)

sealedFileHasEvidenceAttachmentsVersion2 :: TestEnv ()
sealedFileHasEvidenceAttachmentsVersion2 = do
  attachments <- E.extractAttachmentsListFromFileContent
    =<< readTestFileAsBS "pdfs/sealed-2.pdf"
  assertEqual
    "mismatched evidence attachment names"
    [ "DigitalSignatureDocumentation.html"
    , "EvidenceDocumentation.html"
    , "EvidenceLog.html"
    , "EvidenceofIntent.html"
    ]
    (sort attachments)

sealedFileHasEvidenceAttachmentsVersion3 :: TestEnv ()
sealedFileHasEvidenceAttachmentsVersion3 = do
  attachments <- E.extractAttachmentsListFromFileContent
    =<< readTestFileAsBS "pdfs/sealed-3.pdf"
  assertEqual
    "mismatched evidence attachment names"
    [ "Appendix 1 Evidence Quality Framework.html"
    , "Appendix 2 Service Description.html"
    , "Appendix 3 Evidence Log.html"
    -- TODO Add Evidence of Time when it is included.
    , "Appendix 5 Evidence of Intent.html"
    , "Appendix 6 Digital Signature Documentation.html"
    , "Evidence Quality of Scrive Esigned Documents.html"
    ]
    (sort attachments)
