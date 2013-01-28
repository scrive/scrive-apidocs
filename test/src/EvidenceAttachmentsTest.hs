module EvidenceAttachmentsTest (evidenceAttachmentsTest) where

import Control.Applicative((<$>))
import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import qualified Doc.EvidenceAttachments as E

evidenceAttachmentsTest :: Test
evidenceAttachmentsTest = testGroup "EvidenceAttachments"
  [ testCase "Sealed file has evidence attachments" $ sealedFileHasEvidenceAttachments
  ]

sealedFileHasEvidenceAttachments :: Assertion
sealedFileHasEvidenceAttachments = do
  attachments <- E.extract <$> BS.readFile "test/pdfs/sealed.pdf"
  assertEqual "mismatched evidence attachment names"
    [ "evidenceOfIntent.html", "evidencelog.html", "readme.html", "signatureverification.html" ]
    (map E.name $ sort attachments)