module DocControlTest(
    docControlTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import DocState
import User
import DocControl
import SamplerHelper
import SampleData
import Mails.SendMail
import Control.Monad.State

docControlTests :: [Test]
docControlTests = 
                     [testGroup "sendDocumentErrorEmail1"
                           [
                             testCase "sends one mail" test_sendDocumentErrorEmail1_sendsOneMail
                           ]
                     ]

test_sendDocumentErrorEmail1_sendsOneMail = do
  let ctx = aTestCtx{ctxmailer=Mailer {sendMail = countMail}}
      doc = anUnsignedDocument
      siglink = head $ documentsignatorylinks doc
  _ <- sendDocumentErrorEmail1 ctx doc siglink
  assertEqual "for mail count" 1 0
    where countMail _ = return ()
