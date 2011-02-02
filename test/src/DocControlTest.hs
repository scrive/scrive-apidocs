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
import Data.IORef

docControlTests :: [Test]
docControlTests = 
                     [--testGroup "sendDocumentErrorEmail1"
                           [
                      --       testCase "sends one mail" test_sendDocumentErrorEmail1_sendsOneMail
                           ]
                     ]

--test_sendDocumentErrorEmail1_sendsOneMail = do
--  counter <- newIORef 0
--  let ctx = aTestCtx{ctxmailer=countingMailer counter}
--      doc = anUnsignedDocument
--      siglink = head $ documentsignatorylinks doc
--  sendDocumentErrorEmail1 ctx doc siglink
--  numberSent <- readIORef counter
--  assertEqual "for mail count" 1 numberSent
--    where countMail _ = return ()

--countingMailer counter mail = do
--    modifyIORef counter $ (+) 1

