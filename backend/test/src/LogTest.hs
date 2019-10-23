module LogTest (logTests) where

import Control.Monad.IO.Class
import Log
import Log.Backend.Text
import Test.Framework
import qualified Data.Text as T

import Doc.DocumentID
import Log.Identifier
import TestingUtil
import TestKontra

logTests :: TestEnvSt -> Test
logTests env = testGroup
  "Log"
  [testThat "Int64 DocumentID:s are formatted properly" env testDocIDFormatting]

testDocIDFormatting :: TestEnv ()
testDocIDFormatting = do
  (txt, ()) <- liftIO $ withSimpleTextLogger (\logger -> runLogT "LogTest" logger logAct)
  assertBool ("Document ID formatted as floating-point: " ++ T.unpack txt)
             (not $ "1.23456789" `T.isInfixOf` txt)
  assertBool ("Document ID formatted incorrectly: " ++ T.unpack txt)
             ("123456789" `T.isInfixOf` txt)
  where
    dummyDocumentID :: DocumentID
    dummyDocumentID = unsafeDocumentID 123456789

    logAct :: LogT IO ()
    logAct = localData [identifier dummyDocumentID] $ logInfo_ "foo"
