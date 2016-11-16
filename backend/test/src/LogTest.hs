module LogTest (logTests) where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Log
import Log.Backend.Text
import Test.Framework

import Doc.DocumentID
import KontraPrelude
import Log.Identifier
import TestKontra
import TestingUtil

logTests :: TestEnvSt -> Test
logTests env = testGroup "Log" [
  testThat "Int64 DocumentID:s are formatted properly" env testDocIDFormatting
  ]

testDocIDFormatting :: TestEnv ()
testDocIDFormatting = do
  (txt, ()) <- liftIO $ withSimpleTextLogger
               (\logger -> runLogT "LogTest" logger logAct)
  assertBool ("Document ID formatted as floating-point: " ++ T.unpack txt)
    (not $ "1.23456789" `T.isInfixOf` txt)
    where
      dummyDocumentID :: DocumentID
      dummyDocumentID = unsafeDocumentID 123456789

      logAct :: LogT IO ()
      logAct = localData [identifier_ dummyDocumentID] $
               logInfo_ "foo"
