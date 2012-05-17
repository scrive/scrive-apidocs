module LiveDocxTest (liveDocxTests) where

-- import Control.Concurrent
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure, Assertion)
import qualified Data.ByteString as BS
import Data.List

import Configuration
import LiveDocx

liveDocxTests :: Test
liveDocxTests = testGroup "LiveDocx"
  [ testGroup "successes"
    [ testCase "LiveDocx can convert a .docx to a .pdf (warning.  undeterministic.  calls live server with test account.)"
               testDocxToPdf
    , testCase "LiveDocx can convert a .doc to a .pdf (warning.  undeterministic.  calls live server with test account.)"
               testDocToPdf
    , testCase "LiveDocx can convert a .rtf to a .pdf (warning.  undeterministic.  calls live server with test account.)"
               testRtfToPdf
--     , testCase "LiveDocx can handle concurrent conversion requests from one account okay (warning.  undeterministic.  calls live server with test account."
--                testConcurrentConversions
    ]
  , testGroup "problems"
    [ testCase "When trying to convert a badly formatted .docx to a .pdf the result will be a Left (warning.  undeterministic.  calls live server with test account.)"
               testConvertingBadDocxGivesLeft
    , testCase "When trying to convert with a bad livedocx url the result will be a Left (warning.  undeterministic.  calls live server with test account.)"
               testConvertingWithBadUrlGivesLeft
    , testCase "When trying to convert with a bad livedocx username the result will be a Left (warning.  undeterministic.  calls live server with test account.)"
               testConvertingWithBadUsernameGivesLeft
    , testCase "When trying to convert with a bad livedocx password the result will be a Left (warning.  undeterministic.  calls live server with test account.)"
               testConvertingWithBadPasswordGivesLeft
    ]
  ]

testConf :: LiveDocxConf
testConf = confDefault

testDocxToPdf :: Assertion
testDocxToPdf = do
  result <- runLiveDocxConversion testConf "sample_1.docx" DOCX
  assertConversionSuccessful result

testDocToPdf :: Assertion
testDocToPdf = do
  result <- runLiveDocxConversion testConf "sample_3.doc" DOC
  assertConversionSuccessful result

testRtfToPdf :: Assertion
testRtfToPdf = do
  result <- runLiveDocxConversion testConf "sample_4.rtf" RTF
  assertConversionSuccessful result
{-
testConcurrentConversions :: Assertion
testConcurrentConversions = do
  mvar1 <- newEmptyMVar
  mvar2 <- newEmptyMVar
  mvar3 <- newEmptyMVar
  _tid1 <- forkIO $ (putMVar mvar1) =<<
                     (runLiveDocxConversion testConf "sample_1.docx" DOCX)
  _tid2 <- forkIO $ (putMVar mvar2) =<<
                     (runLiveDocxConversion testConf "sample_3.doc" DOC)
  _tid3 <- forkIO $ (putMVar mvar3) =<<
                     (runLiveDocxConversion testConf "sample_4.rtf" RTF)
  res1 <- takeMVar mvar1
  res2 <- takeMVar mvar2
  res3 <- takeMVar mvar3
  assertConversionSuccessful res1
  assertConversionSuccessful res2
  assertConversionSuccessful res3-}

testConvertingBadDocxGivesLeft :: Assertion
testConvertingBadDocxGivesLeft = do
  result <- runLiveDocxConversion testConf "sample_2_bad.docx" DOCX
  assertConversionFailed result

testConvertingWithBadUrlGivesLeft :: Assertion
testConvertingWithBadUrlGivesLeft = do
  -- this is the http rather than https url, so should fail
  let badConf = testConf { url = "http://api.livedocx.com/1.2/mailmerge.asmx" }
  result <- runLiveDocxConversion badConf "sample_1.docx" DOCX
  assertConversionFailed result

testConvertingWithBadUsernameGivesLeft :: Assertion
testConvertingWithBadUsernameGivesLeft = do
  let badConf = testConf { username = "nottheusername" }
  result <- runLiveDocxConversion badConf "sample_1.docx" DOCX
  assertConversionFailed result

testConvertingWithBadPasswordGivesLeft :: Assertion
testConvertingWithBadPasswordGivesLeft = do
  let badConf = testConf { password = "notthepassword" }
  result <- runLiveDocxConversion badConf "sample_1.docx" DOCX
  assertConversionFailed result

runLiveDocxConversion :: LiveDocxConf -> String -> FileFormat -> IO (Either LiveDocxError BS.ByteString)
runLiveDocxConversion conf filename format = do
  contents <- BS.readFile $ "docconv/test/resources/" ++ filename
  convertToPDF conf contents format

assertConversionSuccessful :: Either LiveDocxError BS.ByteString -> Assertion
assertConversionSuccessful (Left err) = if ("Request for principal permission failed." `isInfixOf` (show err))
                                           -- We use free account for tests. This is the error that you may get because of that.
                                           then return ()
                                           else assertFailure $ "conversion failed with error: " ++ show err
assertConversionSuccessful (Right _) = return ()

assertConversionFailed :: Either LiveDocxError a -> Assertion
assertConversionFailed (Left _err) = return ()
assertConversionFailed (Right _) = assertFailure "conversion should've failed but it gave a result"
