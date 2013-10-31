module EvidenceLogTest (evidenceLogTests) where

import Control.Monad
import Data.Convertible
import Data.Ix
import EvidenceLog.Model
import Utils.Either
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base (Assertion)
import TestingUtil
import TestKontra
import Text.StringTemplates.Templates
import Data.List

evidenceLogTests :: TestEnvSt -> Test
evidenceLogTests env = testGroup "Evidence Log" [
      testCase "Testing EvidenceEventType conversions not equal" conversionNEq,
      testCase "Testing EvidenceEventType conversions equal" conversionEq,
      testThat "Testing EvidenceEventType have texts templates" env evidenceLogEventsHaveTexts
      ]

conversionNEq :: Assertion
conversionNEq = do
  forM_ [(a, b) :: (Int, Int) | a <- range (0, 100), b <- range (0, 1000), a < b] $ \(a,b) -> do
    let a' = safeConvert a :: Either ConvertError EvidenceEventType
        b' = safeConvert b
    when (isRight a' && isRight b') $ do
      assertBool ("These two values " ++ show (a,b) ++ " gave same converstion: " ++ show a') $ a' /= b'
      
conversionEq :: Assertion
conversionEq = do
  forM_ (range (0, 100)) $ \a -> do
    let t = safeConvert (a :: Int) :: Either ConvertError EvidenceEventType
    when (isRight t) $ do
      assertBool ("Back conversion did not work on " ++ show a) $ safeConvert (fromRight t) == Right a

evidenceLogEventsHaveTexts:: TestEnv ()
evidenceLogEventsHaveTexts = forM_ (range (0, 100)) $ \a -> do
    let t = safeConvert (a :: Int) :: Either ConvertError EvidenceEventType
    case t of 
      Right (Current ct) -> do
       text <- renderTemplate_ $ eventTextTemplateName ct
       assertBool ("No text provided for event " ++ show ct) (not $ "No template" `isInfixOf` text)
      _ -> return ()