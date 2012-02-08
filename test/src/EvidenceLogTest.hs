module EvidenceLogTest (evidenceLogTests) where

import Control.Monad
import DB.Nexus
import Data.Convertible
import Data.Ix
import EvidenceLog.Model
import Misc
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base (Assertion)
--import Test.QuickCheck
import TestingUtil

evidenceLogTests :: Nexus -> Test
evidenceLogTests _conn = testGroup "Evidence Log" [
      testCase "Testing EvidenceEventType conversions not equal" conversionNEq,
      testCase "Testing EvidenceEventType conversions equal" conversionEq
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
