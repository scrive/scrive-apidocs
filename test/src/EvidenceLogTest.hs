module EvidenceLogTest (evidenceLogTests) where

import Control.Monad
import Data.Int
import Data.List
import Data.Monoid.Space
import Test.Framework
import Text.StringTemplates.Templates

import DB
import EvidenceLog.Model
import TestingUtil
import TestKontra

evidenceLogTests :: TestEnvSt -> Test
evidenceLogTests env = testGroup "Evidence Log" [
      testThat "Testing EvidenceEventType conversions equal" env conversionEq,
      testThat "Testing EvidenceEventType have texts templates" env evidenceLogEventsHaveTexts
      ]

evidenceEventTypes :: [(Int16, EvidenceEventType)]
evidenceEventTypes = zip [1..] $ concat [
    map Current [minBound..maxBound]
  , map Obsolete [minBound..maxBound]
  ]

conversionEq :: TestEnv ()
conversionEq = do
  let eventsNum = fromIntegral (length evidenceEventTypes) :: Int16
  forM_ evidenceEventTypes $ \(n, t) -> do
    -- test that fromSQL . toSQL == id (fromSQL is onto and toSQL is
    -- bijection as function EvidenceEventType -> {1..eventsNum})
    runQuery_ $ rawSQL "SELECT $1, $1 >= 1 AND $1 <= $2" (t, eventsNum)
    (t', True) <- fetchOne id
    assertBool ("fromSQL . toSQL /= id on " ++ show t) $ t' == t

    -- test that id . toSQL . fromSQL == id (fromSQL|{1..eventsNum} is a bijection)
    runSQL_ $ "SELECT" <+> unsafeSQL (show n) <+> "::int2"
    tt :: EvidenceEventType <- fetchOne unSingle
    runSQL_ $ "SELECT" <?> tt
    n' <- fetchOne unSingle
    assertBool ("toSQL . fromSQL /= id on " ++ show n) $ n' == n

evidenceLogEventsHaveTexts:: TestEnv ()
evidenceLogEventsHaveTexts = forM_ evidenceEventTypes $ \(_, t) -> do
    case t of
      Current ct -> do
       text <- renderTemplate_ $ eventTextTemplateName ct
       assertBool ("No text provided for event " ++ show ct) (not $ "No template" `isInfixOf` text)
      _ -> return ()
