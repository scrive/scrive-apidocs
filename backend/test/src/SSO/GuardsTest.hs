module SSO.GuardsTest where

import Control.Monad.Catch
import Data.List.NonEmpty
import Data.Time.Calendar
import Data.Time.Clock
import Network.URI
import SAML2.Core.Assertions
import SAML2.Core.Datatypes
import Test.Framework

import KontraError
import SSO.Guards
import TestingUtil (assertLeft, assertRight, testThat)
import TestKontra

samlConditionsGuardsTests :: TestEnvSt -> Test
samlConditionsGuardsTests env = testGroup
  "SSO SAML guards test"
  [testThat "Test SAML conditions guard" env testConditionsGuard]

day :: Integer -> Int -> Int -> DateTime
day year month dayOfMonth = UTCTime { utctDay     = fromGregorian year month dayOfMonth
                                    , utctDayTime = secondsToDiffTime 0
                                    }

someURI :: URI
someURI = fromJust $ parseURI "http://localhost"

testConditionsGuard :: TestEnv ()
testConditionsGuard = do
  let conditions = Conditions
        { conditionsNotBefore    = Just $ day 2019 12 20
        , conditionsNotOnOrAfter = Just $ day 2019 12 22
        , conditions             = [AudienceRestriction (Audience someURI :| [])]
        }
      tooEarly = day 2019 12 19
      tooLate  = day 2019 12 23
      onDate   = day 2019 12 22
      goodDate = day 2019 12 21

  assertLeft =<< testConditions someURI tooEarly conditions
  assertLeft =<< testConditions someURI tooLate conditions
  assertLeft =<< testConditions someURI onDate conditions
  assertLeft
    =<< testConditions (fromJust . parseURI $ "http://www.amazon.com") goodDate conditions
  assertRight =<< testConditions someURI goodDate conditions
  return ()
  where
    testConditions :: URI -> DateTime -> Conditions -> TestEnv (Either KontraError ())
    testConditions audienceURI dateTime conditions = do
      try $ guardConditionsAreMet audienceURI (\_ -> return dateTime) conditions
