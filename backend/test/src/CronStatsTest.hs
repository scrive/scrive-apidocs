module CronStatsTest (cronStatsTest) where

import Test.Framework

import CronStats.Model
import DB
import TestingUtil
import TestKontra

cronStatsTest :: TestEnvSt -> Test
cronStatsTest env = testGroup
  "CronStats"
  [testThat "CronStats can be generated" env test_CronStatsGeneration]

test_CronStatsGeneration :: TestEnv ()
test_CronStatsGeneration = do
  _ <- dbQuery GetCronStats
  return ()
