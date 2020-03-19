module CronMonthlyInvoiceTest (cronMonthlyInvoiceTest) where

import Data.Int
import Test.Framework

import DB
import MinutesTime
import TestCron
import TestingUtil
import TestKontra
import User.Lang (defaultLang)

cronMonthlyInvoiceTest :: TestEnvSt -> Test
cronMonthlyInvoiceTest env =
  testGroup "CronMonthlyInvoiceTest" [testThat "Monthly-invoice email created" env test]

test :: TestEnv ()
test = do
  setTestTime unixEpoch
  runSQL_
    "UPDATE cron_jobs SET run_at = '1970-01-01 00:00:00+00' WHERE id = 'monthly_invoice';"
  modifyTestTime (31 `daysAfter`)

  let selectInvoiceEmails = "SELECT COUNT(*) FROM mails WHERE title = 'Monthly invoice';"

  runSQL_ selectInvoiceEmails
  fetchOne runIdentity >>= assertEqual "Zero emails with monthly invoice" (0 :: Int64)

  -- Run cron job
  ctx <- mkContext defaultLang
  runTestCronUntilIdle ctx

  runSQL_ selectInvoiceEmails
  fetchOne runIdentity >>= assertEqual "One email with monthly invoice" (1 :: Int64)

  runSQL_
    "SELECT COUNT(*) FROM mail_attachments JOIN mails ON (mails.id = mail_attachments.mail_id) WHERE mails.title = 'Monthly invoice'"
  fetchOne runIdentity >>= assertEqual "Two email attachments with reports" (2 :: Int64)
