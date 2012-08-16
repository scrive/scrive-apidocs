{-# LANGUAGE CPP #-}
module CronMain where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import System.Environment
import qualified Control.Concurrent.Thread.Group as TG
import qualified System.Time

import ActionQueue.EmailChangeRequest
import ActionQueue.Monad
import ActionQueue.PasswordReminder
import ActionQueue.Scheduler
import ActionQueue.UserAccountRequest
import AppConf
import AppDB
import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.SQLFunction
import DB.PostgreSQL
import Doc.API.Callback.Model
import Utils.Cron
import Utils.IO
import Mails.Events
import MinutesTime
import Payments.Config
import Payments.Control
import Session.Data
import Templates.TemplatesLoader
import qualified Amazon as AWS
import qualified Log (cron, withLogger)

main :: IO ()
main = Log.withLogger $ do
  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig Log.cron appname args "kontrakcja.conf"

  withPostgreSQL (dbConfig appConf) $ do
    performDBChecks Log.cron kontraTables kontraMigrations
    runDBEnv $ defineMany kontraFunctions

  templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
  tg <- TG.new
  rng <- newCryptoRNGState

  let runScheduler = runQueue rng (dbConfig appConf) (SchedulerData appConf templates)
      inDB = liftIO . withPostgreSQL (dbConfig appConf) . runCryptoRNGT rng
  t1 <- forkCron tg "timeoutDocuments" (60 * 10) $ do
    Log.cron "Running timeoutDocuments..."
    runScheduler timeoutDocuments
  t2 <- forkCron tg "EmailChangeRequests" (60 * 60) $ do
    Log.cron "Evaluating EmailChangeRequest actions..."
    runScheduler $ actionQueue emailChangeRequest
  t3 <- forkCron tg "PasswordReminders" (60 * 60) $ do
    Log.cron "Evaluating PasswordReminder actions..."
    runScheduler $ actionQueue passwordReminder
  t4 <- forkCron tg "UserAccountRequests" (60 * 60) $ do
    Log.cron "Evaluating UserAccountRequest actions..."
    runScheduler $ actionQueue userAccountRequest
  t5 <-  forkCron tg "Sessions" (60 * 60) $ do
    Log.cron "Evaluating sessions..."
    runScheduler $ actionQueue session
  t6 <- forkCron tg "EventsProcessing" 5 $ do
    runScheduler processEvents
  t7 <- forkCron tg "DocumentAPICallback" 10 $ do
    runScheduler $ actionQueue documentAPICallback
  t8 <- forkCron tg "RecurlySync" (60 * 60) . inDB $ do
    mtime <- getMinutesTime
    ctime <- liftIO $ System.Time.toCalendarTime (toClockTime mtime)
    temps <- snd `liftM` liftIO (readMVar templates)
    when (System.Time.ctHour ctime == 0) $ -- midnight
      handleSyncWithRecurly (hostpart appConf) (mailsConfig appConf)
        temps (recurlyAPIKey $ recurlyConfig appConf) mtime
  t9 <- if AWS.isAWSConfigOk appConf
          then return <$> (forkCron tg "AmazonUploading" 60 $ runScheduler AWS.uploadFilesToAmazon)
          else return []

  waitForTermination
  Log.cron $ "Termination request received, waiting for jobs to finish..."
  mapM_ stopCron (t1:t2:t3:t4:t5:t6:t7:t8:t9)
  TG.wait tg
