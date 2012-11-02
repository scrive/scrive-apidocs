{-# LANGUAGE CPP #-}
module CronMain where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import System.Environment
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
  rng <- newCryptoRNGState

  let runScheduler = runQueue rng (dbConfig appConf) (SchedulerData appConf templates)
      inDB = withPostgreSQL (dbConfig appConf) . runCryptoRNGT rng
  t1 <- forkIO . cron (60 * 10) $ do
    Log.cron "Running timeoutDocuments..."
    runScheduler timeoutDocuments
  t2 <- forkIO . cron (60 * 60) $ do
    Log.cron "Evaluating EmailChangeRequest actions..."
    runScheduler $ actionQueue emailChangeRequest
  t3 <- forkIO . cron (60 * 60) $ do
    Log.cron "Evaluating PasswordReminder actions..."
    runScheduler $ actionQueue passwordReminder
  t4 <- forkIO . cron (60 * 60) $ do
    Log.cron "Evaluating UserAccountRequest actions..."
    runScheduler $ actionQueue userAccountRequest
  t5 <- forkIO . cron (60 * 60) $ do
    Log.cron "Evaluating sessions..."
    runScheduler $ actionQueue session
  t6 <- forkIO . cron 5 $ runScheduler processEvents
  t7 <- forkIO $ cron 10 $ runScheduler $ actionQueue documentAPICallback
  t8 <- forkIO $ if AWS.isAWSConfigOk appConf
                    then cron 60 $ runScheduler AWS.uploadFilesToAmazon
                    else return ()
  t9 <- forkIO . cron (60 * 60) . inDB $ do
    mtime <- getMinutesTime
    ctime <- liftIO $ System.Time.toCalendarTime (toClockTime mtime)
    temps <- snd `liftM` liftIO (readMVar templates)
    when (System.Time.ctHour ctime == 0) $ -- midnight
      handleSyncWithRecurly (hostpart appConf) (mailsConfig appConf)
        temps (recurlyAPIKey $ recurlyConfig appConf) mtime

  waitForTermination
  Log.cron $ "Termination request received"
  mapM_ killThread [t1, t2, t3, t4, t5, t6, t7, t8, t9]
