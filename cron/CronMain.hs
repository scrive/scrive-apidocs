{-# LANGUAGE CPP #-}
module CronMain where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import System.Environment
import qualified System.Time
import Data.Maybe (catMaybes)

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
import HostClock.Collector (collectClockError)
import Session.Data
import Templates
import Doc.Model
import qualified Amazon as AWS
import qualified Log (cron, withLogger, error)

import ThirdPartyStats.Core
import ThirdPartyStats.Mixpanel
import ThirdPartyStats.Precog
import Precog.Ingest (precogConfig)

main :: IO ()
main = Log.withLogger $ do
  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig Log.cron appname args "kontrakcja.conf"

  withPostgreSQL (dbConfig appConf) $ do
    performDBChecks Log.cron kontraTables kontraMigrations
    defineMany kontraFunctions

  templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
  rng <- newCryptoRNGState

  let runScheduler = runQueue rng (dbConfig appConf) (SchedulerData appConf templates)
      inDB = liftIO . withPostgreSQL (dbConfig appConf) . runCryptoRNGT rng
  -- Asynchronous event dispatcher; if you want to add a consumer to the event
  -- dispatcher, please combine the two into one dispatcher function rather
  -- than creating a new thread or something like that, since
  -- asyncProcessEvents removes events after processing.
  mmixpanel <- case mixpanelToken appConf of
    ""    -> Log.error "WARNING: no Mixpanel token present!" >> return Nothing
    token -> return $ Just $ processMixpanelEvent token

  let precogcfg = (precogHost appConf,
                   precogKey appConf,
                   precogRootPath appConf,
                   precogPathPrefix appConf)
  mprecog <- case precogcfg of
    (host@(_:_), key@(_:_), root@(_:_), prefix) ->
      return
        $ Just
        $ processPrecogEvent
        $ precogConfig host key root (Just prefix)
    _ ->
      Log.error "WARNING: no Precog credentials!" >> return Nothing

  withCronJobs
    ([ forkCron_ True "timeoutDocuments" (60 * 10) $ do
         Log.cron "Running timeoutDocuments..."
         runScheduler timeoutDocuments
     , forkCron_ True "EmailChangeRequests" (60 * 60) $ do
         Log.cron "Evaluating EmailChangeRequest actions..."
         runScheduler $ actionQueue emailChangeRequest
     , forkCron_ True "PasswordReminders" (60 * 60) $ do
         Log.cron "Evaluating PasswordReminder actions..."
         runScheduler $ actionQueue passwordReminder
     , forkCron_ True "UserAccountRequests" (60 * 60) $ do
         Log.cron "Evaluating UserAccountRequest actions..."
         runScheduler $ actionQueue userAccountRequest
     , forkCron False "Clock error collector" (60 * 60) $
                       withPostgreSQL (dbConfig appConf) .
                       collectClockError (ntpServers appConf)
     , forkCron_ True "Sessions" (60 * 60) $ do
         Log.cron "Evaluating sessions..."
         runScheduler $ actionQueue session
     , forkCron_ True "EventsProcessing" 5 $ do
         runScheduler processEvents
     , forkCron_ True "DocumentAPICallback" 10 $ do
         runScheduler $ actionQueue documentAPICallback
     , forkCron_ True "RecurlySync" (60 * 60) . inDB $ do
         mtime <- getMinutesTime
         ctime <- liftIO $ System.Time.toCalendarTime (toClockTime mtime)
         temps <- snd `liftM` liftIO (readMVar templates)
         when (System.Time.ctHour ctime == 0) $ do -- midnight
           handleSyncWithRecurly (hostpart appConf) (mailsConfig appConf)
             temps (recurlyAPIKey $ recurlyConfig appConf) mtime
           handleSyncNoProvider mtime
     ] ++ (if AWS.isAWSConfigOk appConf
           then [forkCron_ True "AmazonUploading" 60 $ runScheduler AWS.uploadFilesToAmazon]
           else []) ++
     [ forkCron_ True "removeOldDrafts" (60 * 60) $ do
         Log.cron "Removing old, unsaved draft documents..."
         runScheduler $ do
           delCount <- dbUpdate $ RemoveOldDrafts 100
           Log.cron $ "Removed " ++ show delCount ++ " old, unsaved draft documents."
     , forkCron_ True "Async Event Dispatcher" (10) . inDB $ do
         asyncProcessEvents (catEventProcs $ catMaybes [mmixpanel, mprecog]) All
     ]) $ \_ -> do
       waitForTermination
       Log.cron $ "Termination request received, waiting for jobs to finish..."
