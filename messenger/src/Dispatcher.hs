{-# LANGUAGE RecordWildCards #-}
module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class

import Crypto.RNG
import DB
import DB.PostgreSQL
import MinutesTime
import Sender
import SMS.Data
import SMS.Model
import qualified Log

dispatcher :: (Log.MonadLog m, MonadIO m, MonadMask m, MonadBase IO m) => CryptoRNGState -> Sender -> MVar Sender -> ConnectionSource -> m ()
dispatcher rng _master msender cs = withPostgreSQL cs . runCryptoRNGT rng $ do
    Log.mixlog_ $ "SMS Dispatcher is starting"
    smses <- dbQuery GetIncomingSMSes
    Log.mixlog_ $ "Batch of smses to send is " ++ show (length smses) ++ " sms(es) long."
    forM_ smses $ \sms@ShortMessage{smID} -> do
         -- we want to send service testing emails always with master service
         messenger <- readMVar msender

         Log.mixlog_ $ "Sending sms #" ++ show smID ++ " using " ++ show messenger ++ "..."
         success <- sendSMS messenger sms
         now <- currentTime
         if success
           then do
             res <- dbUpdate $ MarkSMSAsSent smID now
             when (not res) $
               Log.mixlog_ $ "Failed to mark sms #" ++ show smID ++ " as sent."
           else do
             Log.mixlog_ $ "Failed to send sms #" ++ show smID
             if (smAttempt sms < 100)
                then do
                  Log.mixlog_ $ "Deferring sms #" ++ show smID ++" for 5 minutes"
                  res <- dbUpdate $ DeferSMS smID $ 5 `minutesAfter` now
                  when (not res) $
                    Log.mixlog_ $ "Failed to defer sms #" ++ show smID ++ " sendout."
                else do
                  Log.mixlog_ $ "Deleting sms #" ++ show smID ++" since there was over 100 tries to send it"
                  res <- dbUpdate $ DeleteSMS smID
                  when (not res) $
                    Log.mixlog_ $ "Deleting sms #" ++ show smID ++ " failed."
         commit -- commit after sms was handled properly
    Log.mixlog_ $ "SMS Dispatcher is done"
