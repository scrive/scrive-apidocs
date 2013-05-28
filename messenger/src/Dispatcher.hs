{-# LANGUAGE RecordWildCards #-}
module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Crypto.RNG
import DB
import DB.PostgreSQL
import Sender
import SMS.Model
import SMS.Data
import MinutesTime
import qualified Log (messengerServer)

dispatcher :: CryptoRNGState -> Sender -> MVar Sender -> String -> IO ()
dispatcher rng _master msender dbconf = withPostgreSQL dbconf . runCryptoRNGT rng $ do
    Log.messengerServer $ "SMS Dispatcher is starting"
    smses <- dbQuery GetIncomingSMSes
    Log.messengerServer $ "Batch of smses to send is " ++ show (length smses) ++ " sms(es) long."
    forM_ smses $ \sms@ShortMessage{smID} -> do
         -- we want to send service testing emails always with master service
         messenger <- liftIO $ readMVar msender

         Log.messengerServer $ "Sending sms #" ++ show smID ++ " using " ++ show messenger ++ "..."
         success <- sendSMS messenger sms
         now <- getMinutesTime
         if success
           then do
             res <- dbUpdate $ MarkSMSAsSent smID now
             when (not res) $
               Log.messengerServer $ "Failed to mark sms #" ++ show smID ++ " as sent."
           else do
             Log.messengerServer $ "Failed to send sms #" ++ show smID ++ " (deferring this sms for 5 minutes)."
             res <- dbUpdate $ DeferSMS smID $ 5 `minutesAfter` now
             when (not res) $
               Log.messengerServer $ "Failed to defer sms #" ++ show smID ++ " sendout."
         kCommit -- commit after email was handled properly
    Log.messengerServer $ "SMS Dispatcher is done"
