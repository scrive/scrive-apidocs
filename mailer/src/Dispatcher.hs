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
import Mails.Model
import MinutesTime
import qualified Amazon as AWS
import qualified Log

dispatcher :: CryptoRNGState -> Sender -> MVar Sender -> String -> AWS.AmazonConfig -> IO ()
dispatcher rng master msender dbconf amazonconf = withPostgreSQL dbconf . runCryptoRNGT rng . AWS.runAmazonMonadT amazonconf $ do
  Log.mixlog_ $ "Dispatcher is starting"
  do
    mails <- dbQuery GetIncomingEmails
    Log.mixlog_ $ "Batch of mails to send is " ++ show (length mails) ++ " email(s) long."
    forM_ mails $ \mail@Mail{mailID, mailServiceTest} -> do
      if isNotSendable mail
       then do
         Log.mixlog_ $ "Email " ++ show mail ++ " is not sendable, discarding."
         success <- dbUpdate $ DeleteEmail mailID
         when (not success) $
           Log.mixlog_ $ "Couldn't remove unsendable email #" ++ show mailID ++ " from database."
       else do
         -- we want to send service testing emails always with master service
         mailer <- if mailServiceTest
                   then return master
                   else liftIO $ readMVar msender

         Log.mixlog_ $ "Sending email #" ++ show mailID ++ " using " ++ show mailer ++ "..."
         success <- sendMail mailer mail
         now <- getMinutesTime
         if success
           then do
             res <- dbUpdate $ MarkEmailAsSent mailID now
             when (not res) $
               Log.mixlog_ $ "Failed to mark email #" ++ show mailID ++ " as sent."
           else do
             Log.mixlog_ $ "Failed to send email #" ++ show mailID
             if (mailAttempt mail < 100)
                then do
                  Log.mixlog_ $ "Deferring email #" ++ show mailID ++" for 5 minutes"
                  res <- dbUpdate $ DeferEmail mailID $ 5 `minutesAfter` now
                  when (not res) $
                    Log.mixlog_ $ "Failed to defer email #" ++ show mailID ++ " sendout."
                else do
                  Log.mixlog_ $ "Deleting email #" ++ show mailID ++" since there was over 100 tries to send it"
                  res <- dbUpdate $ DeleteEmail mailID
                  when (not res) $
                    Log.mixlog_ $ "Deleting email #" ++ show mailID ++ " failed."
      kCommit -- commit after email was handled properly
      Log.mixlog_ $ "Dispatcher is done"
  where
    isNotSendable Mail{..} =
      null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo
