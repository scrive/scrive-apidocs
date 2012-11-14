{-# LANGUAGE RecordWildCards #-}
module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E

import Crypto.RNG
import DB
import DB.PostgreSQL
import Sender
import Mails.Model
import MinutesTime
import qualified Log (mailingServer)

dispatcher :: CryptoRNGState -> Sender -> MVar Sender -> String -> IO ()
dispatcher rng master msender dbconf = do
  res <- E.try . withPostgreSQL dbconf . runCryptoRNGT rng $ do
    mails <- dbQuery GetIncomingEmails
    Log.mailingServer $ "Batch of mails to send is " ++ show (length mails) ++ " email(s) long."
    forM_ mails $ \mail@Mail{mailID, mailServiceTest} -> do
      if isNotSendable mail
       then do
         Log.mailingServer $ "Email " ++ show mail ++ " is not sendable, discarding."
         success <- dbUpdate $ DeleteEmail mailID
         when (not success) $
           Log.mailingServer $ "Couldn't remove unsendable email #" ++ show mailID ++ " from database."
       else do
         -- we want to send service testing emails always with master service
         mailer <- if mailServiceTest
                   then return master
                   else liftIO $ readMVar msender

         Log.mailingServer $ "Sending email #" ++ show mailID ++ " using " ++ show mailer ++ "..."
         success <- sendMail mailer mail
         now <- getMinutesTime
         if success
           then do
             res <- dbUpdate $ MarkEmailAsSent mailID now
             when (not res) $
               Log.mailingServer $ "Failed to mark email #" ++ show mailID ++ " as sent."
           else do
             Log.mailingServer $ "Failed to send email #" ++ show mailID ++ " (deferring this email for 5 minutes)."
             res <- dbUpdate $ DeferEmail mailID $ 5 `minutesAfter` now
             when (not res) $
               Log.mailingServer $ "Failed to defer email #" ++ show mailID ++ " sendout."
      dbCommit -- commit after email was handled properly
  case res of
    Right () -> threadDelay $ 5 * second
    Left (e::E.SomeException) -> do
      Log.mailingServer $ "Exception '" ++ show e ++ "' thrown while sending emails, sleeping for five minutes."
      threadDelay $ 5 * 60 * second
  dispatcher rng master msender dbconf
  where
    second = 1000000
    isNotSendable Mail{..} =
      null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo
