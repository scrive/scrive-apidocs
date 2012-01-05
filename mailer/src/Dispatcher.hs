{-# LANGUAGE RecordWildCards #-}
module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent
import Control.Monad
import Database.HDBC.PostgreSQL
import qualified Control.Exception as E

import DB.Classes
import Sender
import Mails.Model
import MinutesTime
import qualified Log (mailingServer)

dispatcher :: Sender -> String -> IO ()
dispatcher sender dbconf = withPostgreSQL dbconf send
  where
    send conn = do
      res <- E.try $ ioRunDB conn $ do
        mails <- dbQuery GetIncomingEmails
        forM_ mails $ \mail -> do
          if mailIsNotSendable mail
            then do
              Log.mailingServer $ "Email " ++ show mail ++ " is not sendable, discarding."
              success <- dbUpdate $ DeleteEmail $ mailID mail
              when (not success) $
                Log.mailingServer $ "Couldn't remove email #" ++ show mailID ++ " from database."
            else do
              success <- sendMail sender mail
              if success
                then do
                  time <- getMinutesTime
                  success <- dbUpdate $ MarkEmailAsSent (mailID mail) time
                  when (not success) $
                    error $ "Marking email #" ++ show (mailID mail) ++ " as sent failed"
                else error "Sending email failed"
      case res of
        Right () -> threadDelay $ 5 * second
        Left (e::E.SomeException) -> do
          Log.mailingServer $ "Exception '" ++ show e ++ "' thrown while sending emails, sleeping for 10 minutes"
          threadDelay $ 10 * 60 * second
      send conn
    second = 1000000
    mailIsNotSendable Mail{..} =
      null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo
