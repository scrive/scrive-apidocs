{-# LANGUAGE RecordWildCards #-}
module Dispatcher (
    dispatcher
  ) where

import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E

import Crypto.RNG (CryptoRNGState)
import DB.Classes
import Sender
import Mails.Model
import MinutesTime
import qualified Log (mailingServer)

dispatcher :: CryptoRNGState -> Sender -> String -> IO ()
dispatcher rng sender dbconf = do
  res <- withPostgreSQLDB' dbconf rng $ \dbenv -> E.try $ ioRunDB dbenv $ do
    mails <- dbQuery GetIncomingEmails
    forM_ mails $ \mail@Mail{mailID} -> do
      if isNotSendable mail
       then do
         Log.mailingServer $ "Email " ++ show mail ++ " is not sendable, discarding."
         success <- dbUpdate $ DeleteEmail mailID
         when (not success) $
           Log.mailingServer $ "CRITICAL: couldn't remove unsendable email #" ++ show mailID ++ " from database."
       else do
         Log.mailingServer $ "Sending email #" ++ show mailID ++ "..."
         success <- sendMail sender mail
         now <- getMinutesTime
         if success
           then do
             res <- dbUpdate $ MarkEmailAsSent mailID now
             when (not res) $
               error $ "CRITICAL: marking email #" ++ show mailID ++ " as sent failed."
           else do
             Log.mailingServer $ "Sending email #" ++ show mailID ++ " failed, deferring for 5 minutes."
             res <- dbUpdate $ DeferEmail mailID $ 5 `minutesAfter` now
             when (not res) $
               error $ "CRITICAL: deferring email #" ++ show mailID ++ " failed."
  case res of
    Right () -> threadDelay $ 5 * second
    Left (e::E.SomeException) -> do
      Log.mailingServer $ "Exception '" ++ show e ++ "' thrown while sending emails, sleeping for five minutes."
      threadDelay $ 5 * 60 * second
  dispatcher rng sender dbconf
  where
    second = 1000000
    isNotSendable Mail{..} =
      null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo
