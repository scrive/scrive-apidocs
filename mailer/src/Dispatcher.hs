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
import qualified Log (mailingServer)
import qualified MemCache
import qualified Network.AWS.Authentication as AWS
import Context
import qualified Data.ByteString as BS
import Control.Monad.State

dispatcher :: AWS.S3Action -> CryptoRNGState -> Sender -> MVar Sender -> String -> IO ()
dispatcher s3action rng master msender dbconf = withPostgreSQL dbconf . runCryptoRNGT rng $ do
  Log.mailingServer $ "Dispatcher is starting"
  filecache <- MemCache.new BS.length 50000000
  now' <- getMinutesTime
  let ctx = Context { ctxs3action    = s3action
                    , ctxfilecache   = filecache
                    , ctxtime        = now'
                    -- FIXME: we should really have a limited MonadAmazon
                    , ctxmaybeuser           = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxhostpart            = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxresourcehostpart    = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxflashmessages       = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxnormalizeddocuments = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxipnumber            = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxproduction          = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxtemplates           = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxglobaltemplates     = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxlang                = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxmailsconfig         = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxlivedocxconf        = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxlogicaconf          = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxgtconf              = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxxtoken              = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxadminaccounts       = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxsalesaccounts       = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxmaybepaduser        = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxstaticresources     = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxusehttps            = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxrecurlyconfig       = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxsessionid           = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxmixpaneltoken       = error "Dispatcher.dispatcher: Context is limited in here"
                    , ctxhomebase            = error "Dispatcher.dispatcher: Context is limited in here"
                    }
  flip evalStateT ctx $ do
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
      kCommit -- commit after email was handled properly
      Log.mailingServer $ "Dispatcher is done"
  where
    isNotSendable Mail{..} =
      null (addrEmail mailFrom) || null mailTo || any (null . addrEmail) mailTo
