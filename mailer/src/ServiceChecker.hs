module ServiceChecker (
    serviceAvailabilityChecker
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Control.Exception as E

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import MinutesTime
import Sender
import qualified Log (mailingServer)

serviceAvailabilityChecker :: CryptoRNGState -> String -> (Sender, Sender) -> MVar Sender -> IO ()
serviceAvailabilityChecker rng dbconf (master, slave) msender = do
  res <- E.try $ do
    mid <- inDB $ do
      token <- random
      now <- getMinutesTime
      Log.mailingServer $ "Creating service testing email..."
      mid <- dbUpdate $ CreateServiceTest token testSender testReceivers now
      success <- dbUpdate $ AddContentToEmail mid "test" "test" [] mempty
      when (not success) $
        error "CRITICAL: Couldn't add content to created service testing email."
      return mid
    threadDelay freq
    inDB $ do
      events <- dbQuery GetServiceTestEvents
      if any (isDelivered mid) events
        then do
          Log.mailingServer $ "Service testing emails were delivered successfully."
          liftIO $ modifyMVar_ msender $ \sender -> do
            when (sender /= master) $
              Log.mailingServer $ "Restoring service " ++ show master ++ "."
            return master
        else do
          Log.mailingServer $ "Service testing emails failed to be delivered within 5 minutes."
          oldsender <- liftIO $ takeMVar msender
          Log.mailingServer $ "Current sender: " ++ show oldsender
          when (oldsender == master) $ do
            Log.mailingServer $ "Switching to " ++ show slave ++ " and resending all emails that were sent within this time."
            time <- minutesBefore 5 `fmap` getMinutesTime
            n <- dbUpdate $ ResendEmailsSentSince time
            Log.mailingServer $ show n ++ " emails set to be resent."
          liftIO $ putMVar msender slave
      success <- dbUpdate $ DeleteEmail mid
      when (not success) $
        error "CRITICAL: Couldn't delete service testing email."
  case res of
    Right () -> loop
    Left (e::E.SomeException) -> do
      Log.mailingServer $ "Error while testing service availability: " ++ show e ++ ", sleeping for 5 minutes."
      threadDelay freq
      loop
  where
    inDB :: CryptoRNGT (DBT IO) a -> IO a
    inDB = withPostgreSQL dbconf . runCryptoRNGT rng

    isDelivered mid (_, emid, _, SendGridEvent _ SG_Delivered{} _) = mid == emid
    isDelivered mid (_, emid, _, MailGunEvent _ MG_Delivered) = mid == emid
    isDelivered _ _ = False

    freq = 5 * 60 * second
    second = 1000000
    loop = serviceAvailabilityChecker rng dbconf (master, slave) msender

    testSender = Address { addrName = "Scrive mailer", addrEmail = "noreply@scrive.com" }
    testReceivers = [
        Address { addrName = "aol test",   addrEmail = "jdoe278@aol.com"      }
      , Address { addrName = "yahoo test", addrEmail = "mailer_000@yahoo.com" }
      , Address { addrName = "gmail test", addrEmail = "mailer0088@gmail.com" }
      , Address { addrName = "zoho test",  addrEmail = "mailer_000@zoho.com"  }
      ]
