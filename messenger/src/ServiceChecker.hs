module ServiceChecker (
    serviceAvailabilityChecker
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import MinutesTime
import Sender
import qualified Log (mailingServer)


serviceAvailabilityChecker :: CryptoRNGState -> String -> (Sender, Sender) -> MVar Sender -> (forall a. IO a -> IO a)  -> IO ()
serviceAvailabilityChecker rng dbconf (master, slave) msender interruptible = do
    Log.mailingServer $ "Running service checker"
    mid <- inDB $ do
      token <- random
      now <- getMinutesTime
      mid <- dbUpdate $ CreateServiceTest token testSender testReceivers now
      success <- dbUpdate $ AddContentToEmail mid "test" "test" [] mempty
      Log.mailingServer $ "Creating service testing email #" ++ show mid ++ "..."
      when (not success) $
        Log.mailingServer $ "CRITICAL: Couldn't add content to created service testing email."
      return mid
    interruptible $ threadDelay freq
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
          Log.mailingServer $ "Service testing emails failed to be delivered within 6 minutes."
          oldsender <- liftIO $ takeMVar msender
          when (oldsender == master) $ do
            Log.mailingServer $ "Switching to " ++ show slave ++ " and resending all emails that were sent within this time."
            time <- minutesBefore 5 `fmap` getMinutesTime
            n <- dbUpdate $ ResendEmailsSentSince time
            Log.mailingServer $ show n ++ " emails set to be resent."
          liftIO $ putMVar msender slave
      success <- dbUpdate $ DeleteEmail mid
      when (not success) $
        Log.mailingServer $ "Couldn't delete service testing email #" ++ show mid
  where
    inDB :: CryptoRNGT (DBT IO) a -> IO a
    inDB = withPostgreSQL dbconf . runCryptoRNGT rng

    isDelivered mid (_, emid, _, SendGridEvent _ SG_Delivered{} _) = mid == emid
    isDelivered mid (_, emid, _, MailGunEvent _ MG_Delivered) = mid == emid
    isDelivered _ _ = False

    freq = 6 * 60 * second
    second = 1000000

    testSender = Address { addrName = "Scrive mailer", addrEmail = "noreply@scrive.com" }
    testReceivers = [
        Address { addrName = "aol test",   addrEmail = "jdoe278@aol.com"      }
      , Address { addrName = "yahoo test", addrEmail = "mailer_000@yahoo.com" }
      , Address { addrName = "gmail test", addrEmail = "mailer0088@gmail.com" }
      , Address { addrName = "zoho test",  addrEmail = "mailer_000@zoho.com"  }
      ]
