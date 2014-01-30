module ServiceChecker (
    serviceAvailabilityChecker
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import MailingServerConf
import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import MinutesTime
import Sender
import qualified Log


serviceAvailabilityChecker :: MailingServerConf -> CryptoRNGState -> String -> (Sender, Sender) -> MVar Sender -> (forall a. IO a -> IO a)  -> IO ()
serviceAvailabilityChecker conf rng dbconf (master, slave) msender interruptible = do
    Log.mixlog_ $ "Running service checker"
    mid <- inDB $ do
      token <- random
      now <- getMinutesTime
      mid <- dbUpdate $ CreateServiceTest token testSender (testReceivers conf) now
      success <- dbUpdate $ AddContentToEmail mid "test" "test" [] mempty
      Log.mixlog_ $ "Creating service testing email #" ++ show mid ++ "..."
      when (not success) $
        Log.mixlog_ $ "CRITICAL: Couldn't add content to created service testing email."
      return mid
    interruptible $ threadDelay freq
    inDB $ do
      events <- dbQuery GetServiceTestEvents
      if any (isDelivered mid) events
        then do
          Log.mixlog_ $ "Service testing emails were delivered successfully."
          liftIO $ modifyMVar_ msender $ \sender -> do
            when (sender /= master) $
              Log.mixlog_ $ "Restoring service " ++ show master ++ "."
            return master
        else do
          Log.mixlog_ $ "Service testing emails failed to be delivered within 11 minutes."
          oldsender <- liftIO $ takeMVar msender
          when (oldsender == master) $ do
            Log.mixlog_ $ "Switching to " ++ show slave ++ " and resending all emails that were sent within this time."
            time <- minutesBefore 10 `fmap` getMinutesTime
            n <- dbUpdate $ ResendEmailsSentSince time
            Log.mixlog_ $ show n ++ " emails set to be resent."
          liftIO $ putMVar msender slave
      success <- dbUpdate $ DeleteEmail mid
      when (not success) $
        Log.mixlog_ $ "Couldn't delete service testing email #" ++ show mid
  where
    inDB :: CryptoRNGT (DBT IO) a -> IO a
    inDB = withPostgreSQL dbconf . runCryptoRNGT rng

    isDelivered mid (_, emid, _, SendGridEvent _ SG_Delivered{} _) = mid == emid
    isDelivered mid (_, emid, _, MailGunEvent _ MG_Delivered) = mid == emid
    isDelivered _ _ = False

    freq = 11 * 60 * second
    second = 1000000

    testSender = Address { addrName = "Scrive mailer", addrEmail = "noreply@scrive.com" }
