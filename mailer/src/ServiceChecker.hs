module ServiceChecker (
    serviceAvailabilityChecker
  ) where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Monoid

import Crypto.RNG
import DB
import DB.PostgreSQL
import MailingServerConf
import Mails.Model
import MinutesTime
import Sender
import qualified Log

serviceAvailabilityChecker :: forall m . (MonadBaseControl IO m, Log.MonadLog m, MonadMask m, MonadIO m, Monad m, Functor m, MonadBase IO m) => MailingServerConf -> CryptoRNGState -> ConnectionSource -> (Sender, Sender) -> MVar Sender -> (forall a. m a -> m a)  -> m ()
serviceAvailabilityChecker conf rng cs (master, slave) msender interruptible = do
    Log.mixlog_ $ "Running service checker"
    mid <- inDB $ do
      token <- random
      mid <- dbUpdate $ CreateServiceTest token testSender (testReceivers conf)
      success <- dbUpdate $ AddContentToEmail mid "test" (Just testSender) "test" [] mempty
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
          modifyMVar_ msender $ \sender -> do
            when (sender /= master) $
              Log.mixlog_ $ "Restoring service " ++ show master ++ "."
            return master
        else do
          Log.mixlog_ $ "Service testing emails failed to be delivered within 11 minutes."
          oldsender <- takeMVar msender
          when (oldsender == master) $ do
            Log.mixlog_ $ "Switching to " ++ show slave ++ " and resending all emails that were sent within this time."
            time <- minutesBefore 10 `fmap` currentTime
            n <- dbUpdate $ ResendEmailsSentSince time
            Log.mixlog_ $ show n ++ " emails set to be resent."
          putMVar msender slave
      success <- dbUpdate $ DeleteEmail mid
      when (not success) $
        Log.mixlog_ $ "Couldn't delete service testing email #" ++ show mid
  where
    inDB :: CryptoRNGT (DBT m) a -> m a
    inDB = withPostgreSQL cs . runCryptoRNGT rng

    isDelivered mid (_, emid, _, SendGridEvent _ SG_Delivered{} _) = mid == emid
    isDelivered mid (_, emid, _, MailGunEvent _ MG_Delivered) = mid == emid
    isDelivered _ _ = False

    freq = 11 * 60 * second
    second = 1000000

    testSender = Address { addrName = "Scrive mailer", addrEmail = "noreply@scrive.com" }
