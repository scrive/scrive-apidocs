module Cleaner (
    cleaner
  ) where

import Crypto.RNG
import DB
import DB.PostgreSQL
import SMS.Model
import qualified Log

cleaner :: CryptoRNGState -> String -> IO ()
cleaner rng dbconf = withPostgreSQL dbconf . runCryptoRNGT rng $ do
  Log.messengerServer $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
  removedCount <- dbUpdate $ DeleteSMSesOlderThenDays daylimit
  Log.messengerServer $ show removedCount ++ " mails were removed."
  where
    daylimit = 3
