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
  Log.mixlog_ $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
  removedCount <- dbUpdate $ DeleteSMSesOlderThenDays daylimit
  Log.mixlog_ $ show removedCount ++ " mails were removed."
  where
    daylimit = 3
