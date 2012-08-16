module Cleaner (
    cleaner
  ) where

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import qualified Log (mailingServer)

cleaner :: CryptoRNGState -> String -> IO ()
cleaner rng dbconf = withPostgreSQL dbconf . runCryptoRNGT rng $ do
  Log.mailingServer $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
  removedCount <- dbUpdate $ DeleteMailsOlderThenDays daylimit
  Log.mailingServer $ show removedCount ++ " mails were removed."
  where
    daylimit = 14
