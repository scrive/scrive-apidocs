module Cleaner (
    cleaner
  ) where

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import qualified Log

cleaner :: CryptoRNGState -> ConnectionSource -> IO ()
cleaner rng cs = withPostgreSQL cs . runCryptoRNGT rng $ do
  Log.mixlog_ $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
  removedCount <- dbUpdate $ DeleteMailsOlderThenDays daylimit
  Log.mixlog_ $ show removedCount ++ " mails were removed."
  where
    daylimit = 14
