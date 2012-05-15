module Cleaner (
    cleaner
  ) where

import Control.Concurrent
import qualified Control.Exception as E

import Crypto.RNG
import DB
import DB.PostgreSQL
import Mails.Model
import qualified Log (mailingServer)

cleaner :: CryptoRNGState -> String -> IO ()
cleaner rng dbconf = do
  res <- E.try . withPostgreSQL dbconf . runCryptoRNGT rng $ do
    Log.mailingServer $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
    removedCount <- dbUpdate $ DeleteMailsOlderThenDays daylimit
    Log.mailingServer $ show removedCount ++ " mails were removed."
  case res of
    Right () -> Log.mailingServer $ "Cleaner done. Next run in " ++ show freq ++ " seconds."
    Left (e::E.SomeException) -> Log.mailingServer $ "Cleaner failed with " ++ show e ++ ". Next run in " ++ show freq ++ " seconds."
  threadDelay $ freq * second
  cleaner rng dbconf
  where
    freq = 24 * 60 * 60 -- 24 hours
    second = 1000000
    daylimit = 14
