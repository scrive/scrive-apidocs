{-# LANGUAGE RecordWildCards #-}
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
    Log.mailingServer $ "Removing mails that were (or should be) send " ++ show daylimit ++ " days ago."
    removedCount <- dbUpdate $ DeleteMailsOlderThenDays daylimit
    Log.mailingServer $ (show removedCount) ++ " mails were removed."
    dbCommit -- commit after email was handled properly
  case res of
    Right () -> Log.mailingServer $ "Cleaner done. Next run in " ++ (show freq) ++ " secunds."
    Left (e::E.SomeException) -> Log.mailingServer $ "Cleaner failed with " ++ (show $ e) ++ ". Next run in " ++ (show freq) ++ " secunds."
  threadDelay $ freq * second
  cleaner rng dbconf
  where
    freq = 24 * 60
    second = 1000000
    daylimit = 2
    
