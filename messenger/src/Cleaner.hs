module Cleaner (
    cleaner
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class

import Crypto.RNG
import DB
import DB.PostgreSQL
import SMS.Model
import qualified Log

cleaner :: (Log.MonadLog m, MonadIO m, MonadMask m, MonadBase IO m) => CryptoRNGState -> ConnectionSource -> m ()
cleaner rng cs = withPostgreSQL cs . runCryptoRNGT rng $ do
  Log.mixlog_ $ "Removing mails that were (or should be) sent " ++ show daylimit ++ " days ago."
  removedCount <- dbUpdate $ DeleteSMSesOlderThenDays daylimit
  Log.mixlog_ $ show removedCount ++ " mails were removed."
  where
    daylimit = 3
