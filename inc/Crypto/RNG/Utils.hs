module Crypto.RNG.Utils where

import Control.Monad
import Control.Monad.IO.Class

import Crypto.RNG

-- | Generate random string of specified length that contains allowed
-- chars.
randomString :: (MonadIO m, CryptoRNG m) => Int -> [Char] -> m String
randomString n allowed_chars =
  sequence $ replicate n $ ((!!) allowed_chars `liftM` randomR (0, len))
  where
    len = length allowed_chars - 1
