{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Random.CryptoRNG where

import Crypto.RNG (Random, random)
import System.Random (StdGen, mkStdGen)
import Test.QuickCheck.Random

instance Random StdGen where
  random = mkStdGen <$> random

instance Random QCGen where
  random = mkQCGen <$> random
