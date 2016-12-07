{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Random.CryptoRNG where

import Crypto.RNG (Random, random)
import System.Random (StdGen, mkStdGen)
import Test.QuickCheck.Random

import KontraPrelude

instance Random StdGen where
  random = mkStdGen `liftM` random

instance Random QCGen where
  random = mkQCGen `liftM` random
