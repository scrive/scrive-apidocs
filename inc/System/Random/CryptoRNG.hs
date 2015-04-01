{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Random.CryptoRNG where

import System.Random (StdGen, mkStdGen)

import Crypto.RNG (Random, random)
import KontraPrelude

instance Random StdGen where
  random = mkStdGen `liftM` random
