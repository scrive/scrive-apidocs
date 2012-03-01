{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Random.CryptoRNG where

import Crypto.RNG (Random, random)

import Control.Monad(liftM)
import System.Random (StdGen, mkStdGen)

instance Random StdGen where
  random = mkStdGen `liftM` random
