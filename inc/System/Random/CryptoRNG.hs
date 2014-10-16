{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Random.CryptoRNG where

import Control.Monad(liftM)
import System.Random (StdGen, mkStdGen)

import Crypto.RNG (Random, random)

instance Random StdGen where
  random = mkStdGen `liftM` random
