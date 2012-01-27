

-- | Support for generation of cryptographically secure random
-- numbers, based on the DRBG package.  It maintains a global variable
-- that is initialized using Crypto.Random.newGenIO to obtain
-- sufficient entropy.

module Crypto.GlobalRandom
  ( genBytesIO
  , Random(..)
  , boundedIntegralGenIO
  ) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (GenAutoReseed, HashDRBG)
import Crypto.Types (ByteLength)
import Data.Bits((.|.), shiftL)
import Data.ByteString (ByteString, unpack)
import Data.Int(Int64)
import System.IO.Unsafe (unsafePerformIO)

-- | The global random number generator.  The reason for using a
-- global variable is to facilitate transition from the use of
-- 'System.Random.randomIO' and friends, which use global state.
globalGen :: MVar (GenAutoReseed HashDRBG HashDRBG)
globalGen = unsafePerformIO $ newGenIO >>= newMVar

-- | Generate a number of cryptographically secure random bytes
genBytesIO :: ByteLength -- ^ number of bytes to generate
           -> IO ByteString
genBytesIO l = modifyMVar globalGen $ \g -> do
  (bs, g') <- either (fail "Crypto.GlobalRandom.genBytes") return $
              Crypto.Random.genBytes l g
  return (g', bs)

-- | Class for generating cryptographically secure random values
class Random a where
  genIO :: IO a

-- | Helper function for making Random instances
boundedIntegralGenIO :: forall a . (Integral a, Bounded a) => IO a
boundedIntegralGenIO = do
    bs <- genBytesIO byteLen
    return $
      fromIntegral $
        minb + foldl1 (\r a -> shiftL r 8 .|. a) (map toInteger (unpack bs))
                `mod` range
    where
      minb, maxb, range :: Integer
      minb = fromIntegral (minBound :: a)
      maxb = fromIntegral (maxBound :: a)
      range = maxb - minb + 1
      byteLen = ceiling $ logBase 2 (fromIntegral range) / (8 :: Double)

instance Random Int64 where
  genIO = boundedIntegralGenIO