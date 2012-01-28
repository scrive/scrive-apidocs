

-- | Support for generation of cryptographically secure random
-- numbers, based on the DRBG package.

module Crypto.RNG
  ( -- * Generation of strings and numbers
    CryptoRNGState
  , newCryptoRNGState
  , CryptoRNG(..)
  , randomBytes
  , randomR
    -- * Generation using explicit RNG state
  , CryptoRNGIO
  , inIO
    -- * Generation of values in other types
  , Random(..)
  , boundedIntegralRandom
  ) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad.Trans(MonadIO, liftIO, lift)
import Control.Monad.Reader(ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State(StateT)
import Control.Monad.Error(ErrorT, Error)
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (GenAutoReseed, HashDRBG)
import Crypto.Types (ByteLength)
import Data.Bits((.|.), shiftL)
import Data.ByteString (ByteString, unpack)
import Data.Int(Int64)

-- | The random number generator state.  It sits inside an MVar to
-- support concurrent thread access.
newtype CryptoRNGState = CryptoRNGState (MVar (GenAutoReseed HashDRBG HashDRBG))

-- | Create a new 'CryptoRNGState', based on system entropy.
newCryptoRNGState :: MonadIO m => m CryptoRNGState
newCryptoRNGState = liftIO $ newGenIO >>= fmap CryptoRNGState . newMVar

-- | Generate given number of cryptographically secure random bytes.
randomBytes :: CryptoRNG m =>
               ByteLength -- ^ number of bytes to generate
            -> m ByteString
randomBytes l = do
  CryptoRNGState gv <- getCryptoRNGState
  liftIO $ modifyMVar gv $ \g -> do
    (bs, g') <- either (fail "Crypto.GlobalRandom.genBytes") return $
                genBytes l g
    return (g', bs)

-- | Generate a cryptographically secure random number in given,
-- closed range.
randomR :: (CryptoRNG m, Integral a) => (a, a) -> m a
randomR (minb', maxb') = do
    bs <- randomBytes byteLen
    return $
      fromIntegral $
        minb + foldl1 (\r a -> shiftL r 8 .|. a) (map toInteger (unpack bs))
                `mod` range
    where
      minb, maxb, range :: Integer
      minb = fromIntegral minb'
      maxb = fromIntegral maxb'
      range = maxb - minb + 1
      byteLen = ceiling $ logBase 2 (fromIntegral range) / (8 :: Double)

-- | Monads carrying around the RNG state.
class MonadIO m => CryptoRNG m where
  getCryptoRNGState :: m CryptoRNGState

instance CryptoRNG m => CryptoRNG (ReaderT c m) where
 getCryptoRNGState = lift $ getCryptoRNGState

instance (Error e, CryptoRNG m) => CryptoRNG (ErrorT e m) where
 getCryptoRNGState = lift $ getCryptoRNGState

instance CryptoRNG m => CryptoRNG (StateT s m) where
 getCryptoRNGState = lift $ getCryptoRNGState

-- | Monad transformer with RNG state.
newtype CryptoRNGIO m a = CryptoRNGIO (ReaderT CryptoRNGState m a)
  deriving (Monad, MonadReader CryptoRNGState, MonadIO)

instance (Monad m, MonadIO (CryptoRNGIO m)) => CryptoRNG (CryptoRNGIO m) where
  getCryptoRNGState = ask

-- | Run crypto operations with explicitly parameterized RNG state in
-- an IO-like monad.
inIO :: MonadIO m => CryptoRNGState -> CryptoRNGIO m a -> m a
inIO gv (CryptoRNGIO m) = runReaderT m gv

-- | Class for generating cryptographically secure random values.
class Random a where
  random :: CryptoRNG m => m a

-- | Helper function for making Random instances.
boundedIntegralRandom :: forall m a . (CryptoRNG m, Integral a, Bounded a)
                      => m a
boundedIntegralRandom = randomR (minBound :: a, maxBound :: a)

instance Random Int64 where
  random = boundedIntegralRandom
