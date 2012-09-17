-- | Support for generation of cryptographically secure random
-- numbers, based on the DRBG package.
--
-- This is a convenience layer on top of DRBG, which allows you to
-- pull random values by means of the method 'random', while keeping
-- the state of the random number generator (RNG) inside a monad.  The
-- state is protected by an MVar, which means that concurrent
-- generation of random values from several threads works straight out
-- of the box.
--
-- The access to the RNG state is captured by a class.  By making
-- instances of this class, client code can enjoy RNG generation from
-- their own monads.

module Crypto.RNG (
  -- * Generation of strings and numbers
    CryptoRNGState
  , newCryptoRNGState
  , unsafeCryptoRNGState
  , CryptoRNG(..)
  , randomBytes
  , randomR
  -- * Generation of values in other types
  , Random(..)
  , boundedIntegralRandom
  -- * Monad transformer for carrying rng state
  , CryptoRNGT
  , mapCryptoRNGT
  , runCryptoRNGT
  , withCryptoRNGState
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Crypto.Random
import Crypto.Random.DRBG
import Crypto.Types
import Data.Bits
import Data.ByteString (ByteString, unpack)
import Data.Int
import Data.List
import Happstack.Server
import qualified Control.Monad.State.Lazy as LS
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy as LW
import qualified Control.Monad.Writer.Strict as SW

import Control.Monad.Trans.Control.Util

-- | The random number generator state.  It sits inside an MVar to
-- support concurrent thread access.
newtype CryptoRNGState = CryptoRNGState (MVar (GenAutoReseed HashDRBG HashDRBG))

-- | Create a new 'CryptoRNGState', based on system entropy.
newCryptoRNGState :: MonadIO m => m CryptoRNGState
newCryptoRNGState = liftIO $ newGenIO >>= fmap CryptoRNGState . newMVar

-- | Create a new 'CryptoRNGState', based on a bytestring seed.
-- Should only be used for testing.
unsafeCryptoRNGState :: MonadIO m => ByteString -> m CryptoRNGState
unsafeCryptoRNGState s = liftIO $
  either (fail . show) (fmap CryptoRNGState . newMVar) (newGen s)

-- | Generate given number of cryptographically secure random bytes.
randomBytes :: CryptoRNG m
            => ByteLength -- ^ number of bytes to generate
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
  return . fromIntegral $
    minb + foldl1' (\r a -> shiftL r 8 .|. a) (map toInteger (unpack bs))
            `mod` range
    where
      minb, maxb, range :: Integer
      minb = fromIntegral minb'
      maxb = fromIntegral maxb'
      range = maxb - minb + 1
      byteLen = ceiling $ logBase 2 (fromIntegral range) / (8 :: Double)

-- | Helper function for making Random instances.
boundedIntegralRandom :: forall m a. (CryptoRNG m, Integral a, Bounded a) => m a
boundedIntegralRandom = randomR (minBound :: a, maxBound :: a)

-- | Class for generating cryptographically secure random values.
class Random a where
  random :: CryptoRNG m => m a

instance Random Int64 where
  random = boundedIntegralRandom

instance Random Int where
  random = boundedIntegralRandom

type InnerCryptoRNGT = ReaderT CryptoRNGState

-- | Monad transformer with RNG state.
newtype CryptoRNGT m a = CryptoRNGT { unCryptoRNGT :: InnerCryptoRNGT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadPlus, MonadTrans)

mapCryptoRNGT :: (m a -> n b) -> CryptoRNGT m a -> CryptoRNGT n b
mapCryptoRNGT f m = withCryptoRNGState $ \s -> f (runCryptoRNGT s m)

runCryptoRNGT :: CryptoRNGState -> CryptoRNGT m a -> m a
runCryptoRNGT gv m = runReaderT (unCryptoRNGT m) gv

withCryptoRNGState :: (CryptoRNGState -> m a) -> CryptoRNGT m a
withCryptoRNGState = CryptoRNGT . ReaderT

instance MonadTransControl CryptoRNGT where
  newtype StT CryptoRNGT a = StCryptoRNGT { unStCryptoRNGT :: StT InnerCryptoRNGT a }
  liftWith = defaultLiftWith CryptoRNGT unCryptoRNGT StCryptoRNGT
  restoreT = defaultRestoreT CryptoRNGT unStCryptoRNGT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (CryptoRNGT m) where
  newtype StM (CryptoRNGT m) a = StMCryptoRNGT { unStMCryptoRNGT :: ComposeSt CryptoRNGT m a }
  liftBaseWith = defaultLiftBaseWith StMCryptoRNGT
  restoreM     = defaultRestoreM unStMCryptoRNGT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadError e m => MonadError e (CryptoRNGT m) where
  throwError     = lift . throwError
  catchError m h = withCryptoRNGState $ \s -> catchError (runCryptoRNGT s m) (runCryptoRNGT s . h)

instance MonadReader r m => MonadReader r (CryptoRNGT m) where
  ask     = lift ask
  local f = mapCryptoRNGT $ local f

instance MonadState s m => MonadState s (CryptoRNGT m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (CryptoRNGT m) where
  tell   = lift . tell
  listen = mapCryptoRNGT listen
  pass   = mapCryptoRNGT pass

-- Happstack specific instances, to be moved somewhere else

instance FilterMonad f m => FilterMonad f (CryptoRNGT m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter     = mapCryptoRNGT getFilter

instance (HasRqData m, Monad m) => HasRqData (CryptoRNGT m) where
  askRqEnv     = lift askRqEnv
  localRqEnv f = mapCryptoRNGT $ localRqEnv f
  rqDataError  = lift . rqDataError

instance ServerMonad m => ServerMonad (CryptoRNGT m) where
  askRq     = lift askRq
  localRq f = mapCryptoRNGT $ localRq f

instance WebMonad r m => WebMonad r (CryptoRNGT m) where
  finishWith = lift . finishWith

-- | Monads carrying around the RNG state.
class MonadIO m => CryptoRNG m where
  getCryptoRNGState :: m CryptoRNGState

instance MonadIO m => CryptoRNG (CryptoRNGT m) where
  getCryptoRNGState = CryptoRNGT ask

instance CryptoRNG m => CryptoRNG (ReaderT r m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (ContT r m) where
  getCryptoRNGState = lift getCryptoRNGState

instance (Error e, CryptoRNG m) => CryptoRNG (ErrorT e m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (IdentityT m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (ListT m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (MaybeT m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (SS.StateT s m) where
  getCryptoRNGState = lift getCryptoRNGState

instance CryptoRNG m => CryptoRNG (LS.StateT s m) where
  getCryptoRNGState = lift getCryptoRNGState

instance (CryptoRNG m, Monoid w) => CryptoRNG (LW.WriterT w m) where
  getCryptoRNGState = lift getCryptoRNGState

instance (CryptoRNG m, Monoid w) => CryptoRNG (SW.WriterT w m) where
  getCryptoRNGState = lift getCryptoRNGState
