{-# OPTIONS_GHC -fno-warn-orphans #-}
{- | Certain Happstack functions use MonadPlus(mzero) to indicate that
the invoking handler should fail and trigger backtracking.  We want to
avoid backtracking and MonadPlus, and MPlusT allows us to interface
with Happstack functions requiring MonadPlus.-}

module Happstack.MonadPlus
  ( MPlusT(..)
  , runMPlusT
  )  where

import Control.Applicative
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Happstack.Server (FilterMonad(..), WebMonad(..), ServerMonad(..), HasRqData(..))

-- | Capture 'MonadPlus' contexts using 'MaybeT'.  Useful as a wrapper
-- around backtracking Happstack functions.
newtype MPlusT m a = MPlusT { unMPlusT :: MaybeT m a }
  deriving (Applicative, Functor, Monad)

runMPlusT :: MPlusT m a -> m (Maybe a)
runMPlusT = runMaybeT . unMPlusT

instance MonadTrans MPlusT where
  lift m = MPlusT $ lift m

instance (Functor m, Monad m) => Alternative (MPlusT m) where
  empty = MPlusT empty
  MPlusT a <|> MPlusT b = MPlusT $ a <|> b

instance Monad m => MonadPlus (MPlusT m) where
  mzero = MPlusT $ mzero
  mplus (MPlusT a) (MPlusT b) = MPlusT $ mplus a b

instance FilterMonad r m => FilterMonad r (MaybeT m) where
  setFilter f = lift $ setFilter f
  composeFilter f = lift $ composeFilter f
  getFilter (MaybeT m) = MaybeT $ m >>= maybe (return Nothing) (liftM Just . getFilter . return)


instance FilterMonad r m => FilterMonad r (MPlusT m) where
  setFilter f = lift $ setFilter f
  composeFilter f = lift $ composeFilter f
  getFilter (MPlusT m) = MPlusT $ getFilter m

instance WebMonad r m => WebMonad r (MaybeT m) where
  finishWith a = lift $ finishWith a

instance WebMonad r m => WebMonad r (MPlusT m) where
  finishWith a = lift $ finishWith a

instance ServerMonad m => ServerMonad (MPlusT m) where
  askRq = lift askRq
  localRq f (MPlusT m) = MPlusT $ localRq f m

instance ServerMonad m => ServerMonad (MaybeT m) where
  askRq = lift askRq
  localRq f (MaybeT m) = MaybeT $ localRq f m


instance MonadIO m => MonadIO (MPlusT m) where
  liftIO m = MPlusT $ liftIO m

instance (Monad m, HasRqData m) => HasRqData (MPlusT m) where
    askRqEnv = lift askRqEnv
    localRqEnv f m = MPlusT $ localRqEnv f $ unMPlusT m
    rqDataError = MPlusT . rqDataError

instance (Monad m, HasRqData m) => HasRqData (MaybeT m) where
    askRqEnv = lift askRqEnv
    localRqEnv f m = MaybeT $ localRqEnv f $ runMaybeT m
    rqDataError _ = MaybeT $ return Nothing
