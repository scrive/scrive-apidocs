module Utils.Monad where

import Control.Applicative
import Control.Monad
import Data.Traversable

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM ma mb mc = do
  b <- ma
  if b
     then mb
     else mc

-- | 'sequenceA' says that if we maybe have @(Maybe (m a))@ a computation
-- that gives a then we can get real computation that may fail m
-- @(Maybe a)@ 'sequenceMM' does the same, but is aware that first
-- computation can also fail, and so it joins two posible fails.
sequenceMM :: (Applicative m) => Maybe (m (Maybe a)) -> m (Maybe a)
sequenceMM = (fmap join) . sequenceA

-- | Applies the first argument to the second argument if the second
-- argument returns Just. Otherwise, returns nothing.
liftMM :: Monad m => (a -> m (Maybe b)) -> m (Maybe a) -> m (Maybe b)
liftMM f v = do
  mv <- v
  maybe (return Nothing) f mv

lift_M :: Monad m => (a -> m b) -> m (Maybe a) -> m (Maybe b)
lift_M f v = do
  mv <- v
  maybe (return Nothing) (liftM Just . f) mv

-- | like when but always returns ()
when_ :: Monad m => Bool -> m a -> m ()
when_ b c = when b $ c >> return ()
