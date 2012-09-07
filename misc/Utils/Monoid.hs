module Utils.Monoid where

import Control.Monad
import Data.Monoid

-- intercalate for monoids
mintercalate :: Monoid s => (s -> s -> s) -> [s] -> s
mintercalate f = go
  where
    go []     = mempty
    go (s:[]) = s
    go (s:ss) = s `f` go ss

-- | Pack value to just unless we have 'mzero'.  Since we can not check
-- emptyness of string in templates we want to pack it in maybe.
nothingIfEmpty :: (Eq a, Monoid a) => a -> Maybe a
nothingIfEmpty a = if mempty == a then Nothing else Just a

-- | Failing if inner value is empty
joinEmpty :: (MonadPlus m, Monoid a, Ord a) => m a -> m a
joinEmpty m = do
  mv <- m
  if mv == mempty
    then mzero
    else return mv

optional :: MonadPlus m => m a -> m (Maybe a)
optional c = (liftM Just c) `mplus` (return Nothing)
