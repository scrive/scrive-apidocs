module Utils.List where

import Data.List

import KontraPrelude

mapKeep :: (a -> b) -> [a] -> [(a, b)]
mapKeep f = map (\a -> (a, f a))

mapKeepM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapKeepM f = mapM (\a -> (a, ) `liftM` f a)

rlookup :: Eq b => b -> [(a, b)] -> Maybe a
rlookup v = fmap fst . find ((== v) . snd)
