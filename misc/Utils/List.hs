module Utils.List where

import Data.List

import KontraPrelude

-- Splits string over some substring
splitOver :: Eq a => [a] -> [a] -> [[a]]
splitOver = splitOver' []
  where
    splitOver' [] _ []  = []
    splitOver' c _ []  = [reverse c]
    splitOver' c a b@(bh:bt) =
      if (a `isPrefixOf` b)
        then (reverse c) : (splitOver' [] a (drop (length a) b))
        else splitOver' (bh:c) a bt

mapKeep :: (a -> b) -> [a] -> [(a, b)]
mapKeep f = map (\a -> (a, f a))

mapKeepM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapKeepM f = mapM (\a -> (a, ) `liftM` f a)

rlookup :: Eq b => b -> [(a, b)] -> Maybe a
rlookup v = fmap fst . find ((== v) . snd)
