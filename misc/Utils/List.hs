module Utils.List where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

propagateFst :: (a, [b]) -> [(a, b)]
propagateFst (a, bs) = map (a, ) bs

-- Splits string over some substring
splitOver:: (Eq a) => [a] -> [a] -> [[a]]
splitOver = splitOver' []
  where
    splitOver' [] _ []  = []
    splitOver' c _ []  = [reverse c]
    splitOver' c a b@(bh:bt) =
      if (a `isPrefixOf` b)
        then (reverse c) : (splitOver' [] a (drop (length a) b))
        else splitOver' (bh:c) a bt

-- To be extended
smartZip :: [Maybe a] -> [b] -> [(a, b)]
smartZip ((Just a):as) (b:bs) = (a, b) : (smartZip as bs)
smartZip (_:as) (_:bs) = smartZip as bs
smartZip _ _  = []

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith k ls = sortBy (\a b-> compare (k a) (k b)) ls

groupWith :: Eq b => (a -> b) -> [a] -> [[a]]
groupWith k ls = Data.List.groupBy (\a b -> k a == k b) ls

mapJust :: (a -> Maybe b) -> [a] -> [b]
mapJust f l = catMaybes $ map f l

mapassoc :: (a -> b) -> [a] -> [(a, b)]
mapassoc f = map (id &&& f)

mapassocM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapassocM f = mapM (\a -> return . (a,) =<< f a)

-- | Version of elem that as a value takes Maybe
melem :: Eq a => Maybe a -> [a] -> Bool
melem Nothing   _  = False
melem (Just  e) es = elem e es

firstWithDefault :: Monad m => [m (Maybe a)] -> m a -> m a
firstWithDefault [] da = da
firstWithDefault (ma:mas) da = do
  a <- ma
  case a of
    Just a' -> return a'
    Nothing -> firstWithDefault mas da

findM :: Monad m => (a -> Bool) -> [m a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (a:as) = do
  a' <- a
  if f a'
    then return $ Just a'
    else findM f as

firstOrNothing :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstOrNothing l = join `liftM` findM isJust l

-- | changing an element in a list
chng :: [a] -> Int -> a -> [a]
chng ls i v = let (h, t) = splitAt i ls
              in h ++ [v] ++ (drop 1 t)

listDiff :: Eq a => [a] -> [a] -> ([a], [a], [a])
listDiff a b = ([x|x <- a, x `notElem` b],
                [x|x <- a, x `elem`    b],
                [x|x <- b, x `notElem` a])
