module Utils.Tuples where

import Control.Arrow

mapFst :: Functor f => (a -> c) -> f (a, b)  -> f (c, b)
mapFst = fmap . first

mapSnd :: Functor f => (b -> c)  -> f (a, b) -> f (a, c)
mapSnd = fmap . second

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just a) (Just b) = Just (a, b)
pairMaybe _ _ = Nothing

pairMaybe3 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
pairMaybe3 (Just a) (Just b) (Just c) = Just (a, b, c)
pairMaybe3 _ _ _ = Nothing

-- | Triples
fst3 :: (t1, t2, t3) -> t1
fst3 (a,_,_) = a

snd3 :: (t1, t2, t3) -> t2
snd3 (_,b,_) = b

thd3 :: (t1, t2, t3) -> t3
thd3 (_,_,c) = c
