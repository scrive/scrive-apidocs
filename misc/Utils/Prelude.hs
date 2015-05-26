module Utils.Prelude where

import KontraPrelude

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

maybeToBool :: Maybe Bool -> Bool
maybeToBool (Just b) = b
maybeToBool _ = False

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l
