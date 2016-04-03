module Utils.Prelude where

import KontraPrelude

maybeToBool :: Maybe Bool -> Bool
maybeToBool (Just b) = b
maybeToBool _ = False

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l
