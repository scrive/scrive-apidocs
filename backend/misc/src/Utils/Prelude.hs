module Utils.Prelude where


maybeToBool :: Maybe Bool -> Bool
maybeToBool (Just b) = b
maybeToBool _        = False

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l

