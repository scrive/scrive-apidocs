module Utils.Prelude where

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

joinB :: Maybe Bool -> Bool
joinB (Just b) = b
joinB _ = False

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l

maybeM :: Monad m => (a -> m b) -> Maybe a -> m ()
maybeM _ Nothing = return ()
maybeM m (Just a) = m a >> return ()

($^) :: Maybe (a -> a) -> a -> a
($^) Nothing  a = a
($^) (Just f) a = f a
infixr 9 $^

($^^) :: [a -> a] -> a -> a
($^^) []  a = a
($^^) (f : fs) a = fs $^^ f a
infixr 9 $^^

-- | Select first alternative from a list of options.
--
-- Remeber LISP and its cond!
caseOf :: [(Bool, t)] -> t -> t
caseOf ((True,a):_) _ = a
caseOf (_:r) d = caseOf r d
caseOf [] d = d
