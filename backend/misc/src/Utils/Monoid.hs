module Utils.Monoid where


-- | Pack value to just unless we have 'mzero'. Since we can't check
-- emptyness of string in templates we want to pack it in maybe.
emptyToNothing :: (Eq a, Monoid a) => a -> Maybe a
emptyToNothing a | a == mempty = Nothing
                 | otherwise   = Just a

justEmptyToNothing :: (Eq a, Monoid a) => Maybe a -> Maybe a
justEmptyToNothing = maybe Nothing emptyToNothing

optional :: MonadPlus m => m a -> m (Maybe a)
optional c = fmap Just c `mplus` return Nothing

