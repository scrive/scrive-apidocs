module Utils.Monad where


-- | like when but always returns ()
when_ :: Monad m => Bool -> m a -> m ()
when_ b = when b . void

