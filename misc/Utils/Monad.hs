module Utils.Monad where

import Control.Monad

-- | like when but always returns ()
when_ :: Monad m => Bool -> m a -> m ()
when_ b c = when b $ c >> return ()
