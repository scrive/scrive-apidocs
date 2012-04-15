{-# LANGUAGE UnicodeSyntax #-}
module Control.Monad.Trans.Control.Util where

import Control.Monad
import Control.Monad.Trans.Control

-- | Taken from http://www.mail-archive.com/haskell-cafe@haskell.org/msg95180.html
-- Can be there until these functions make their way to monad-control package.

{-# INLINE defaultLiftWith #-}
defaultLiftWith ∷ (Monad m, MonadTransControl tInner)
                ⇒ (tInner m α → t m α)          -- ^ Constructor
                → (∀ β n. t n β → tInner n β)   -- ^ Deconstructor
                → (∀ β. StT tInner β → StT t β) -- ^ State constructor
                → ((Run t → m α) → t m α)
defaultLiftWith con deCon st = \f → con $ liftWith $ \run →
                                      f $ liftM st . run . deCon

{-# INLINE defaultRestoreT #-}
defaultRestoreT ∷ (Monad m, MonadTransControl tInner)
                ⇒ (tInner m α → t m α)     -- ^ Constructor
                → (StT t α → StT tInner α) -- ^ State deconstructor
                → (m (StT t α) → t m α)
defaultRestoreT con unSt = con . restoreT . liftM unSt

{-# INLINE newtypeLiftBaseWith #-}
newtypeLiftBaseWith ∷ (Monad m, MonadBaseControl b mInner)
                    ⇒ (mInner α → m α)               -- ^ Constructor
                    → (∀ β. m β → mInner β)          -- ^ Deconstructor
                    → (∀ β. StM mInner β -> StM m β) -- ^ State constructor
                    → ((RunInBase m b -> b α) → m α)
newtypeLiftBaseWith con deCon st = \f → con $ liftBaseWith $ \run →
                                          f $ liftM st . run . deCon

{-# INLINE newtypeRestoreM #-}
newtypeRestoreM ∷ (Monad m, MonadBaseControl b mInner)
                ⇒ (mInner a → m a)         -- ^ Constructor
                → (StM m a → StM mInner a) -- ^ State deconstructor
                → (StM m a → m a)
newtypeRestoreM con unSt = con . restoreM . unSt
