{-# LANGUAGE UnicodeSyntax #-}
module Control.Monad.Trans.Control.Util where

import Control.Monad
import Control.Monad.Trans.Control

{-# INLINE newtypeLiftBaseWith #-}
newtypeLiftBaseWith ∷ (Monad m, MonadBaseControl b mInner)
                    ⇒ (mInner α → m α)              -- ^ Constructor
                    → (∀ β. m β → mInner β)         -- ^ Deconstructor
                    → (∀ β. StM mInner β → StM m β) -- ^ State constructor
                    → ((RunInBase m b → b α) → m α)
newtypeLiftBaseWith con deCon st = \f → con $ liftBaseWith $ \run →
                                          f $ liftM st . run . deCon

{-# INLINE newtypeRestoreM #-}
newtypeRestoreM ∷ (Monad m, MonadBaseControl b mInner)
                ⇒ (mInner a → m a)         -- ^ Constructor
                → (StM m a → StM mInner a) -- ^ State deconstructor
                → (StM m a → m a)
newtypeRestoreM con unSt = con . restoreM . unSt
