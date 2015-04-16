{-# LANGUAGE UnicodeSyntax #-}
module Control.Monad.Trans.Control.Util where

import Control.Monad.Trans.Control

import KontraPrelude

controlT :: (MonadTransControl t, Monad (t m), Monad m)
         => (Run t -> m (StT t a)) -> t m a
controlT f = liftWith f >>= restoreT . return

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

liftMask :: (Monad (t m), Monad m, MonadTransControl t)
         => (((forall a.   m a ->   m a) -> m (StT t b)) -> m (StT t b))
         -> (((forall a. t m a -> t m a) -> t m b      ) -> t m b      )
liftMask fmask m = controlT $ \run -> fmask $ \release ->
  run $ m $ \f -> restoreT $ release (run f)
