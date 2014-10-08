{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
module Control.Monad.Trans.Instances () where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Util
import Text.StringTemplates.Templates

instance (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , MonadReader r m
  ) => MonadReader r (t m) where
    ask       = lift ask
    local f m = controlT $ \run -> local f (run m)

instance (
    Monad (t m)
  , MonadTrans t
  , MonadState s m
  ) => MonadState s (t m) where
    get = lift get
    put = lift . put

instance (
    Functor (t m)
  , Monad (t m)
  , MonadTrans t
  , TemplatesMonad m
  ) => TemplatesMonad (t m) where
    getTemplates = lift getTemplates
    getTextTemplatesByLanguage = lift . getTextTemplatesByLanguage

instance (
    Monad (t m)
  , MonadTrans t
  , MonadThrow m
  ) => MonadThrow (t m) where
    throwM = lift . throwM

instance (
    Monad (t m)
  , MonadTransControl t
  , MonadCatch m
  ) => MonadCatch (t m) where
    catch m h = controlT $ \run -> run m `catch` (run . h)

instance (
    Monad (t m)
  , MonadTransControl t
  , MonadMask m
  ) => MonadMask (t m) where
    mask = liftMask mask
    uninterruptibleMask = liftMask uninterruptibleMask

liftMask :: (Monad (t m), Monad m, MonadTransControl t)
         => (((forall a.   m a ->   m a) -> m (StT t b)) -> m (StT t b))
         -> (((forall a. t m a -> t m a) -> t m b      ) -> t m b      )
liftMask fmask m = controlT $ \run -> fmask $ \release ->
  run $ m $ \f -> restoreT $ release (run f)
