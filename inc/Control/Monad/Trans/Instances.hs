{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverlappingInstances #-}
module Control.Monad.Trans.Instances () where

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
