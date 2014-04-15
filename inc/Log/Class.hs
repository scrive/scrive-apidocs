{-# LANGUAGE OverlappingInstances #-}
module Log.Class where

import Control.Monad.Trans
import Text.JSON.Gen

-- | MonadLog is for situations when you want to have access to
-- logging, but to not expose whole IO functionality. It is a safe
-- entry to a restricted IO monad.
--
-- Should be used together with other IO based monads that do not
-- expose MonadIO or MonadBase IO.
class Monad m => MonadLog m where
  -- | This is a variation on 'mixlog' that takes a premade version of
  -- properties object. Useful for logging data directly from API calls
  -- for example.
  mixlogjs :: ToJSValue js => String -> js -> m ()

-- | Generic, overlapping instance.
instance (
    MonadLog m
  , Monad (t m)
  , MonadTrans t
  ) => MonadLog (t m) where
    mixlogjs title js = lift (mixlogjs title js)
