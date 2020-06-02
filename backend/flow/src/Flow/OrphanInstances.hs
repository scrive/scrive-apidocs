{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Strict #-}

module Flow.OrphanInstances () where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Time (MonadTime(currentTime))
import Servant.Server (Handler)

instance MonadTime Handler where
  currentTime = liftIO currentTime
