{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StrictData #-}
module Flow.OrphanInstances () where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Time (MonadTime(currentTime))
import Servant.Server (Handler)

-- TODO cache the time when the handler starts
instance MonadTime Handler where
  currentTime = liftIO currentTime
