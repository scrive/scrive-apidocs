{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.Instances where

import Control.Monad.Trans
import Database.PostgreSQL.PQTypes
import Happstack.Server

instance FilterMonad r m => FilterMonad r (DBT m) where
  setFilter f   = lift $ setFilter f
  composeFilter = lift . composeFilter
  getFilter     = mapDBT $ \m -> do
    ((b, s), f) <- getFilter m
    return ((b, f), s)

instance (Monad m, HasRqData m) => HasRqData (DBT m) where
  askRqEnv    = lift askRqEnv
  localRqEnv  = mapDBT . localRqEnv
  rqDataError = lift . rqDataError

instance ServerMonad m => ServerMonad (DBT m) where
  askRq   = lift askRq
  localRq = mapDBT . localRq

instance WebMonad r m => WebMonad r (DBT m) where
  finishWith = lift . finishWith
