{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.Instances where

import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Happstack.Server

import Control.Monad.Trans.Control.Util

instance NFData HeaderPair where
  rnf HeaderPair {..} = rnf hName `seq` rnf hValue

instance NFData Length where
  rnf = (`seq` ())

instance NFData RsFlags where
  rnf RsFlags {..} = rnf rsfLength

instance NFData Response where
  rnf Response {..} =
    rnf rsCode
      `seq` rnf rsHeaders
      `seq` rnf rsFlags
      `seq` rnf rsBody
      `seq` rnf rsValidator
  rnf SendFile {..} =
    rnf rsCode
      `seq` rnf rsHeaders
      `seq` rnf rsFlags
      `seq` rnf rsValidator
      `seq` rnf sfFilePath
      `seq` rnf sfOffset
      `seq` rnf sfCount

----------------------------------------

instance {-# OVERLAPPABLE #-} (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , FilterMonad f m
  ) => FilterMonad f (t m) where
  setFilter     = lift . setFilter
  composeFilter = lift . composeFilter
  getFilter m = do
    (stT, f) <- liftWith $ \run -> getFilter (run m)
    (, f) <$> restoreT (return stT)

instance {-# OVERLAPPABLE #-} (
    Monad m
  , Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , HasRqData m
  ) => HasRqData (t m) where
  askRqEnv = lift askRqEnv
  localRqEnv f m = controlT $ \run -> localRqEnv f (run m)
  rqDataError = lift . rqDataError

instance {-# OVERLAPPABLE #-} (
    Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , ServerMonad m
  ) => ServerMonad (t m) where
  askRq = lift askRq
  localRq f m = controlT $ \run -> localRq f (run m)

instance {-# OVERLAPPABLE #-} (
    Monad (t m)
  , MonadTrans t
  , WebMonad r m
  ) => WebMonad r (t m) where
  finishWith = lift . finishWith
