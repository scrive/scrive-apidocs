{-# LANGUAGE OverlappingInstances #-}
module GuardTime.Class where

import Control.Monad.Trans
import Data.Unjson

import KontraPrelude

data GuardTimeConf = GuardTimeConf {
  guardTimeURL ::  String
, guardTimeExtendingServiceURL :: String
, guardTimeControlPublicationsURL :: String
} deriving (Eq, Ord, Show)

instance Unjson GuardTimeConf where
  unjsonDef = objectOf $ pure GuardTimeConf
    <*> field "url"
        guardTimeURL
        "GuardTime URL"
    <*> field "extending_service_url"
        guardTimeExtendingServiceURL
        "GuardTime Extendind Service URL"
    <*> field "control_publications_url"
        guardTimeControlPublicationsURL
        "GuardTime Control Publications URL"

class Monad m => GuardTimeConfMonad m where
  getGuardTimeConf :: m GuardTimeConf

-- | Generic, overlapping instance.
instance (
    GuardTimeConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => GuardTimeConfMonad (t m) where
    getGuardTimeConf = lift getGuardTimeConf
