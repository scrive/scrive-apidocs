{-# LANGUAGE OverlappingInstances #-}
module GuardTime.Class where

import Control.Monad.Trans
import Data.Unjson

import KontraPrelude

data GuardTimeConf = GuardTimeConf {
  guardTimeSigningServiceURL ::  String
, guardTimeExtendingServiceURL :: String
, guardTimeControlPublicationsURL :: String
, guardTimeSigningLoginUser :: String
, guardTimeSigningLoginKey :: String
, guardTimeExtendingLoginUser :: String
, guardTimeExtendingLoginKey :: String

-- this is old guardtime config to handle pre-KSI GT gateway
, guardTimeOldURL :: String
, guardTimeOldExtendingServiceURL :: String
, guardTimeOldControlPublicationsURL :: String

} deriving (Eq, Ord, Show)

instance Unjson GuardTimeConf where
  unjsonDef = objectOf $ pure GuardTimeConf
    <*> field "signing_service_url"
        guardTimeSigningServiceURL
        "GuardTime Signing Service URL"
    <*> field "extending_service_url"
        guardTimeExtendingServiceURL
        "GuardTime Extendind Service URL"
    <*> field "control_publications_url"
        guardTimeControlPublicationsURL
        "GuardTime Control Publications URL"
    <*> field "signing_login_user"
        guardTimeSigningLoginUser
        "GuardTime login for signing service"
    <*> field "signing_login_key"
        guardTimeSigningLoginKey
        "GuardTime password for signing service"
    <*> field "extending_login_user"
        guardTimeExtendingLoginUser
        "GuardTime login for extending service"
    <*> field "extending_login_key"
        guardTimeExtendingLoginKey
        "GuardTime password for extending service"
    -- this is old guardtime config to handle pre-KSI GT gateway
    <*> field "old_url"
        guardTimeOldURL
        "Old GuardTime URL"
    <*> field "old_extending_service_url"
        guardTimeOldExtendingServiceURL
        "Old GuardTime Extendind Service URL"
    <*> field "old_control_publications_url"
        guardTimeOldControlPublicationsURL
        "Old GuardTime Control Publications URL"

class Monad m => GuardTimeConfMonad m where
  getGuardTimeConf :: m GuardTimeConf

-- | Generic, overlapping instance.
instance (
    GuardTimeConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => GuardTimeConfMonad (t m) where
    getGuardTimeConf = lift getGuardTimeConf
