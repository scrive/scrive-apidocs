{-# LANGUAGE OverlappingInstances #-}
module GuardTime.Class where

import Control.Monad.Trans
import Data.Default
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

instance Default GuardTimeConf where
  def = GuardTimeConf {
        guardTimeSigningServiceURL = "http://internal-gt-signer-848430379.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
      , guardTimeExtendingServiceURL ="http://internal-gt-extender-2081608339.eu-west-1.elb.amazonaws.com:8081/gt-extendingservice"
      , guardTimeControlPublicationsURL = "http://verify.guardtime.com/ksi-publications.bin"
      , guardTimeSigningLoginUser ="anon"
      , guardTimeSigningLoginKey = "anon"
      , guardTimeExtendingLoginUser ="anon"
      , guardTimeExtendingLoginKey = "1234"
      , guardTimeOldURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
      , guardTimeOldExtendingServiceURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-extendingservice"
      , guardTimeOldControlPublicationsURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-controlpublications.bin"
      }

class Monad m => GuardTimeConfMonad m where
  getGuardTimeConf :: m GuardTimeConf

-- | Generic, overlapping instance.
instance (
    GuardTimeConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => GuardTimeConfMonad (t m) where
    getGuardTimeConf = lift getGuardTimeConf
