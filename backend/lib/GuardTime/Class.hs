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

instance Default GuardTimeConf where
  def = GuardTimeConf {
        guardTimeSigningServiceURL = "http://internal-gt-signer-848430379.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
      , guardTimeExtendingServiceURL ="http://internal-gt-extender-2081608339.eu-west-1.elb.amazonaws.com:8081/gt-extendingservice"
      , guardTimeControlPublicationsURL = "http://verify.guardtime.com/ksi-publications.bin"
      , guardTimeSigningLoginUser ="anon"
      , guardTimeSigningLoginKey = "anon"
      , guardTimeExtendingLoginUser ="anon"
      , guardTimeExtendingLoginKey = "1234"
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
