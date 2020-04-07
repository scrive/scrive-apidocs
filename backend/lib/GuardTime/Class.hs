module GuardTime.Class where

import Control.Monad.Trans
import Data.Unjson

data GuardTimeConf = GuardTimeConf
  { guardTimeSigningServiceURL ::  String
  , guardTimeExtendingServiceURL :: String
  , guardTimeControlPublicationsURL :: String
  , guardTimeSigningLoginUser :: String
  , guardTimeSigningLoginKey :: String
  , guardTimeExtendingLoginUser :: String
  , guardTimeExtendingLoginKey :: String
  } deriving (Eq, Ord, Show)

instance Unjson GuardTimeConf where
  unjsonDef =
    objectOf
      $   GuardTimeConf
      <$> field "signing_service_url"
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

class Monad m => GuardTimeConfMonad m where
  getGuardTimeConf :: m GuardTimeConf

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-} (
    GuardTimeConfMonad m
  , Monad (t m)
  , MonadTrans t
  ) => GuardTimeConfMonad (t m) where
  getGuardTimeConf = lift getGuardTimeConf
