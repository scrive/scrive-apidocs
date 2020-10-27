module LoginAuth.LoginAuthMethod
  ( LoginAuthMethod(..)
  , loginAuthMethodFromText
  , loginAuthMethodToText
  ) where

import Control.Monad.Catch (throwM)
import Data.Int

import DB

data LoginAuthMethod =
    LoginAuthNative
  -- ^ Kontrakcja does authentication.
  | LoginAuthSSO
  -- ^ SSO IdP (or other SSO entity) does authentication.
  deriving (Eq, Ord, Show, Read)

instance PQFormat LoginAuthMethod where
  pqFormat = pqFormat @Int16

instance FromSQL LoginAuthMethod where
  type PQBase LoginAuthMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return LoginAuthNative
      2 -> return LoginAuthSSO
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL LoginAuthMethod where
  type PQDest LoginAuthMethod = PQDest Int16
  toSQL LoginAuthNative = toSQL (1 :: Int16)
  toSQL LoginAuthSSO    = toSQL (2 :: Int16)

loginAuthMethodFromText :: Text -> Maybe LoginAuthMethod
loginAuthMethodFromText = \case
  "native" -> Just LoginAuthNative
  "sso"    -> Just LoginAuthSSO
  _        -> Nothing

loginAuthMethodToText :: LoginAuthMethod -> Text
loginAuthMethodToText = \case
  LoginAuthNative -> "native"
  LoginAuthSSO    -> "sso"
