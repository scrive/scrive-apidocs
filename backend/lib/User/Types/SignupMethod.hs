module User.Types.SignupMethod where

import Data.Int (Int16)
import Happstack.Server (FromReqURI(..))
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

import DB

{-# DEPRECATED BySigning "BySigning is not used anymore" #-}
{- BySigning is not used anymore. We can't drop it right away, but it doesn't need to be supported -}
data SignupMethod = AccountRequest | ViralInvitation | BySigning | ByAdmin | CompanyInvitation | PartnerInvitation | PortalInvite
  deriving (Eq, Ord, Show, Read)

instance PQFormat SignupMethod where
  pqFormat = pqFormat @Int16

instance FromSQL SignupMethod where
  type PQBase SignupMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return AccountRequest
      2 -> return ViralInvitation
      3 -> return BySigning
      4 -> return ByAdmin
      5 -> return CompanyInvitation
      6 -> return PartnerInvitation
      7 -> return PortalInvite
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 7)]
      , reValue = n
      }

instance ToSQL SignupMethod where
  type PQDest SignupMethod = PQDest Int16
  toSQL AccountRequest    = toSQL (1::Int16)
  toSQL ViralInvitation   = toSQL (2::Int16)
  toSQL BySigning         = toSQL (3::Int16)
  toSQL ByAdmin           = toSQL (4::Int16)
  toSQL CompanyInvitation = toSQL (5::Int16)
  toSQL PartnerInvitation = toSQL (6::Int16)
  toSQL PortalInvite = toSQL (7::Int16)

instance FromReqURI SignupMethod where
  fromReqURI = maybeRead . T.pack
