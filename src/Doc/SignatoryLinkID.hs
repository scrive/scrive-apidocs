-- | This module contains definition of 'SignatoryLinkID' and
-- associated class implementations.  It is meant to be short module
-- just for this datatype so it can be included when nothing else is
-- needed.
module Doc.SignatoryLinkID (
    SignatoryLinkID
  , unsafeSignatoryLinkID
  , fromSignatoryLinkID
  ) where

import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Data.Unjson

import DB.Derive
import KontraPrelude
import Log.Identifier

-- | 'SignatoryLinkID' is an integer that identifies
-- a signatory inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)

instance FromReqURI SignatoryLinkID where
  fromReqURI = maybeRead

instance Identifier SignatoryLinkID Int64 where
  gidentifier f n = f "signatory_link_id" .= fmap (\(SignatoryLinkID k) -> k) n

instance Unjson SignatoryLinkID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse SignatoryLinkID")  return) . maybeRead) show unjsonDef

instance FromSQL SignatoryLinkID where
  type PQBase SignatoryLinkID = PQBase Int64
  fromSQL mbase = SignatoryLinkID <$> fromSQL mbase

instance ToSQL SignatoryLinkID where
  type PQDest SignatoryLinkID = PQDest Int64
  toSQL (SignatoryLinkID n) = toSQL n

unsafeSignatoryLinkID :: Int64 -> SignatoryLinkID
unsafeSignatoryLinkID = SignatoryLinkID


fromSignatoryLinkID :: SignatoryLinkID -> Int64
fromSignatoryLinkID (SignatoryLinkID did) = did
