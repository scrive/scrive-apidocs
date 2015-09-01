-- | This module contains definition of 'SignatoryLinkID' and
-- associated class implementations.  It is meant to be short module
-- just for this datatype so it can be included when nothing else is
-- needed.
module Doc.SignatoryLinkID (
    SignatoryLinkID
  , unsafeSignatoryLinkID
  ) where

import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server

import DB.Derive
import KontraPrelude
import Log.Identifier

-- | 'SignatoryLinkID' is and integer that identifies
-- a signatory inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID Int64
  deriving (Eq, Ord, PQFormat, Data, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)

instance FromReqURI SignatoryLinkID where
  fromReqURI = maybeRead

instance Identifier SignatoryLinkID Int64 where
  gidentifier f n = f "signatory_link_id" .= fmap (\(SignatoryLinkID k) -> k) n

instance FromSQL SignatoryLinkID where
  type PQBase SignatoryLinkID = PQBase Int64
  fromSQL mbase = SignatoryLinkID <$> fromSQL mbase

instance ToSQL SignatoryLinkID where
  type PQDest SignatoryLinkID = PQDest Int64
  toSQL (SignatoryLinkID n) = toSQL n

unsafeSignatoryLinkID :: Int64 -> SignatoryLinkID
unsafeSignatoryLinkID = SignatoryLinkID
