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
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

-- | 'SignatoryLinkID' is an integer that identifies
-- a signatory inside a document scope.
newtype SignatoryLinkID = SignatoryLinkID Int64
  deriving (Eq, Ord)
deriving newtype instance Read SignatoryLinkID
deriving newtype instance Show SignatoryLinkID
deriving newtype instance TextShow SignatoryLinkID

instance PQFormat SignatoryLinkID where
  pqFormat = pqFormat @Int64

instance FromReqURI SignatoryLinkID where
  fromReqURI = maybeRead . T.pack

instance Identifier SignatoryLinkID where
  idDefaultLabel = "signatory_link_id"
  idValue (SignatoryLinkID k) = int64AsStringIdentifier k

instance Unjson SignatoryLinkID where
  unjsonDef = unjsonInvmapR
    (maybe (fail "Can't parse SignatoryLinkID") return . maybeRead . T.pack)
    show
    unjsonDef

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
