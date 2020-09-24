{-# LANGUAGE DerivingVia #-}
module Flow.Id where

import Data.Aeson
import Data.UUID
import Database.PostgreSQL.PQTypes hiding (JSON)
import GHC.Generics
import Happstack.Server
import Servant.API
import qualified Data.Text as T

import Log.Identifier (Loggable(logDefaultLabel, logValue))

data FlowIdKind
    = InstanceId
    | TemplateId
    | EventId
    | InstanceAccessTokenId
    | CallbackId

newtype Id (a :: FlowIdKind) = Id UUID
  deriving (Eq, Ord, Generic)
  deriving Show via UUID

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
  toUrlPiece (Id a) = toUrlPiece a

instance FromJSON (Id a) where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON (Id a) where
  toEncoding = genericToEncoding defaultOptions

type InstanceId = Id 'InstanceId
type TemplateId = Id 'TemplateId
type EventId = Id 'EventId
type InstanceAccessTokenId = Id 'InstanceAccessTokenId
type CallbackId = Id 'CallbackId

instance PQFormat (Id a) where
  pqFormat = pqFormat @UUID

instance FromSQL (Id a) where
  type PQBase (Id a) = PQBase UUID
  fromSQL mbase = Id <$> fromSQL mbase

instance ToSQL (Id a) where
  type PQDest (Id a) = PQDest UUID
  toSQL (Id id) = toSQL id

instance FromReqURI (Id a) where
  fromReqURI = fmap Id . maybeRead . T.pack

instance Loggable InstanceId where
  logValue = toJSON
  logDefaultLabel _ = "flow_instance_id"
