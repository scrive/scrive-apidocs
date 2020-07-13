{-# LANGUAGE StrictData #-}
module Flow.Id where

import Data.Aeson
import Data.UUID
import Database.PostgreSQL.PQTypes hiding (JSON)
import GHC.Generics
import Servant.API

import Log.Identifier (Loggable(logDefaultLabel, logValue))

data FlowIdKind
    = InstanceId
    | TemplateId
    | EventId
    | InstanceAccessTokenId

newtype Id (a :: FlowIdKind) = Id UUID
  deriving (Eq, Show, Generic)

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

instance PQFormat (Id a) where
  pqFormat = pqFormat @UUID

instance FromSQL (Id a) where
  type PQBase (Id a) = PQBase UUID
  fromSQL mbase = Id <$> fromSQL mbase

instance ToSQL (Id a) where
  type PQDest (Id a) = PQDest UUID
  toSQL (Id id) = toSQL id

instance Loggable InstanceId where
  logValue = toJSON
  logDefaultLabel _ = "flow_instance_id"
