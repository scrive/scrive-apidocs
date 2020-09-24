module Flow.Names.Internal
  ( Name(..)
  , DocumentName
  , UserName
  , MessageName
  , FieldName
  , StageName
  ) where

import Data.Aeson
import Database.PostgreSQL.PQTypes
import Servant.API
import qualified Data.Text as T

data NameKind
    = DocumentName
    | UserName
    | MessageName
    | FieldName
    | StageName

newtype Name (a :: NameKind) = Name Text
  deriving (Eq, FromJSON, FromJSONKey, Ord, ToJSON, ToJSONKey)

instance Show (Name a) where
  show (Name t) = T.unpack t

instance FromHttpApiData (Name a) where
  parseUrlPiece = fmap Name . parseUrlPiece

instance ToHttpApiData (Name a) where
  toUrlPiece (Name a) = toUrlPiece a

instance PQFormat (Name a) where
  pqFormat = pqFormat @Text

instance FromSQL (Name a) where
  type PQBase (Name a) = PQBase Text
  fromSQL mbase = Name <$> fromSQL mbase

instance ToSQL (Name a) where
  type PQDest (Name a) = PQDest Text
  toSQL (Name n) = toSQL n

type DocumentName = Name 'DocumentName
type UserName = Name 'UserName
type MessageName = Name 'MessageName
type FieldName = Name 'FieldName
type StageName = Name 'StageName
