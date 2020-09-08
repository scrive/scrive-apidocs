module Flow.Core.Type.Url
    ( Url(..)
    )
  where

import Data.Aeson
import Database.PostgreSQL.PQTypes

newtype Url = Url { fromUrl :: Text }
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON)

instance PQFormat Url where
  pqFormat = pqFormat @Text

instance FromSQL Url where
  type PQBase Url = PQBase Text
  fromSQL mbase = Url <$> fromSQL mbase

instance ToSQL Url where
  type PQDest Url = PQDest Text
  toSQL (Url url) = toSQL url
