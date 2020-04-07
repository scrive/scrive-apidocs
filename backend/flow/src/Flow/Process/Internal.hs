module Flow.Process.Internal
  ( Process(..)
  ) where

import Data.Aeson
import Database.PostgreSQL.PQTypes

newtype Process = Process Text
  deriving (Eq, FromJSON, Ord, Show, ToJSON)

instance PQFormat Process where
  pqFormat = pqFormat @Text

instance FromSQL Process where
  type PQBase Process = PQBase Text
  fromSQL mbase = Process <$> fromSQL mbase

instance ToSQL Process where
  type PQDest Process = PQDest Text
  toSQL (Process n) = toSQL n
