module Flow.Core.Type.Url
    ( Url(..)
    )
  where

import Data.Aeson
import Database.PostgreSQL.PQTypes

newtype Url = Url { fromUrl :: Text }
  deriving newtype (Eq, FromJSON, Ord, Show, ToJSON, ToSQL, FromSQL, PQFormat)
