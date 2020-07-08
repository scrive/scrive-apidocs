{-# LANGUAGE StrictData #-}

module Flow.Message.Internal
  ( Message(..)
  ) where

import Data.Aeson
import Database.PostgreSQL.PQTypes

newtype Message = Message Text
  deriving (Eq, FromJSON, Ord, Show, ToJSON)

instance PQFormat Message where
  pqFormat = pqFormat @Text

instance FromSQL Message where
  type PQBase Message = PQBase Text
  fromSQL mbase = Message <$> fromSQL mbase

instance ToSQL Message where
  type PQDest Message = PQDest Text
  toSQL (Message n) = toSQL n
