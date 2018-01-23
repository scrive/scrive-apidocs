module User.Data.Stats
    (
      DocumentStats(..)
    , StatsPartition(..)
    , UserUsageStats(..)
    ) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)

data StatsPartition = PartitionByDay | PartitionByMonth

data DocumentStats = DocumentStats {
    dsDocumentsSent    :: !Int64
  , dsDocumentsClosed  :: !Int64
  , dsSignaturesClosed :: !Int64
  } deriving (Eq, Ord, Show)

instance Monoid DocumentStats where
  mempty = DocumentStats 0 0 0
  ds1 `mappend` ds2 = DocumentStats {
      dsDocumentsSent = dsDocumentsSent ds1 + dsDocumentsSent ds2
    , dsDocumentsClosed = dsDocumentsClosed ds1 + dsDocumentsClosed ds2
    , dsSignaturesClosed = dsSignaturesClosed ds1 + dsSignaturesClosed ds2
    }

data UserUsageStats = UserUsageStats {
    uusTimeWindowStart  :: !UTCTime
  , uusUserEmail        :: !String
  , uusUserName         :: !String
  , uusDocumentStats    :: !DocumentStats
  } deriving (Eq, Ord, Show)

