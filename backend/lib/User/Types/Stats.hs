module User.Types.Stats
    (
      DocumentStats(..)
    , StatsPartition(..)
    , UserUsageStats(..)
    ) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.Semigroup as SG

data StatsPartition = PartitionByDay | PartitionByMonth

data DocumentStats = DocumentStats {
    dsDocumentsSent    :: !Int64
  , dsDocumentsClosed  :: !Int64
  , dsSignaturesClosed :: !Int64
  } deriving (Eq, Ord, Show)

instance SG.Semigroup DocumentStats where
  ds1 <> ds2 = DocumentStats {
      dsDocumentsSent = dsDocumentsSent ds1 + dsDocumentsSent ds2
    , dsDocumentsClosed = dsDocumentsClosed ds1 + dsDocumentsClosed ds2
    , dsSignaturesClosed = dsSignaturesClosed ds1 + dsSignaturesClosed ds2
    }

instance Monoid DocumentStats where
  mempty = DocumentStats 0 0 0
  mappend = (SG.<>)

data UserUsageStats = UserUsageStats {
    uusTimeWindowStart  :: !UTCTime
  , uusUserEmail        :: !String
  , uusUserName         :: !String
  , uusDocumentStats    :: !DocumentStats
  } deriving (Eq, Ord, Show)
