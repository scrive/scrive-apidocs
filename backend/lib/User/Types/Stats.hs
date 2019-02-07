module User.Types.Stats
    (
      DocumentStats(..)
    , TemplateStats(..)
    , StatsPartition(..)
    , UserUsageStats(..)
    , ShareableLinkUsageStats(..)
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


data TemplateStats = TemplateStats {
    tsDocumentsSent    :: !Int64
  , tsDocumentsClosed  :: !Int64
  } deriving (Eq, Ord, Show)

instance SG.Semigroup TemplateStats where
  ts1 <> ts2 = TemplateStats {
      tsDocumentsSent = tsDocumentsSent ts1 + tsDocumentsSent ts2
    , tsDocumentsClosed = tsDocumentsClosed ts1 + tsDocumentsClosed ts2
    }

instance Monoid TemplateStats where
  mempty = TemplateStats 0 0
  mappend = (SG.<>)

data ShareableLinkUsageStats = ShareableLinkUsageStats {
    slusTimeWindowStart  :: !UTCTime
  , slusTemplateId       :: !Int64
  , slusTemplateTitle    :: !String
  , slusTemplateStats    :: !TemplateStats
  } deriving (Eq, Ord, Show)
