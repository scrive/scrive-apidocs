module User.Types.Stats
    (
      DocumentStats(..)
    , StatsPartition(..)
    , UserUsageStats(..)
    , ShareableLinkUsageStats(..)
    ) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import qualified Data.Semigroup as SG

data StatsPartition = PartitionByDay | PartitionByMonth

data DocumentStats = DocumentStats {
    dsDocumentsSent           :: !Int64
  , dsDocumentsClosed         :: !Int64
  , dsSignaturesClosed        :: !Int64
  , dsSMSSent                 :: !Int64
  , dsSMSSentViaTelia         :: !Int64
  , dsSEBankIDSignatures      :: !Int64
  , dsSEBankIDAuthentications :: !Int64
  , dsNOBankIDAuthentications :: !Int64
  , dsNemIDAuthentications    :: !Int64
  , dsNOBankIDSignatures      :: !Int64
  , dsNemIDSignatures         :: !Int64
  , dsTupasAuthentications    :: !Int64
  , dsShareableLinks          :: !Int64
  } deriving (Eq, Ord, Show)

instance SG.Semigroup DocumentStats where
  ds1 <> ds2 = DocumentStats
    { dsDocumentsSent           = dsDocumentsSent ds1 + dsDocumentsSent ds2
    , dsDocumentsClosed         = dsDocumentsClosed ds1 + dsDocumentsClosed ds2
    , dsSignaturesClosed        = dsSignaturesClosed ds1 + dsSignaturesClosed ds2
    , dsSMSSent                 = dsSMSSent ds1 + dsSMSSent ds2
    , dsSMSSentViaTelia         = dsSMSSentViaTelia ds1 + dsSMSSentViaTelia ds2
    , dsSEBankIDSignatures      = dsSEBankIDSignatures ds1 + dsSEBankIDSignatures ds2
    , dsSEBankIDAuthentications = dsSEBankIDAuthentications ds1
                                    + dsSEBankIDAuthentications ds2
    , dsNOBankIDAuthentications = dsNOBankIDAuthentications ds1
                                    + dsNOBankIDAuthentications ds2
    , dsNemIDAuthentications    = dsNemIDAuthentications ds1 + dsNemIDAuthentications ds2
    , dsNOBankIDSignatures      = dsNOBankIDSignatures ds1 + dsNOBankIDSignatures ds2
    , dsNemIDSignatures         = dsNemIDSignatures ds1 + dsNemIDSignatures ds2
    , dsTupasAuthentications    = dsTupasAuthentications ds1 + dsTupasAuthentications ds2
    , dsShareableLinks          = dsShareableLinks ds1 + dsShareableLinks ds2
    }

instance Monoid DocumentStats where
  mempty  = DocumentStats 0 0 0 0 0 0 0 0 0 0 0 0 0
  mappend = (SG.<>)

data UserUsageStats = UserUsageStats {
    uusTimeWindowStart  :: !UTCTime
  , uusUserEmail        :: !String
  , uusUserName         :: !String
  , uusDocumentStats    :: !DocumentStats
  } deriving (Eq, Ord, Show)

data ShareableLinkUsageStats = ShareableLinkUsageStats {
    slusTimeWindowStart  :: !UTCTime
  , slusTemplateId       :: !Int64
  , slusTemplateTitle    :: !String
  , slusDocumentStats    :: !DocumentStats
  } deriving (Eq, Ord, Show)
