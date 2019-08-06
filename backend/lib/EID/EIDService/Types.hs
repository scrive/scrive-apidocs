module EID.EIDService.Types (
      EIDServiceTransactionID
    , unsafeEIDServiceTransactionID
    , fromEIDServiceTransactionID

    , EIDServiceTransactionProvider(..)
    , eidServiceTransactionProviderText
    , EIDServiceTransactionStatus(..)
    , EIDServiceTransaction(..)

    , EIDServiceVerimiAuthentication(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Time
import Database.PostgreSQL.PQTypes
import Prelude hiding (empty)
import qualified Data.Text as T

import Doc.SignatoryLinkID
import Doc.Types.SignatoryLink
import Log.Identifier
import Session.SessionID

newtype EIDServiceTransactionID = EIDServiceTransactionID T.Text
  deriving (Eq, Ord, Read, Show)

instance Identifier EIDServiceTransactionID where
  idDefaultLabel = "eid_transaction_id"
  idValue (EIDServiceTransactionID t) = stringIdentifier $ T.unpack t

instance PQFormat EIDServiceTransactionID where
  pqFormat = pqFormat @T.Text

instance FromSQL EIDServiceTransactionID where
  type PQBase EIDServiceTransactionID = PQBase T.Text
  fromSQL mbase =  EIDServiceTransactionID <$> fromSQL mbase

instance ToSQL EIDServiceTransactionID where
  type PQDest EIDServiceTransactionID = PQDest T.Text
  toSQL (EIDServiceTransactionID tid) = toSQL tid


unsafeEIDServiceTransactionID :: T.Text -> EIDServiceTransactionID
unsafeEIDServiceTransactionID = EIDServiceTransactionID

fromEIDServiceTransactionID :: EIDServiceTransactionID -> T.Text
fromEIDServiceTransactionID (EIDServiceTransactionID tid) = tid

data EIDServiceTransactionProvider =
  EIDServiceTransactionProviderVerimi
  deriving (Eq, Ord, Show)

instance PQFormat EIDServiceTransactionProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceTransactionProvider where
  type PQBase EIDServiceTransactionProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceTransactionProviderVerimi
      _ -> throwM RangeError {
        reRange = [(1, 1)]
      , reValue = n
      }

instance ToSQL EIDServiceTransactionProvider where
  type PQDest EIDServiceTransactionProvider = PQDest Int16
  toSQL EIDServiceTransactionProviderVerimi = toSQL (1::Int16)

eidServiceTransactionProviderText :: EIDServiceTransactionProvider -> T.Text
eidServiceTransactionProviderText EIDServiceTransactionProviderVerimi = "verimi"


-- In statuses we separate complete into two statuses, since
-- we can't force email/phone number validation on eid service side
data EIDServiceTransactionStatus =
  EIDServiceTransactionStatusNew      |
  EIDServiceTransactionStatusStarted  |
  EIDServiceTransactionStatusFailed   |
  EIDServiceTransactionStatusCompleteAndSuccess |
  -- When transaction is complete, there may be some other
  -- reason why we don't consider it a success.
  -- Example: email check failure, if it's done in kontrakcja
  EIDServiceTransactionStatusCompleteAndFailed
  deriving (Eq, Ord, Show)

instance PQFormat EIDServiceTransactionStatus where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceTransactionStatus where
  type PQBase EIDServiceTransactionStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceTransactionStatusNew
      2 -> return EIDServiceTransactionStatusStarted
      3 -> return EIDServiceTransactionStatusFailed
      4 -> return EIDServiceTransactionStatusCompleteAndSuccess
      5 -> return EIDServiceTransactionStatusCompleteAndFailed
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL EIDServiceTransactionStatus where
  type PQDest EIDServiceTransactionStatus = PQDest Int16
  toSQL EIDServiceTransactionStatusNew = toSQL (1::Int16)
  toSQL EIDServiceTransactionStatusStarted = toSQL (2::Int16)
  toSQL EIDServiceTransactionStatusFailed = toSQL (3::Int16)
  toSQL EIDServiceTransactionStatusCompleteAndSuccess = toSQL (4::Int16)
  toSQL EIDServiceTransactionStatusCompleteAndFailed = toSQL (5::Int16)


data EIDServiceTransaction = EIDServiceTransaction
  { estID :: !EIDServiceTransactionID
  , estStatus :: !EIDServiceTransactionStatus
  , estSignatoryLinkID :: !SignatoryLinkID
  , estAuthKind :: !AuthenticationKind
  , estProvider :: !EIDServiceTransactionProvider
  , estSessionID :: !SessionID
  , estDeadline :: !UTCTime
  } deriving (Show)



data EIDServiceVerimiAuthentication = EIDServiceVerimiAuthentication {
    eidServiceVerimiName            :: !(T.Text)
  , eidServiceVerimiVerifiedEmail   :: !(Maybe T.Text)
  , eidServiceVerimiVerifiedPhone   :: !(Maybe T.Text)
} deriving (Eq, Ord, Show)
