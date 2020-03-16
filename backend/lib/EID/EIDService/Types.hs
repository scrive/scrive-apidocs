module EID.EIDService.Types (
      EIDServiceTransactionID
    , unsafeEIDServiceTransactionID
    , fromEIDServiceTransactionID

    , EIDServiceTransactionProvider(..)
    , EIDServiceTransactionStatus(..)
    , EIDServiceAuthenticationKind(..)
    , EIDServiceTransaction(..)

    , EIDServiceVerimiAuthentication(..)
    , EIDServiceIDINAuthentication(..)
    , EIDServiceIDINSignature(..)
    , CompleteIDINEIDServiceTransactionData(..)
    , EIDServiceNemIDAuthentication(..)
    , EIDServiceNemIDInternalProvider(..)
    , unsafeEIDServiceNemIDInternalProviderFromInt16
    , CompleteNemIDEIDServiceTransactionData(..)
    , EIDServiceNOBankIDAuthentication(..)
    , EIDServiceNOBankIDInternalProvider(..)
    , unsafeEIDServiceNOBankIDInternalProviderFromInt16
    , CompleteNOBankIDEIDServiceTransactionData(..)
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
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
  fromSQL mbase = EIDServiceTransactionID <$> fromSQL mbase

instance ToSQL EIDServiceTransactionID where
  type PQDest EIDServiceTransactionID = PQDest T.Text
  toSQL (EIDServiceTransactionID tid) = toSQL tid

unsafeEIDServiceTransactionID :: T.Text -> EIDServiceTransactionID
unsafeEIDServiceTransactionID = EIDServiceTransactionID

fromEIDServiceTransactionID :: EIDServiceTransactionID -> T.Text
fromEIDServiceTransactionID (EIDServiceTransactionID tid) = tid

data EIDServiceTransactionProvider =
    EIDServiceTransactionProviderVerimi
  | EIDServiceTransactionProviderIDIN
  | EIDServiceTransactionProviderNemID
  | EIDServiceTransactionProviderNOBankID
  deriving (Eq, Ord, Show)

instance PQFormat EIDServiceTransactionProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceTransactionProvider where
  type PQBase EIDServiceTransactionProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceTransactionProviderVerimi
      2 -> return EIDServiceTransactionProviderIDIN
      3 -> return EIDServiceTransactionProviderNemID
      4 -> return EIDServiceTransactionProviderNOBankID
      _ -> throwM RangeError { reRange = [(1, 4)], reValue = n }

instance ToSQL EIDServiceTransactionProvider where
  type PQDest EIDServiceTransactionProvider = PQDest Int16
  toSQL EIDServiceTransactionProviderVerimi   = toSQL (1 :: Int16)
  toSQL EIDServiceTransactionProviderIDIN     = toSQL (2 :: Int16)
  toSQL EIDServiceTransactionProviderNemID    = toSQL (3 :: Int16)
  toSQL EIDServiceTransactionProviderNOBankID = toSQL (4 :: Int16)

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
      _ -> throwM RangeError { reRange = [(1, 5)], reValue = n }

instance ToSQL EIDServiceTransactionStatus where
  type PQDest EIDServiceTransactionStatus = PQDest Int16
  toSQL EIDServiceTransactionStatusNew                = toSQL (1 :: Int16)
  toSQL EIDServiceTransactionStatusStarted            = toSQL (2 :: Int16)
  toSQL EIDServiceTransactionStatusFailed             = toSQL (3 :: Int16)
  toSQL EIDServiceTransactionStatusCompleteAndSuccess = toSQL (4 :: Int16)
  toSQL EIDServiceTransactionStatusCompleteAndFailed  = toSQL (5 :: Int16)

data EIDServiceAuthenticationKind =
  EIDServiceAuthToView AuthenticationKind
  | EIDServiceAuthToSign
  deriving (Eq, Ord, Show)


instance PQFormat EIDServiceAuthenticationKind where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceAuthenticationKind where
  type PQBase EIDServiceAuthenticationKind = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return $ EIDServiceAuthToView AuthenticationToView
      2 -> return $ EIDServiceAuthToView AuthenticationToViewArchived
      3 -> return $ EIDServiceAuthToSign
      _ -> throwM RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL EIDServiceAuthenticationKind where
  type PQDest EIDServiceAuthenticationKind = PQDest Int16
  toSQL (EIDServiceAuthToView AuthenticationToView) = toSQL (1 :: Int16)
  toSQL (EIDServiceAuthToView AuthenticationToViewArchived) = toSQL (2 :: Int16)
  toSQL EIDServiceAuthToSign = toSQL (3 :: Int16)

data EIDServiceTransaction = EIDServiceTransaction
  { estID :: !EIDServiceTransactionID
  , estStatus :: !EIDServiceTransactionStatus
  , estSignatoryLinkID :: !SignatoryLinkID
  , estAuthKind :: !EIDServiceAuthenticationKind
  , estProvider :: !EIDServiceTransactionProvider
  , estSessionID :: !SessionID
  , estDeadline :: !UTCTime
  } deriving (Show)

data EIDServiceVerimiAuthentication = EIDServiceVerimiAuthentication
  { eidServiceVerimiName            :: !(T.Text)
  , eidServiceVerimiVerifiedEmail   :: !(Maybe T.Text)
  , eidServiceVerimiVerifiedPhone   :: !(Maybe T.Text)
  } deriving (Eq, Ord, Show)

data EIDServiceIDINAuthentication = EIDServiceIDINAuthentication
  { eidServiceIDINName            :: !(T.Text)
  , eidServiceIDINVerifiedEmail   :: !(Maybe T.Text)
  , eidServiceIDINVerifiedPhone   :: !(Maybe T.Text)
  , eidServiceIDINBirthDate       :: !(Maybe T.Text)
  , eidServiceIDINCustomerID      :: !(Maybe T.Text)
} deriving (Eq, Ord, Show)

data EIDServiceNemIDAuthentication = EIDServiceNemIDAuthentication
  { eidServiceNemIDInternalProvider :: !(EIDServiceNemIDInternalProvider)
  , eidServiceNemIDSignatoryName    :: !(T.Text)
  , eidServiceNemIDDateOfBirth      :: !(T.Text)
  , eidServiceNemIDCertificate      :: !(ByteString)
  } deriving (Eq, Ord, Show)

data EIDServiceNOBankIDAuthentication = EIDServiceNOBankIDAuthentication
  { eidServiceNOBankIDInternalProvider     :: !(EIDServiceNOBankIDInternalProvider)
  , eidServiceNOBankIDSignatoryName        :: !Text
  , eidServiceNOBankIDPhoneNumber          :: !(Maybe Text)
  , eidServiceNOBankIDDateOfBirth          :: !Text
  , eidServiceNOBankIDCertificate          :: !(Maybe ByteString)
  } deriving (Eq, Ord, Show)

data EIDServiceIDINSignature = EIDServiceIDINSignature
  { unEIDServiceIDINSignature :: CompleteIDINEIDServiceTransactionData
  }
  deriving (Eq, Ord, Show)

data CompleteIDINEIDServiceTransactionData = CompleteIDINEIDServiceTransactionData
  { eiditdName :: T.Text
  , eiditdVerifiedEmail :: T.Text
  , eiditdBirthDate :: T.Text
  , eiditdCustomerID :: T.Text
  } deriving (Eq, Ord, Show)

data CompleteNemIDEIDServiceTransactionData = CompleteNemIDEIDServiceTransactionData
  { eidnidInternalProvider :: !(EIDServiceNemIDInternalProvider)
  , eidnidSSN :: !(T.Text)
  , eidnidBirthDate :: !(T.Text)
  , eidnidCertificate :: !(T.Text)
  , eidnidDistinguishedName :: !(T.Text)
  , eidnidPid :: !(T.Text)
  } deriving (Eq, Ord, Show)

data CompleteNOBankIDEIDServiceTransactionData = CompleteNOBankIDEIDServiceTransactionData
  { eidnobidInternalProvider :: !(EIDServiceNOBankIDInternalProvider)
  , eidnobidBirthDate :: !(Maybe T.Text)
  , eidnobidCertificate :: !(Maybe T.Text)
  , eidnobidDistinguishedName :: !T.Text
  , eidnobidIssuerDistinguishedName :: !T.Text
  , eidnobidName :: !(Maybe Text)
  , eidnobidPhoneNumber :: !(Maybe T.Text)
  , eidnobidPid :: !(T.Text)
  } deriving (Eq, Ord, Show)

data EIDServiceNemIDInternalProvider
  = EIDServiceNemIDKeyCard
  | EIDServiceNemIDKeyFile
  deriving (Eq, Ord, Show)

unsafeEIDServiceNemIDInternalProviderFromInt16 :: Int16 -> EIDServiceNemIDInternalProvider
unsafeEIDServiceNemIDInternalProviderFromInt16 v = case v of
  1 -> EIDServiceNemIDKeyCard
  2 -> EIDServiceNemIDKeyFile
  _ ->
    unexpectedError "Range error while fetching NetsNOBankIDInternalProvider from Int16"

instance PQFormat EIDServiceNemIDInternalProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceNemIDInternalProvider where
  type PQBase EIDServiceNemIDInternalProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceNemIDKeyCard
      2 -> return EIDServiceNemIDKeyFile
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL EIDServiceNemIDInternalProvider where
  type PQDest EIDServiceNemIDInternalProvider = PQDest Int16
  toSQL EIDServiceNemIDKeyCard = toSQL (1 :: Int16)
  toSQL EIDServiceNemIDKeyFile = toSQL (2 :: Int16)

data EIDServiceNOBankIDInternalProvider
  = EIDServiceNOBankIDStandard
  | EIDServiceNOBankIDMobile
  deriving (Eq, Ord, Show)

unsafeEIDServiceNOBankIDInternalProviderFromInt16
  :: Int16 -> EIDServiceNOBankIDInternalProvider
unsafeEIDServiceNOBankIDInternalProviderFromInt16 v = case v of
  1 -> EIDServiceNOBankIDStandard
  2 -> EIDServiceNOBankIDMobile
  _ ->
    unexpectedError "Range error while fetching NetsNOBankIDInternalProvider from Int16"

instance PQFormat EIDServiceNOBankIDInternalProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceNOBankIDInternalProvider where
  type PQBase EIDServiceNOBankIDInternalProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceNOBankIDStandard
      2 -> return EIDServiceNOBankIDMobile
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL EIDServiceNOBankIDInternalProvider where
  type PQDest EIDServiceNOBankIDInternalProvider = PQDest Int16
  toSQL EIDServiceNOBankIDStandard = toSQL (1 :: Int16)
  toSQL EIDServiceNOBankIDMobile   = toSQL (2 :: Int16)

