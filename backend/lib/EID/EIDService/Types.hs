module EID.EIDService.Types (
      EIDServiceTransactionID
    , unsafeEIDServiceTransactionID
    , fromEIDServiceTransactionID

    , HasEIDServiceName(..)
    , EIDServiceTransactionProvider(..)
    , EIDServiceProviderParams(..)
    , eidServiceProviderParamsToProvider
    , EIDServiceTransactionStatus(..)
    , EIDServiceAuthenticationKind(..)
    , EIDServiceTransaction(..)

    , EIDServiceVerimiAuthentication(..)
    , CompleteVerimiEIDServiceTransactionData(..)
    , EIDServiceNLIDINAuthentication(..)
    , EIDServiceNLIDINSignature(..)
    , CompleteNLIDINEIDServiceTransactionData(..)
    , EIDServiceFITupasSignature(..)
    , CompleteFITupasEIDServiceTransactionData(..)
    , EIDServiceDKNemIDAuthentication(..)
    , EIDServiceDKNemIDInternalProvider(..)
    , unsafeEIDServiceDKNemIDInternalProviderFromInt16
    , CompleteDKNemIDEIDServiceTransactionData(..)
    , EIDServiceNOBankIDAuthentication(..)
    , EIDServiceNOBankIDInternalProvider(..)
    , unsafeEIDServiceNOBankIDInternalProviderFromInt16
    , CompleteNOBankIDEIDServiceTransactionData(..)

    , dateOfBirthFromDKPersonalNumber
  ) where

import Control.Monad.Catch
import Data.Aeson
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

instance FromJSON EIDServiceTransactionID where
  parseJSON = fmap EIDServiceTransactionID . withObject "object" (.: "id")

class HasEIDServiceName a where
  toEIDServiceName :: a -> Text

data EIDServiceTransactionProvider =
    EIDServiceTransactionProviderVerimi
  | EIDServiceTransactionProviderNLIDIN
  | EIDServiceTransactionProviderDKNemID
  | EIDServiceTransactionProviderNOBankID
  | EIDServiceTransactionProviderFITupas
  deriving (Eq, Ord, Show)

instance HasEIDServiceName EIDServiceTransactionProvider where
  toEIDServiceName EIDServiceTransactionProviderVerimi   = "verimi"
  toEIDServiceName EIDServiceTransactionProviderNLIDIN   = "nlIDIN"
  toEIDServiceName EIDServiceTransactionProviderDKNemID  = "dkNemID"
  toEIDServiceName EIDServiceTransactionProviderNOBankID = "noBankID"
  toEIDServiceName EIDServiceTransactionProviderFITupas  = "fiTupas"

instance PQFormat EIDServiceTransactionProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceTransactionProvider where
  type PQBase EIDServiceTransactionProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceTransactionProviderVerimi
      2 -> return EIDServiceTransactionProviderNLIDIN
      3 -> return EIDServiceTransactionProviderDKNemID
      4 -> return EIDServiceTransactionProviderNOBankID
      5 -> return EIDServiceTransactionProviderFITupas
      _ -> throwM RangeError { reRange = [(1, 5)], reValue = n }

instance ToSQL EIDServiceTransactionProvider where
  type PQDest EIDServiceTransactionProvider = PQDest Int16
  toSQL EIDServiceTransactionProviderVerimi   = toSQL (1 :: Int16)
  toSQL EIDServiceTransactionProviderNLIDIN   = toSQL (2 :: Int16)
  toSQL EIDServiceTransactionProviderDKNemID  = toSQL (3 :: Int16)
  toSQL EIDServiceTransactionProviderNOBankID = toSQL (4 :: Int16)
  toSQL EIDServiceTransactionProviderFITupas  = toSQL (5 :: Int16)

data EIDServiceProviderParams =
  EIDServiceProviderParamsVerimi {
    esppRedirectURL :: Text
  } |
  EIDServiceProviderParamsNLIDIN {
    esppRedirectURL :: Text
  } |
  EIDServiceProviderParamsDKNemID {
    esppRedirectURL :: Text
  , esppUILocale :: Text
  } |
  EIDServiceProviderParamsNOBankID {
    esppRedirectURL :: Text
  , esppPhoneNumber :: Maybe Text
  , esppPersonalNumber :: Text
  } |
  EIDServiceProviderParamsFITupas {
    esppRedirectURL :: Text
  }

eidServiceProviderParamsToProvider
  :: EIDServiceProviderParams -> EIDServiceTransactionProvider
eidServiceProviderParamsToProvider EIDServiceProviderParamsVerimi{} =
  EIDServiceTransactionProviderVerimi
eidServiceProviderParamsToProvider EIDServiceProviderParamsNLIDIN{} =
  EIDServiceTransactionProviderNLIDIN
eidServiceProviderParamsToProvider EIDServiceProviderParamsDKNemID{} =
  EIDServiceTransactionProviderDKNemID
eidServiceProviderParamsToProvider EIDServiceProviderParamsNOBankID{} =
  EIDServiceTransactionProviderNOBankID
eidServiceProviderParamsToProvider EIDServiceProviderParamsFITupas{} =
  EIDServiceTransactionProviderFITupas

instance HasEIDServiceName EIDServiceProviderParams where
  toEIDServiceName = toEIDServiceName . eidServiceProviderParamsToProvider

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

instance FromJSON EIDServiceTransactionStatus where
  parseJSON outer = withObject "object" (.: "status") outer >>= \s -> case (s :: Text) of
    "new"      -> return EIDServiceTransactionStatusNew
    "started"  -> return EIDServiceTransactionStatusStarted
    "failed"   -> return EIDServiceTransactionStatusFailed
    "complete" -> return EIDServiceTransactionStatusCompleteAndSuccess
    _          -> fail "JSON is invalid: unrecognised value for \"status\""

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

data EIDServiceNLIDINAuthentication = EIDServiceNLIDINAuthentication
  { eidServiceIDINName            :: !(T.Text)
  , eidServiceIDINVerifiedPhone   :: !(Maybe T.Text)
  , eidServiceIDINBirthDate       :: !(Maybe T.Text)
  , eidServiceIDINCustomerID      :: !(Maybe T.Text)
} deriving (Eq, Ord, Show)

data EIDServiceDKNemIDAuthentication = EIDServiceDKNemIDAuthentication
  { eidServiceNemIDInternalProvider :: !(EIDServiceDKNemIDInternalProvider)
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

data EIDServiceNLIDINSignature = EIDServiceNLIDINSignature
  { unEIDServiceIDINSignature :: CompleteNLIDINEIDServiceTransactionData
  }
  deriving (Eq, Ord, Show)

data EIDServiceFITupasSignature = EIDServiceFITupasSignature
  { eidServiceFITupasSigSignatoryName :: Text
  , eidServiceFITupasSigPersonalNumber :: Maybe Text
  , eidServiceFITupasSigDateOfBirth :: Text
  }
  deriving (Eq, Ord, Show)

data CompleteVerimiEIDServiceTransactionData = CompleteVerimiEIDServiceTransactionData {
    eidvtdName :: T.Text
  , eidvtdVerifiedEmail :: T.Text
  }

data CompleteFITupasEIDServiceTransactionData = CompleteFITupasEIDServiceTransactionData
  { eidtupasName :: !Text
  , eidtupasBirthDate :: !Text
  , eidtupasDistinguishedName :: !Text  -- may contain the personal number
  , eidtupasBank :: !(Maybe Text)  -- absent when using Mobile ID
  , eidtupasPid :: !(Maybe Text)
  -- ^ 'A fixed identifier for the user set in the E-Ident / FTN service.' (from
  -- the Nets documentation)
  , eidtupasSSN :: !(Maybe Text)  -- seems to be absent for 'legal persons'
  } deriving (Eq, Ord, Show)

data CompleteNLIDINEIDServiceTransactionData = CompleteNLIDINEIDServiceTransactionData
  { eiditdName :: T.Text
  , eiditdBirthDate :: T.Text
  , eiditdCustomerID :: T.Text
  } deriving (Eq, Ord, Show)

data CompleteDKNemIDEIDServiceTransactionData = CompleteDKNemIDEIDServiceTransactionData
  { eidnidInternalProvider :: !(EIDServiceDKNemIDInternalProvider)
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

data EIDServiceDKNemIDInternalProvider
  = EIDServiceNemIDKeyCard
  | EIDServiceNemIDKeyFile
  deriving (Eq, Ord, Show)

unsafeEIDServiceDKNemIDInternalProviderFromInt16
  :: Int16 -> EIDServiceDKNemIDInternalProvider
unsafeEIDServiceDKNemIDInternalProviderFromInt16 v = case v of
  1 -> EIDServiceNemIDKeyCard
  2 -> EIDServiceNemIDKeyFile
  _ ->
    unexpectedError "Range error while fetching NetsNOBankIDInternalProvider from Int16"

instance PQFormat EIDServiceDKNemIDInternalProvider where
  pqFormat = pqFormat @Int16

instance FromSQL EIDServiceDKNemIDInternalProvider where
  type PQBase EIDServiceDKNemIDInternalProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EIDServiceNemIDKeyCard
      2 -> return EIDServiceNemIDKeyFile
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL EIDServiceDKNemIDInternalProvider where
  type PQDest EIDServiceDKNemIDInternalProvider = PQDest Int16
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

-- TODO: Decide where to put this
dateOfBirthFromDKPersonalNumber :: Text -> Text
dateOfBirthFromDKPersonalNumber personalnumber =
  case T.chunksOf 2 (T.take 6 $ personalnumber) of
    [day, month, year] ->
      let
        yearWithoutCentury = read year
        firstDigitOfSequenceNumber = T.index personalnumber 7
        century = showt $ resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      in
        day <> "." <> month <> "." <> century <> year
    _ ->
      unexpectedError
        $  "This personal number cannot be formatted to date: "
        <> personalnumber
  where
    resolveCentury :: Int -> Char -> Int
    resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      | firstDigitOfSequenceNumber < '4'
      = 19
      | firstDigitOfSequenceNumber == '4'
      = if yearWithoutCentury < 37 then 20 else 19
      | firstDigitOfSequenceNumber > '4' && firstDigitOfSequenceNumber < '9'
      = if yearWithoutCentury < 58 then 20 else 18
      | otherwise
      = if yearWithoutCentury < 37 then 20 else 19
