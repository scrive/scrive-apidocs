module EID.EIDService.Types (
    EIDServiceTransactionID(..)
  , unsafeEIDServiceTransactionID
  , fromEIDServiceTransactionID
  , EIDServiceTransactionProvider(..)
  , ToEIDServiceTransactionProvider(..)
  , EIDServiceEndpointType(..)
  , EIDServiceTransactionStatus (..)
  , EIDServiceAuthenticationKind(..)
  , CreateEIDServiceTransactionRequest(..)
  , CreateEIDServiceTransactionResponse(..)
  , EIDServiceTransactionResponse(..)
  , EIDServiceTransactionFromDB(..)

  -- Redundant types, will be removed later
  , EIDServiceVerimiAuthentication(..)
  , EIDServiceNLIDINAuthentication(..)
  , EIDServiceDKNemIDAuthentication(..)
  , EIDServiceNOBankIDAuthentication(..)
  , EIDServiceNLIDINSignature(..)
  , EIDServiceFITupasSignature(..)
  , EIDServiceOnfidoSignature(..)
  , OnfidoMethod(..)

  -- Provider specific, can be refactored once redundant types above are removed
  , EIDServiceDKNemIDInternalProvider(..)
  , unsafeEIDServiceDKNemIDInternalProviderFromInt16
  , EIDServiceNOBankIDInternalProvider(..)
  , unsafeEIDServiceNOBankIDInternalProviderFromInt16
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Encoding hiding (day)
import Data.ByteString (ByteString)
import Data.Int
import Data.Time
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Prelude hiding (empty)
import qualified Data.Text as T

import Doc.DocumentID
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
  parseJSON = fmap EIDServiceTransactionID . parseJSON

data EIDServiceTransactionProvider =
    EIDServiceTransactionProviderVerimi
  | EIDServiceTransactionProviderNLIDIN
  | EIDServiceTransactionProviderDKNemID
  | EIDServiceTransactionProviderNOBankID
  | EIDServiceTransactionProviderFITupas
  | EIDServiceTransactionProviderOnfido
  deriving (Eq, Ord, Show)

class ToEIDServiceTransactionProvider a where
  toProvider :: a -> EIDServiceTransactionProvider

  toEIDServiceProviderName :: a -> Text
  toEIDServiceProviderName = toEIDServiceProviderName . toProvider

  toRedirectURLName :: a -> Text
  toRedirectURLName = toRedirectURLName . toProvider

instance ToEIDServiceTransactionProvider EIDServiceTransactionProvider where
  toProvider = identity

  toEIDServiceProviderName EIDServiceTransactionProviderVerimi   = "verimi"
  toEIDServiceProviderName EIDServiceTransactionProviderNLIDIN   = "nlIDIN"
  toEIDServiceProviderName EIDServiceTransactionProviderDKNemID  = "dkNemID"
  toEIDServiceProviderName EIDServiceTransactionProviderNOBankID = "noBankID"
  toEIDServiceProviderName EIDServiceTransactionProviderFITupas  = "fiTupas"
  toEIDServiceProviderName EIDServiceTransactionProviderOnfido   = "onfido"

  toRedirectURLName EIDServiceTransactionProviderVerimi   = "verimi"
  toRedirectURLName EIDServiceTransactionProviderNLIDIN   = "idin"
  toRedirectURLName EIDServiceTransactionProviderDKNemID  = "nemid"
  toRedirectURLName EIDServiceTransactionProviderNOBankID = "nobankid"
  toRedirectURLName EIDServiceTransactionProviderFITupas  = "fitupas"
  toRedirectURLName EIDServiceTransactionProviderOnfido   = "onfido"

instance FromReqURI EIDServiceTransactionProvider where
  fromReqURI = \case
    "verimi"   -> Just EIDServiceTransactionProviderVerimi
    "idin"     -> Just EIDServiceTransactionProviderNLIDIN
    "nemid"    -> Just EIDServiceTransactionProviderDKNemID
    "nobankid" -> Just EIDServiceTransactionProviderNOBankID
    "fitupas"  -> Just EIDServiceTransactionProviderFITupas
    "onfido"   -> Just EIDServiceTransactionProviderOnfido
    _          -> Nothing

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
      6 -> return EIDServiceTransactionProviderOnfido
      _ -> throwM RangeError { reRange = [(1, 6)], reValue = n }

instance ToSQL EIDServiceTransactionProvider where
  type PQDest EIDServiceTransactionProvider = PQDest Int16
  toSQL EIDServiceTransactionProviderVerimi   = toSQL (1 :: Int16)
  toSQL EIDServiceTransactionProviderNLIDIN   = toSQL (2 :: Int16)
  toSQL EIDServiceTransactionProviderDKNemID  = toSQL (3 :: Int16)
  toSQL EIDServiceTransactionProviderNOBankID = toSQL (4 :: Int16)
  toSQL EIDServiceTransactionProviderFITupas  = toSQL (5 :: Int16)
  toSQL EIDServiceTransactionProviderOnfido   = toSQL (6 :: Int16)

data EIDServiceEndpointType = EIDServiceAuthEndpoint | EIDServiceSignEndpoint

instance FromReqURI EIDServiceEndpointType where
  fromReqURI = \case
    "view" -> Just EIDServiceAuthEndpoint
    "sign" -> Just EIDServiceSignEndpoint
    _      -> Nothing

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
  parseJSON = withText "status text" $ \case
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
      3 -> return EIDServiceAuthToSign
      _ -> throwM RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL EIDServiceAuthenticationKind where
  type PQDest EIDServiceAuthenticationKind = PQDest Int16
  toSQL (EIDServiceAuthToView AuthenticationToView) = toSQL (1 :: Int16)
  toSQL (EIDServiceAuthToView AuthenticationToViewArchived) = toSQL (2 :: Int16)
  toSQL EIDServiceAuthToSign = toSQL (3 :: Int16)

instance FromJSON EIDServiceTransactionProvider where
  parseJSON = withText "provider text" $ \case
    "verimi"   -> return EIDServiceTransactionProviderVerimi
    "nlIDIN"   -> return EIDServiceTransactionProviderNLIDIN
    "dkNemID"  -> return EIDServiceTransactionProviderDKNemID
    "noBankID" -> return EIDServiceTransactionProviderNOBankID
    "fiTupas"  -> return EIDServiceTransactionProviderFITupas
    "onfido"   -> return EIDServiceTransactionProviderOnfido
    _          -> fail "JSON is invalid: unrecognised value for \"provider\""

-- Corresponds to POST /transaction/new request to EID service
data CreateEIDServiceTransactionRequest a = CreateEIDServiceTransactionRequest {
    cestDomain :: Text
  , cestEIDServiceProvider :: EIDServiceTransactionProvider
  , cestDocumentID :: DocumentID
  , cestSignatoryLinkID :: SignatoryLinkID
  , cestAuthKind :: EIDServiceAuthenticationKind
  , cestKontraRedirectUrl :: Maybe Text
  , cestmProviderParams :: Maybe a
  }

instance ToEIDServiceTransactionProvider (CreateEIDServiceTransactionRequest a) where
  toProvider = cestEIDServiceProvider

instance ToJSON a => ToJSON (CreateEIDServiceTransactionRequest a) where
  toJSON _ = Null
  toEncoding req@CreateEIDServiceTransactionRequest {..} =
    pairs
      $  ("method" .= ("auth" :: Text))
      <> ("provider" .= eidServiceProviderName)
      <> ("redirectUrl" .= redirectUrl)
      <> providerParams
    where
      eidServiceProviderName = toEIDServiceProviderName req
      makeProviderParams =
        pair "providerParameters" . pairs . pair eidServiceProviderName
      providerParams = maybe mempty (makeProviderParams . toEncoding) cestmProviderParams
      redirectUrl =
        let providerName = toRedirectURLName req
            endpointType = case cestAuthKind of
              (EIDServiceAuthToView _) -> "view"
              EIDServiceAuthToSign     -> "sign"
            rdFragment = case cestKontraRedirectUrl of
              Just kontraRedirect -> "?redirect=" <> kontraRedirect
              Nothing             -> ""
        in  cestDomain
              <> "/eid-service/redirect-endpoint/"
              <> providerName
              <> "/"
              <> endpointType
              <> "/"
              <> showt cestDocumentID
              <> "/"
              <> showt cestSignatoryLinkID
              <> rdFragment

-- Corresponds to POST /transaction/new response from EID service
data CreateEIDServiceTransactionResponse = CreateEIDServiceTransactionResponse {
    cestRespTransactionID :: EIDServiceTransactionID
  , cestRespAccessUrl :: Text
  }

instance FromJSON CreateEIDServiceTransactionResponse where
  parseJSON = withObject "object" $ \o -> do
    tid       <- o .: "id"
    accessUrl <- o .: "accessUrl"
    return CreateEIDServiceTransactionResponse { cestRespTransactionID = tid
                                               , cestRespAccessUrl     = accessUrl
                                               }

-- Corresponds to GET /transaction/{transaction_id} response from EID service
data EIDServiceTransactionResponse a = EIDServiceTransactionResponse {
    estRespID :: EIDServiceTransactionID
  , estRespProvider :: EIDServiceTransactionProvider
  , estRespStatus :: EIDServiceTransactionStatus
  , estRespCompletionData :: Maybe a
  } deriving Show

instance ToEIDServiceTransactionProvider (EIDServiceTransactionResponse a) where
  toProvider = estRespProvider

instance FromJSON a => FromJSON (EIDServiceTransactionResponse a) where
  parseJSON outer = withObject "object" return outer >>= \o -> do
    tid      <- o .: "id"
    provider <- o .: "provider"
    status   <- o .: "status"
    mcd      <- parseJSON outer <|> return Nothing
    return EIDServiceTransactionResponse { estRespID             = tid
                                         , estRespProvider       = provider
                                         , estRespStatus         = status
                                         , estRespCompletionData = mcd
                                         }

data EIDServiceTransactionFromDB = EIDServiceTransactionFromDB
  { estID :: !EIDServiceTransactionID
  , estStatus :: !EIDServiceTransactionStatus
  , estSignatoryLinkID :: !SignatoryLinkID
  , estAuthKind :: !EIDServiceAuthenticationKind
  , estProvider :: !EIDServiceTransactionProvider
  , estSessionID :: !SessionID
  , estDeadline :: !UTCTime
  } deriving (Show)

-- TODO: XAUTHENTICATION TYPES ARE REDUNDANT AND ARE  1:1 EQUIVALENT TO COMPLETION DATA
-- TODO: THE FUNCTIONS THAT RELY ON THESE SHOULD USE TRANSACTION/COMPLETEION DATA DIRECTLY

data EIDServiceVerimiAuthentication = EIDServiceVerimiAuthentication
  { eidServiceVerimiName            :: !T.Text
  , eidServiceVerimiVerifiedEmail   :: !(Maybe T.Text)
  , eidServiceVerimiVerifiedPhone   :: !(Maybe T.Text)
  } deriving (Eq, Ord, Show)

data EIDServiceNLIDINAuthentication = EIDServiceNLIDINAuthentication
  { eidServiceIDINName            :: !T.Text
  , eidServiceIDINVerifiedPhone   :: !(Maybe T.Text)
  , eidServiceIDINBirthDate       :: !(Maybe T.Text)
  , eidServiceIDINCustomerID      :: !(Maybe T.Text)
} deriving (Eq, Ord, Show)

data EIDServiceDKNemIDAuthentication = EIDServiceDKNemIDAuthentication
  { eidServiceNemIDInternalProvider :: !EIDServiceDKNemIDInternalProvider
  , eidServiceNemIDSignatoryName    :: !T.Text
  , eidServiceNemIDDateOfBirth      :: !T.Text
  , eidServiceNemIDCertificate      :: !ByteString
  } deriving (Eq, Ord, Show)

data EIDServiceNOBankIDAuthentication = EIDServiceNOBankIDAuthentication
  { eidServiceNOBankIDInternalProvider     :: !EIDServiceNOBankIDInternalProvider
  , eidServiceNOBankIDSignatoryName        :: !Text
  , eidServiceNOBankIDPhoneNumber          :: !(Maybe Text)
  , eidServiceNOBankIDDateOfBirth          :: !Text
  , eidServiceNOBankIDCertificate          :: !(Maybe ByteString)
  } deriving (Eq, Ord, Show)

data EIDServiceNLIDINSignature = EIDServiceNLIDINSignature
  { unEIDServiceIDINSigSignatoryName :: Text
  , unEIDServiceIDINSigDateOfBirth :: Text
  , unEIDServiceIDINSigCustomerID :: Text
  }
  deriving (Eq, Ord, Show)

data EIDServiceFITupasSignature = EIDServiceFITupasSignature
  { eidServiceFITupasSigSignatoryName :: Text
  , eidServiceFITupasSigPersonalNumber :: Maybe Text
  , eidServiceFITupasSigDateOfBirth :: Text
  }
  deriving (Eq, Ord, Show)

data OnfidoMethod = OnfidoDocumentCheck | OnfidoDocumentAndPhotoCheck
  deriving (Eq, Ord, Show)

instance ToJSON OnfidoMethod where
  toJSON OnfidoDocumentCheck         = String "document"
  toJSON OnfidoDocumentAndPhotoCheck = String "documentFacialSimilarityPhoto"

instance FromJSON OnfidoMethod where
  parseJSON (String "document") = return OnfidoDocumentCheck
  parseJSON (String "documentFacialSimilarityPhoto") = return OnfidoDocumentAndPhotoCheck
  parseJSON _                   = mzero

data EIDServiceOnfidoSignature = EIDServiceOnfidoSignature
  { eidServiceOnfidoSigSignatoryName :: Text
  , eidServiceOnfidoSigDateOfBirth :: Text
  , eidServiceOnfidoSigMethod :: OnfidoMethod
  }
  deriving (Eq, Ord, Show)

--TODO: THESE BELOW CAN'T BE REFACTORED AWAY OR INTO THE PROVIDER MODULES UNTIL THE
--TODO: AUTH/SIG TYPES ABOVE ARE REMOVED AND THE TRANSACTION/COMPLETION DATA USED DIRECTLY

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
