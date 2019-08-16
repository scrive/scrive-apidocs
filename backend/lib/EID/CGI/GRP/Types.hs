module EID.CGI.GRP.Types (
    CGISEBankIDSignature(..)
  , CGISEBankIDAuthentication(..)
  , GrpFault(..)
  , grpFaultText
  , xpGrpFault
  , AuthRequest(..)
  , SignRequest(..)
  , AutoStartToken
  , unAutoStartToken
  , AuthResponse(..)
  , SignResponse(..)
  , xpAuthResponse
  , xpSignResponse
  , CollectRequest(..)
  , ProgressStatus(..)
  , progressStatusText
  , CollectResponse(..)
  , xpCollectResponse
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid as Monoid
import Data.Unjson
import Network.SOAP.Parsing.Cursor
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (content, many, node)
import qualified Data.Text as T

import Log.Identifier
import Session.Cookies
import Text.XML.Parser

-- | Final BankID signature.
data CGISEBankIDSignature = CGISEBankIDSignature {
  cgisebidsSignatoryName           :: !Text
, cgisebidsSignatoryPersonalNumber :: !Text
, cgisebidsSignatoryIP             :: !Text
, cgisebidsSignedText              :: !Text
, cgisebidsSignature               :: !ByteString
, cgisebidsOcspResponse            :: !ByteString
} deriving (Eq, Ord, Show)

-- | Final BankID signature.
data CGISEBankIDAuthentication = CGISEBankIDAuthentication {
  cgisebidaSignatoryName           :: !Text
, cgisebidaSignatoryPersonalNumber :: !Text
, cgisebidaSignatoryIP             :: !Text
, cgisebidaSignature               :: !ByteString
, cgisebidaOcspResponse            :: !ByteString
} deriving (Eq, Ord, Show)

----------------------------------------

-- | Represents error codes used in GRP api.
data GrpFault
  = InvalidParameters
  | AlreadyInProgress
  | AccessDeniedRp
  | Retry
  | InternalError
  | ExpiredTransaction
  | UserCancel
  | ClientError
  | CertificateError
  | Cancelled
  | StartFailed
  deriving (Eq, Ord, Show)

grpFaultText :: GrpFault -> Text
grpFaultText InvalidParameters  = "invalid_parameters"
grpFaultText AlreadyInProgress  = "already_in_progress"
grpFaultText AccessDeniedRp     = "access_denied_rp"
grpFaultText Retry              = "retry"
grpFaultText InternalError      = "internal_error"
grpFaultText ExpiredTransaction = "expired_transaction"
grpFaultText UserCancel         = "user_cancel"
grpFaultText ClientError        = "client_error"
grpFaultText CertificateError   = "certificate_error"
grpFaultText Cancelled          = "cancelled"
grpFaultText StartFailed        = "start_failed"

-- | Convert 'GrpFault' to JSON to show it to the client.
instance Unjson GrpFault where
  unjsonDef = disjointUnionOf "grp_fault" $ map grpFaultWithText [
      InvalidParameters
    , AlreadyInProgress
    , AccessDeniedRp
    , Retry
    , InternalError
    , ExpiredTransaction
    , UserCancel
    , ClientError
    , CertificateError
    , Cancelled
    , StartFailed
    ]
    where
      grpFaultWithText con = (grpFaultText con, (== con), pure con)

-- | Retrieve 'GrpFault' from SOAP response.
xpGrpFault :: XMLParser GrpFault
xpGrpFault = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "Fault" &/ laxElement "detail" &/ laxElement "GrpFault" &| \fault ->
  case readT "faultStatus" fault of
    "INVALID_PARAMETERS"  -> InvalidParameters
    "ALREADY_IN_PROGRESS" -> AlreadyInProgress
    "ACCESS_DENIED_RP"    -> AccessDeniedRp
    "RETRY"               -> Retry
    "INTERNAL_ERROR"      -> InternalError
    "EXPIRED_TRANSACTION" -> ExpiredTransaction
    "USER_CANCEL"         -> UserCancel
    "CLIENT_ERR"          -> ClientError
    "CERTIFICATE_ERR"     -> CertificateError
    "CANCELLED"           -> Cancelled
    "START_FAILED"        -> StartFailed
    gf                    -> unexpectedError $ "unknown faultStatus:" <+> gf

----------------------------------------

-- | Auth action request.
data AuthRequest = AuthRequest {
  arqPolicy          :: !Text
, arqDisplayName     :: !Text
, arqPersonalNumber  :: !Text
, arqProvider        :: !Text
} deriving (Eq, Ord, Show)

-- | Construct SOAP request from the 'AuthRequest'.
instance ToXML AuthRequest where
  toXML AuthRequest{..} =
    element "{http://funktionstjanster.se/grp/service/v1.0.0/}AuthenticateRequest" $ do
      element "policy" arqPolicy
      element "displayName" arqDisplayName
      element "personalNumber" arqPersonalNumber
      element "provider" arqProvider
      element "requirementAlternatives" $ do
        element "requirement" $ do
          element "condition" $ do
            element "key" ("AllowFingerprint" :: Text)
            element "value" ("yes" :: Text)

----------------------------------------

-- | Sign action request.
data SignRequest = SignRequest {
  srqPolicy          :: !Text
, srqDisplayName     :: !Text
, srqPersonalNumber  :: !Text
, srqUserVisibleData :: !Text
, srqProvider        :: !Text
} deriving (Eq, Ord, Show)

-- | Construct SOAP request from the 'SignRequest'.
instance ToXML SignRequest where
  toXML SignRequest{..} =
    element "{http://funktionstjanster.se/grp/service/v1.0.0/}SignRequest" $ do
      element "policy" srqPolicy
      element "displayName" srqDisplayName
      element "personalNumber" srqPersonalNumber
      element "userVisibleData" srqUserVisibleData
      element "provider" srqProvider
      element "requirementAlternatives" $ do
        element "requirement" $ do
          element "condition" $ do
            element "key" ("AllowFingerprint" :: Text)
            element "value" ("yes" :: Text)

----------------------------------------

newtype AutoStartToken = AutoStartToken Text
  deriving (Eq, Ord, Show)

unAutoStartToken :: AutoStartToken -> Text
unAutoStartToken (AutoStartToken t) = t


-- With autostart token we always return session id - so bankid app can bag user back to service
instance {-# OVERLAPPING #-} Unjson (AutoStartToken,SessionCookieInfo) where
  unjsonDef = objectOf $ (pure $ \at si -> (AutoStartToken at, si))
    <*> field "auto_start_token"
        (unAutoStartToken . fst)
        "Token for starting the application"
    <*> fieldBy "session_id"
        snd
        "Token for starting the application"
        (unjsonInvmapR
          ((maybe (fail "SessionCookieInfo") return) . maybeRead)
          (showt :: SessionCookieInfo -> Text)
          unjsonDef
        )

----------------------------------------

-- | Auth action response.
data AuthResponse = AuthResponse {
  arsTransactionID  :: !Text
, arsOrderRef       :: !Text
, arsAutoStartToken :: !AutoStartToken
} deriving (Eq, Ord, Show)

instance Loggable AuthResponse where
  logValue = toJSON
  logDefaultLabel _ = "auth_response"

instance ToJSON AuthResponse where
  toJSON AuthResponse{..} = object [
      "transaction_id" .= T.unpack arsTransactionID
    , "order_ref" .= T.unpack arsOrderRef
    , "auto_start_token" .= show arsAutoStartToken
    ]
  toEncoding AuthResponse{..} = pairs $ Monoid.mconcat [
      "transaction_id" .= T.unpack arsTransactionID
    , "order_ref" .= T.unpack arsOrderRef
    , "auto_start_token" .= show arsAutoStartToken
    ]

-- | Retrieve 'SignResponse' from SOAP response.
xpAuthResponse :: XMLParser AuthResponse
xpAuthResponse = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "AuthenticateResponse" &| \sr -> AuthResponse {
      arsTransactionID  = readT "transactionId"  sr
    , arsOrderRef       = readT "orderRef"       sr
    , arsAutoStartToken = AutoStartToken $ readT "AutoStartToken" sr
    }

----------------------------------------


-- | Sign action response.
data SignResponse = SignResponse {
  srsTransactionID  :: !Text
, srsOrderRef       :: !Text
, srsAutoStartToken :: !AutoStartToken
} deriving (Eq, Ord, Show)

instance Loggable SignResponse where
  logValue SignResponse{..} = object [
      "transaction_id" .= srsTransactionID
    , "order_ref" .= srsOrderRef
    , "auto_start_token" .= unAutoStartToken srsAutoStartToken
    ]
  logDefaultLabel _ = "sign_response"

-- | Retrieve 'SignResponse' from SOAP response.
xpSignResponse :: XMLParser SignResponse
xpSignResponse = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "SignResponse" &| \sr -> SignResponse {
      srsTransactionID  = readT "transactionId"  sr
    , srsOrderRef       = readT "orderRef"       sr
    , srsAutoStartToken = AutoStartToken $ readT "AutoStartToken" sr
    }

----------------------------------------

-- | Collect action request.
data CollectRequest = CollectRequest {
  crqPolicy        :: !Text
, crqTransactionID :: !Text
, crqOrderRef      :: !Text
, crqDisplayName   :: !Text
} deriving (Eq, Ord, Show)

-- | Construct SOAP request from the 'CollectRequest'.
instance ToXML CollectRequest where
  toXML CollectRequest{..} =
    element "{http://funktionstjanster.se/grp/service/v1.0.0/}CollectRequest" $ do
      element "policy" crqPolicy
      element "transactionId" crqTransactionID
      element "orderRef" crqOrderRef
      element "displayName" crqDisplayName

----------------------------------------

-- | Progress status of the BankID transaction.
data ProgressStatus
  = OutstandingTransaction
  | UserSign
  | NoClient
  | Started
  | Complete
  deriving (Eq, Ord, Show)



progressStatusText :: ProgressStatus -> Text
progressStatusText OutstandingTransaction = "outstanding_transaction"
progressStatusText UserSign               = "user_sign"
progressStatusText NoClient               = "no_client"
progressStatusText Started                = "started"
progressStatusText Complete               = "complete"

-- | Convert 'ProgressStatus' to JSON and show it to the client.
instance Unjson ProgressStatus where
  unjsonDef = disjointUnionOf "progress_status" $ map statusWithText [
      OutstandingTransaction
    , UserSign
    , NoClient
    , Started
    , Complete
    ]
    where
      statusWithText con = (progressStatusText con, (== con), pure con)

----------------------------------------

-- | Collect action response.
data CollectResponse = CollectResponse {
  crsProgressStatus :: !ProgressStatus
, crsSignature      :: !(Maybe Text)
, crsAttributes     :: ![(Text, Text)]
} deriving (Eq, Ord, Show)

-- | Retrieve 'CollectResponse' from SOAP response.
xpCollectResponse :: XMLParser CollectResponse
xpCollectResponse = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "CollectResponse" &| \cr -> CollectResponse {
      crsProgressStatus = readProgressStatus cr
    , crsSignature      = listToMaybe $ cr $/ laxElement "signature" &/ content
    , crsAttributes     = cr $/ laxElement "attributes" &| readAttribute
    }
 where
   readAttribute attr = (readT "name" attr, readT "value" attr)
   readProgressStatus cr = case readT "progressStatus" cr of
     "OUTSTANDING_TRANSACTION" -> OutstandingTransaction
     "USER_SIGN"               -> UserSign
     "NO_CLIENT"               -> NoClient
     "STARTED"                 -> Started
     "COMPLETE"                -> Complete
     status                    -> unexpectedError $ "xpCollectResponse: unknown progressStatus:" <+> status
