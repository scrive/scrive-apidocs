module EID.CGI.GRP.Data (
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
import Data.Unjson
import Network.SOAP.Parsing.Cursor
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (content, many, node)
import qualified Data.Text as T

import KontraPrelude
import Log.Identifier
import Network.SOAP.Call
import Session.Cookies

-- | Final BankID signature.
data CGISEBankIDSignature = CGISEBankIDSignature {
  cgisebidsSignatoryName           :: !T.Text
, cgisebidsSignatoryPersonalNumber :: !T.Text
, cgisebidsSignedText              :: !T.Text
, cgisebidsSignature               :: !ByteString
, cgisebidsOcspResponse            :: !ByteString
} deriving (Eq, Ord, Show)

-- | Final BankID signature.
data CGISEBankIDAuthentication = CGISEBankIDAuthentication {
  cgisebidaSignatoryName           :: !T.Text
, cgisebidaSignatoryPersonalNumber :: !T.Text
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

grpFaultText :: GrpFault -> T.Text
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
    gf                    -> $unexpectedError $ "unknown faultStatus:" <+> T.unpack gf

----------------------------------------

-- | Auth action request.
data AuthRequest = AuthRequest {
  arqPolicy          :: !T.Text
, arqDisplayName     :: !T.Text
, arqPersonalNumber  :: !T.Text
, arqProvider        :: !T.Text
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
            element "key" ("AllowFingerprint" :: T.Text)
            element "value" ("yes" :: T.Text)

----------------------------------------

-- | Sign action request.
data SignRequest = SignRequest {
  srqPolicy          :: !T.Text
, srqDisplayName     :: !T.Text
, srqPersonalNumber  :: !T.Text
, srqUserVisibleData :: !T.Text
, srqProvider        :: !T.Text
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

----------------------------------------

newtype AutoStartToken = AutoStartToken T.Text
  deriving (Eq, Ord, Show)

unAutoStartToken :: AutoStartToken -> T.Text
unAutoStartToken (AutoStartToken t) = t


-- With autostart token we always return session id - so bankid app can bag user back to service
instance Unjson (AutoStartToken,SessionCookieInfo) where
  unjsonDef = objectOf $ (pure $ \at si -> (AutoStartToken at, si))
    <*> field "auto_start_token"
        (unAutoStartToken . fst)
        "Token for starting the application"
    <*> fieldBy "session_id"
        snd
        "Token for starting the application"
        (unjsonInvmapR ((maybe (fail "SessionCookieInfo")  return) . maybeRead) (show :: SessionCookieInfo -> String) unjsonDef)

----------------------------------------

-- | Auth action response.
data AuthResponse = AuthResponse {
  arsTransactionID  :: !T.Text
, arsOrderRef       :: !T.Text
, arsAutoStartToken :: !AutoStartToken
} deriving (Eq, Ord, Show)

instance LogObject AuthResponse where
  logObject = toJSON

instance LogDefaultLabel AuthResponse where
  logDefaultLabel _ = "auth_response"

instance ToJSON AuthResponse where
  toJSON AuthResponse{..} = object [
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
  srsTransactionID  :: !T.Text
, srsOrderRef       :: !T.Text
, srsAutoStartToken :: !AutoStartToken
} deriving (Eq, Ord, Show)

instance LogObject SignResponse where
  logObject SignResponse{..} = object [
      "transaction_id" .= srsTransactionID
    , "order_ref" .= srsOrderRef
    , "auto_start_token" .= unAutoStartToken srsAutoStartToken
    ]

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
  crqPolicy        :: !T.Text
, crqTransactionID :: !T.Text
, crqOrderRef      :: !T.Text
, crqDisplayName   :: !T.Text
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



progressStatusText :: ProgressStatus -> T.Text
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
, crsSignature      :: !(Maybe T.Text)
, crsAttributes     :: ![(T.Text, T.Text)]
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
     status                    -> $unexpectedError $ "xpCollectResponse: unknown progressStatus:" <+> T.unpack status
