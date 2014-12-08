module EID.CGI.GRP.Data (
    BankIDSignature(..)
  , GrpFault(..)
  , xpGrpFault
  , SignRequest(..)
  , AutoStartToken
  , unAutoStartToken
  , SignResponse(..)
  , xpSignResponse
  , CollectRequest(..)
  , ProgressStatus(..)
  , CollectResponse(..)
  , xpCollectResponse
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid.Space
import Data.Text (Text)
import Data.Unjson
import Network.SOAP.Parsing.Cursor
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (content, many, node)
import qualified Data.Text as T

import DB hiding (InternalError)
import Network.SOAP.Call
import OurPrelude

-- | Final BankID signature.
data BankIDSignature = BankIDSignature {
  bidsSignatoryName           :: !Text
, bidsSignatoryPersonalNumber :: !Text
, bidsSignedText              :: !Text
, bidsSignature               :: !(Binary ByteString)
, bidsOcspResponse            :: !(Binary ByteString)
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

-- | Convert 'GrpFault' to JSON to show it to the client.
instance Unjson GrpFault where
  unjsonDef = DisjointUnjsonDef "grp_fault" [
      InvalidParameters  <=> "invalid_parameters"
    , AlreadyInProgress  <=> "already_in_progress"
    , AccessDeniedRp     <=> "access_denied_rp"
    , Retry              <=> "retry"
    , InternalError      <=> "internal_error"
    , ExpiredTransaction <=> "expired_transaction"
    , UserCancel         <=> "user_cancel"
    , ClientError        <=> "client_error"
    , CertificateError   <=> "certificate_error"
    , Cancelled          <=> "cancelled"
    , StartFailed        <=> "start_failed"
    ]
    where
      con <=> name = (name, (== con), pure con)

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

----------------------------------------

newtype AutoStartToken = AutoStartToken Text
  deriving (Eq, Ord, Show)

unAutoStartToken :: AutoStartToken -> Text
unAutoStartToken (AutoStartToken t) = t

instance Unjson AutoStartToken where
  unjsonDef = objectOf $ AutoStartToken
    <$> field "auto_start_token"
        unAutoStartToken
        "Token for starting the application"

----------------------------------------

-- | Sign action response.
data SignResponse = SignResponse {
  srsTransactionID  :: !Text
, srsOrderRef       :: !Text
, srsAutoStartToken :: !AutoStartToken
} deriving (Eq, Ord, Show)

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

-- | Convert 'ProgressStatus' to JSON and show it to the client.
instance Unjson ProgressStatus where
  unjsonDef = DisjointUnjsonDef "progress_status" [
      OutstandingTransaction <=> "outstanding_transaction"
    , UserSign               <=> "user_sign"
    , NoClient               <=> "no_client"
    , Started                <=> "started"
    , Complete               <=> "complete"
    ]
    where
      con <=> name = (name, (== con), pure con)

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
     status                    -> $unexpectedError $ "xpCollectResponse: unknown progressStatus:" <+> T.unpack status
