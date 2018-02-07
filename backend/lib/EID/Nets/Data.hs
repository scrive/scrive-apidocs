module EID.Nets.Data (
      NetsNOBankIDInternalProvider(..)
    , unsafeNetsNOBankIDInternalProviderFromInt16
    , NetsNOBankIDAuthentication(..)
    , NetsDKNemIDInternalProvider(..)
    , unsafeNetsDKNemIDInternalProviderFromInt16
    , NetsDKNemIDAuthentication(..)
    , NetsNOBankIDSignature(..)

    -- Target passed inside request
    , NetsTarget(..)
    , decodeNetsTarget

    -- Request calls
    , GetAssertionRequest(..)
    , GetAssertionResponse(..)
    , xpGetAssertionResponse

    , NetsSignOrder(..)
    , InsertOrderRequest(..)
    , InsertOrderResponse(..)
    , xpInsertOrderResponse

    , GetSigningProcessesRequest(..)
    , GetSigningProcessesResponse(..)
    , xpGetSigningProcessesResponse

    , GetOrderStatusRequest(..)
    , GetOrderStatusResponse(..)
    , xpGetOrderStatusResponse

    , GetSDORequest(..)
    , GetSDOResponse(..)
    , xpGetSDOResponse

    , GetSDODetailsRequest(..)
    , GetSDODetailsResponse(..)
    , xpGetSDODetailsResponse

    , CancelOrderRequest(..)
    , CancelOrderResponse(..)
    , xpCancelOrderResponse

    , OrderStatus(..)

    , NetsSignStatus(..)
    , NetsFault(..)
    , netsFaultText
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Data.Time
import Network.SOAP.Parsing.Cursor (readT)
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (content, many, node)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import EID.Nets.Config
import EID.Nets.SignID
import KontraPrelude
import Log.Identifier
import MinutesTime
import Session.SessionID
import Text.XML.Parser

data NetsNOBankIDInternalProvider
  = NetsNOBankIDStandard
  | NetsNOBankIDMobile
  deriving (Eq, Ord, Show)

data NetsDKNemIDInternalProvider
  = NetsDKNemIDKeyCard
  | NetsDKNemIDKeyFile
  deriving (Eq, Ord, Show)

unsafeNetsNOBankIDInternalProviderFromInt16 :: Int16 -> NetsNOBankIDInternalProvider
unsafeNetsNOBankIDInternalProviderFromInt16 v = case v of
  1 -> NetsNOBankIDStandard
  2 -> NetsNOBankIDMobile
  _ -> $unexpectedError "Range error while fetching NetsNOBankIDInternalProvider from Int16"

unsafeNetsDKNemIDInternalProviderFromInt16 :: Int16 -> NetsDKNemIDInternalProvider
unsafeNetsDKNemIDInternalProviderFromInt16 v = case v of
  1 -> NetsDKNemIDKeyCard
  2 -> NetsDKNemIDKeyFile
  _ -> $unexpectedError "Range error while fetching NetsDKNemIDInternalProvider from Int16"

instance PQFormat NetsNOBankIDInternalProvider where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL NetsNOBankIDInternalProvider where
  type PQBase NetsNOBankIDInternalProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return NetsNOBankIDStandard
      2 -> return NetsNOBankIDMobile
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL NetsNOBankIDInternalProvider where
  type PQDest NetsNOBankIDInternalProvider = PQDest Int16
  toSQL NetsNOBankIDStandard = toSQL (1::Int16)
  toSQL NetsNOBankIDMobile   = toSQL (2::Int16)

instance PQFormat NetsDKNemIDInternalProvider where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL NetsDKNemIDInternalProvider where
  type PQBase NetsDKNemIDInternalProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return NetsDKNemIDKeyCard
      2 -> return NetsDKNemIDKeyFile
      _ -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL NetsDKNemIDInternalProvider where
  type PQDest NetsDKNemIDInternalProvider = PQDest Int16
  toSQL NetsDKNemIDKeyCard = toSQL (1::Int16)
  toSQL NetsDKNemIDKeyFile = toSQL (2::Int16)
----------------------------------------

data NetsNOBankIDAuthentication = NetsNOBankIDAuthentication {
    netsNOBankIDInternalProvider     :: !NetsNOBankIDInternalProvider
  , netsNOBankIDSignatoryName        :: !T.Text
  , netsNOBankIDPhoneNumber          :: !(Maybe T.Text)
  , netsNOBankIDDateOfBirth          :: !T.Text
  , netsNOBankIDCertificate          :: !ByteString
} deriving (Eq, Ord, Show)

data NetsDKNemIDAuthentication = NetsDKNemIDAuthentication {
    netsDKNemIDInternalProvider     :: !NetsDKNemIDInternalProvider
  , netsDKNemIDSignatoryName        :: !T.Text
  , netsDKNemIDDateOfBirth          :: !T.Text
  , netsDKNemIDCertificate          :: !ByteString
} deriving (Eq, Ord, Show)

-- | Information for transaction. It is encoded in frontend as (,,,) + Base64 when starting nets iframe.
--   Backend decodes that within resolve handler.
data NetsTarget = NetsTarget {
    netsTransactionDomain    :: String -- Domain where session cookie is set
  , netsDocumentID           :: !DocumentID
  , netsSignatoryID          :: !SignatoryLinkID
  , netsReturnURL            :: !String
} deriving (Eq, Ord, Show)

decodeNetsTarget :: String -> Maybe NetsTarget
decodeNetsTarget t = case (B64.decode $ BS.pack $ t) of
                       Right t' -> case (maybeRead $ BS.unpack t') of
                         Just (dmn,did,sid,rurl) -> Just $ NetsTarget dmn did sid rurl
                         _ -> Nothing
                       _ -> Nothing

-- | GetAssertionRequest request
data GetAssertionRequest = GetAssertionRequest {
  assertionArtifact :: !T.Text
} deriving (Eq, Ord, Show)

-- | Construct SOAP request from the 'GetAssertionRequest'.
instance ToXML GetAssertionRequest where
  toXML GetAssertionRequest{..} = do
      elementA "Request" [("xmlns","urn:oasis:names:tc:SAML:1.0:protocol"),("MajorVersion","1"),("MinorVersion","1")] $ do
         element "AssertionArtifact" assertionArtifact

-- | Collect action response.
data GetAssertionResponse = GetAssertionResponse {
  assertionStatusCode :: !T.Text,
  assertionAttributes :: ![(T.Text,T.Text)]
} deriving (Eq, Ord, Show)

-- | Retrieve 'GetAssertionResponse' from SOAP response.
xpGetAssertionResponse :: XMLParser GetAssertionResponse
xpGetAssertionResponse = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "Response" &| (\sr -> GetAssertionResponse {
      assertionStatusCode = T.concat $ sr $/ laxElement "Status" &/ laxAttribute "Value"
    , assertionAttributes = concatMap (uncurry zip) $
        sr $/ laxElement "Assertion" &/ laxElement "AttributeStatement" &|
          \a -> (a $/ laxAttribute "AttributeName" , a $/ laxElement "Attribute" &/ laxElement "AttributeValue"  &/ content)

    })

-- data NetsEidType = NetsEidNOBankID
--
data NetsNOBankIDSignature = NetsNOBankIDSignature {
  netsnoSignedText    :: !T.Text
, netsnoB64SDO        :: !T.Text -- base64 SDO from Nets ESigning
, netsnoSignatoryName :: !T.Text
, netsnoSignatoryPID  :: !T.Text
} deriving (Eq, Ord, Show)

data NetsSignStatus
  = NetsSignStatusSuccess
  | NetsSignStatusFailure NetsFault
  | NetsSignStatusAlreadySigned
  | NetsSignStatusInProgress

data NetsFault
  = NetsFaultExpiredTransaction
  | NetsFaultAPIError
  | NetsFaultCancelledByMerchant
  | NetsFaultExpired
  | NetsFaultExpiredByProxy
  | NetsFaultRejectedBySigner
  deriving (Eq, Ord, Show)

netsFaultText :: NetsFault -> T.Text
netsFaultText NetsFaultExpiredTransaction  = "nets_error_expired_transaction"
netsFaultText NetsFaultAPIError            = "nets_error_api"
netsFaultText NetsFaultCancelledByMerchant = "nets_error_canceled_by_merchant"
netsFaultText NetsFaultExpired             = "nets_error_expired"
netsFaultText NetsFaultExpiredByProxy      = "nets_error_expired_by_proxy"
netsFaultText NetsFaultRejectedBySigner    = "nets_error_rejected_by_signer"

data NetsSignOrder = NetsSignOrder
  { nsoSignOrderID :: !SignOrderUUID
  , nsoSignatoryLinkID :: !SignatoryLinkID
  , nsoTextToBeSigned :: !T.Text
  , nsoSessionID :: !SessionID
  , nsoDeadline :: !UTCTime
  , nsoIsCanceled :: !Bool
  } deriving (Show)

instance Loggable NetsSignOrder where
  logValue NetsSignOrder{..} = object [
      identifier_ nsoSignOrderID
    , identifier_ nsoSessionID
    , identifier_ nsoSignatoryLinkID
    , "is_canceled" .= nsoIsCanceled
    ]
  logDefaultLabel _ = "nets_insert_order"

-- NETS SIGNING - Insert Order Request

data InsertOrderRequest = InsertOrderRequest NetsSignOrder NetsSignConfig T.Text

instance ToXML InsertOrderRequest where
  toXML (InsertOrderRequest NetsSignOrder{..} NetsSignConfig{..} host_part) =
    element "InsertOrder" $ do
      element "OrderID" . T.pack . show $ nsoSignOrderID
      element "Documents" $ do
        element "Document" $ do
          element "LocalDocumentReference" ("doc_1" :: T.Text)
          element "Presentation" $ do
            element "Title" ("Scrive document" :: T.Text)
            element "Description" ("Scrive document to be signed" :: T.Text)
          element "DocType" $ do
            element "TEXT" $ do
              element "B64DocumentBytes" . T.pack . BS.unpack . B64.encode . T.encodeUtf8 $ nsoTextToBeSigned
          -- element "RequiresAuthentication" ("false" :: T.Text)
      element "Signers" $ do
        element "Signer" $ do
          element "EndUserSigner" $ do
            element "LocalSignerReference" ("signer_1" :: T.Text)
            element "AcceptedPKIs" $ do
              element "BankIDNOMobile" $ do
                element "CertificatePolicy" ("Personal" :: T.Text)
      element "WebContexts" $ do
        element "WebContext" $ do
          element "LocalWebContextRef" ("web_1" :: T.Text)
          element "SignURLBase" (netssignSignURLBase <> "index.html?ref=")
          element "ErrorURLBase" (host_part <> "/nets/sign_error?err=")
          element "StyleURL" (host_part <> "/css/assets/nets_no.css")
          element "ExitURL" (host_part <> "/nets/sign_exit")
          element "AbortURL" (host_part <> "/nets/sign_abort")
      element "ExecutionDetails" $ do
        element "OrderDeadline" . T.pack . formatTimeISO $ nsoDeadline
        element "Steps" $ do
          element "Step" $ do
            element "StepNumber" (1 :: Int)
            element "SigningProcess" $ do
              element "LocalWebContextRef" ("web_1" :: T.Text)
              element "LocalDocumentReference" ("doc_1" :: T.Text)
              element "LocalSignerReference" ("signer_1" :: T.Text)

-- NETS SIGNING - Insert Order Response

data InsertOrderResponse = InsertOrderResponse
  { iorsSignOrderUUID :: !SignOrderUUID
  , iorsTransactionRef :: !T.Text
  } deriving (Show)

instance Loggable InsertOrderResponse where
  logValue InsertOrderResponse{..} = object [
      identifier_ iorsSignOrderUUID
    , "transaction_ref" .= iorsTransactionRef
    ]
  logDefaultLabel _ = "nets_insert_order_response"

xpInsertOrderResponse :: XMLParser InsertOrderResponse
xpInsertOrderResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "InsertOrderResponse" &| \rs -> InsertOrderResponse {
      iorsSignOrderUUID    = parseSignOrderUUID $ readT "OrderID" rs
    , iorsTransactionRef = readT "TransRef" rs
    }

-- NETS Signing - Get Signing Processes Request
--
newtype GetSigningProcessesRequest = GetSigningProcessesRequest { unGetSigningProcessesRequest :: NetsSignOrder }

instance ToXML GetSigningProcessesRequest where
  toXML (GetSigningProcessesRequest NetsSignOrder{..}) =
    element "GetSigningProcesses" $ do
      element "OrderID" . T.pack . show $ nsoSignOrderID
      element "LocalSignerReference" ("signer_1" :: T.Text)

-- NETS Signing - Get Signing Processes Response

data GetSigningProcessesResponse = GetSigningProcessesResponse
  { gsprsTransactionRef :: !T.Text
  , gsprsSignOrderUUID    :: !SignOrderUUID
  , gsprsSignURL        :: !T.Text
  }

xpGetSigningProcessesResponse :: XMLParser GetSigningProcessesResponse
xpGetSigningProcessesResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "GetSigningProcessesResponse" &| \rs -> GetSigningProcessesResponse {
      gsprsSignOrderUUID    = parseSignOrderUUID $ readT "OrderID" rs
    , gsprsTransactionRef = readT "TransRef" rs
    , gsprsSignURL        = readTs ["SigningProcessResults", "SigningProcessResult", "SignURL"] rs
    }

instance Loggable GetSigningProcessesResponse where
  logValue GetSigningProcessesResponse{..} = object [
      identifier_ gsprsSignOrderUUID
    , "transaction_ref" .= gsprsTransactionRef
    , "nets_sign_url" .= gsprsSignURL
    ]
  logDefaultLabel _ = "nets_get_signing_processes_response"

-- NETS Signing - Get Order Status Request

newtype GetOrderStatusRequest = GetOrderStatusRequest { unGetOrderStatusRequest :: NetsSignOrder }

instance ToXML GetOrderStatusRequest where
  toXML (GetOrderStatusRequest NetsSignOrder{..}) =
    element "GetOrderStatus" $ do
      element "OrderID" . T.pack . show $ nsoSignOrderID

--NETS Signing - Get Order Status Response

data OrderStatus
  = Active
  | CancelledByMerchant
  | Expired
  | ExpiredByProxy
  | RejectedBySigner
  | Complete
  deriving (Eq, Ord, Show, Read)

data GetOrderStatusResponse = GetOrderStatusResponse
  { gosrsSignOrderUUID :: !SignOrderUUID
  , gosrsTransactionRef :: !T.Text
  , gosrsOrderStatus :: !OrderStatus
  }

instance Loggable GetOrderStatusResponse where
  logValue GetOrderStatusResponse{..} = object [
      identifier_ gosrsSignOrderUUID
    , "transaction_ref" .= gosrsTransactionRef
    , "order_status" .= (T.pack . show $ gosrsOrderStatus)
    ]
  logDefaultLabel _ = "nets_get_order_status_response"

xpGetOrderStatusResponse :: XMLParser GetOrderStatusResponse
xpGetOrderStatusResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "GetOrderStatusResponse" &| \rs -> GetOrderStatusResponse {
      gosrsSignOrderUUID    = parseSignOrderUUID $ readT "OrderID" rs
    , gosrsTransactionRef = readT "TransRef" rs
    , gosrsOrderStatus    = parseOrderStatus $ readT "OrderStatus" rs
    }

parseOrderStatus :: T.Text -> OrderStatus
parseOrderStatus t = fromMaybe
  ($unexpectedError $ "Cannot parse OrderStatus in GetOrderStatusResponse:" <+> T.unpack t)
  (maybeRead $ T.unpack t)

-- NETS Signing - Get SDO Request

newtype GetSDORequest = GetSDORequest { unGetSDORequest :: NetsSignOrder }

instance ToXML GetSDORequest where
  toXML (GetSDORequest NetsSignOrder{..}) =
    element "GetSDO" $ do
      element "OrderID" . T.pack . show $ nsoSignOrderID

--NETS Signing - Get SDO Response

data GetSDOResponse = GetSDOResponse
  { gsdorsSignOrderUUID    :: !SignOrderUUID
  , gsdorsTransactionRef :: !T.Text
  , gsdorsB64SDOBytes    :: !T.Text
  }

instance Loggable GetSDOResponse where
  logValue GetSDOResponse{..} = object [
      identifier_ gsdorsSignOrderUUID
    , "transaction_ref" .= gsdorsTransactionRef
    , "b64_sdo_bytes_length" .= T.length gsdorsB64SDOBytes
    ]
  logDefaultLabel _ = "nets_get_sdo_response"

xpGetSDOResponse :: XMLParser GetSDOResponse
xpGetSDOResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "GetSDOResponse" &| \rs -> GetSDOResponse {
      gsdorsSignOrderUUID    = parseSignOrderUUID $ readT "OrderID" rs
    , gsdorsTransactionRef = readT "TransRef" rs
    , gsdorsB64SDOBytes    = readT "B64SDOBytes" rs
    }


-- NETS Signing - Get SDO Details Request

data GetSDODetailsRequest = GetSDODetailsRequest !T.Text

instance ToXML GetSDODetailsRequest where
  toXML (GetSDODetailsRequest b64SDO) =
    element "GetSDODetails" $ do
      element "B64SDOBytes" b64SDO
      element "VerifySDO" ("true" :: T.Text)

--NETS Signing - Get SDO Response

data GetSDODetailsResponse = GetSDODetailsResponse
  { gsdodrsTransactionRef :: !T.Text
  , gsdodrsSignedText     :: !T.Text
  , gsdodrsSignerCN       :: !T.Text
  , gsdodrsSignerPID      :: !T.Text
  }

instance Loggable GetSDODetailsResponse where
  logValue GetSDODetailsResponse{..} = object [
      "transaction_ref" .= gsdodrsTransactionRef
    , "signed_text" .= gsdodrsSignedText
    , "signer_cn" .= gsdodrsSignerCN
    ]
  logDefaultLabel _ = "nets_get_sdo_details_response"

xpGetSDODetailsResponse :: XMLParser GetSDODetailsResponse
xpGetSDODetailsResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "GetSDODetailsResponse" &| \rs -> GetSDODetailsResponse {
      gsdodrsTransactionRef = readT "TransRef" rs
    , gsdodrsSignedText     = readTs ["SDOList", "SDO", "SignedData"] rs
    , gsdodrsSignerCN       = readTs ["SDOList", "SDO", "SDOSignatures", "SDOSignature", "SignerCertificateInfo", "CN"] rs
    , gsdodrsSignerPID      = readTs ["SDOList", "SDO", "SDOSignatures", "SDOSignature", "SignerCertificateInfo", "UniqueID"] rs
    }

-- NETS Signing - Cancel Order Request

data CancelOrderRequest = CancelOrderRequest !SignOrderUUID

instance ToXML CancelOrderRequest where
  toXML (CancelOrderRequest soid) =
    element "CancelOrder" $ do
      element "OrderID" . T.pack . show $ soid

--NETS Signing - Cancel Order Response

data CancelOrderResponse = CancelOrderResponse
  { corsTransactionRef :: !T.Text
  }

instance Loggable CancelOrderResponse where
  logValue CancelOrderResponse{..} = object [
      "transaction_ref" .= corsTransactionRef
    ]
  logDefaultLabel _ = "nets_cancel_order_response"

xpCancelOrderResponse :: XMLParser CancelOrderResponse
xpCancelOrderResponse = XMLParser $ \cursor -> listToMaybe $ cursor
  $/ laxElement "CancelOrderResponse" &| \rs -> CancelOrderResponse {
      corsTransactionRef = readT "TransRef" rs
    }

readTs :: [T.Text] -> Cursor -> T.Text
readTs names cursor = T.concat $ cursor $/ laxElements names content
  where
    laxElements = foldr (.) id . fmap (&/) . fmap laxElement
