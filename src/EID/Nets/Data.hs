module EID.Nets.Data (
      NetsNOBankIDInternalProvider(..)
    , unsafeNetsNOBankIDInternalProviderFromInt16

    , NetsNOBankIDAuthentication(..)
    -- Target passed inside request
    , NetsTarget(..)
    , encodeNetsTarget
    , decodeNetsTarget

    -- Request calls
    , GetAssertionRequest(..)
    , GetAssertionResponse(..)
    , xpGetAssertionResponse
  ) where

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Int
import Data.Text hiding (concatMap,zip)
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (content, many, node)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS

import DB
import Doc.DocumentID
import Doc.SignatoryLinkID
import KontraPrelude
import Network.SOAP.Call

data NetsNOBankIDInternalProvider = NetsNOBankIDStandard | NetsNOBankIDMobile
  deriving (Eq, Ord, Show)

unsafeNetsNOBankIDInternalProviderFromInt16 :: Int16 -> NetsNOBankIDInternalProvider
unsafeNetsNOBankIDInternalProviderFromInt16 v = case v of
  1 -> NetsNOBankIDStandard
  2 -> NetsNOBankIDMobile
  _ -> $unexpectedError "Range error while fetching NetsNOBankIDInternalProvider from Int16"


instance PQFormat NetsNOBankIDInternalProvider where
  pqFormat = const $ pqFormat ($undefined::Int16)

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
  toSQL NetsNOBankIDStandard     = toSQL (1::Int16)
  toSQL NetsNOBankIDMobile       = toSQL (2::Int16)

----------------------------------------


data NetsNOBankIDAuthentication = NetsNOBankIDAuthentication {
    netsNOBankIDInternalProvider     :: !NetsNOBankIDInternalProvider
  , netsNOBankIDSignatoryName        :: !Text
  , netsNOBankIDPhoneNumber          :: !(Maybe Text)
  , netsNOBankIDCertificate          :: !(Binary ByteString)
} deriving (Eq, Ord, Show)


-- | Final BankID signature.
data NetsTarget = NetsTarget {
    netsDocumentID           :: !DocumentID
  , netsSignatoryID          :: !SignatoryLinkID
  , netsReturnURL            :: !String
} deriving (Eq, Ord, Show)

encodeNetsTarget :: NetsTarget -> String
encodeNetsTarget nt = BS.unpack $ B64.encode $ BS.pack $ show (netsDocumentID nt,netsSignatoryID nt, netsReturnURL nt)

decodeNetsTarget :: String -> Maybe NetsTarget
decodeNetsTarget t = case (B64.decode $ BS.pack $ t) of
                       Right t' -> case (maybeRead $ BS.unpack t') of
                         Just (did,sid,rurl) -> Just $ NetsTarget did sid rurl
                         _ -> Nothing
                       _ -> Nothing




-- | GetAssertionRequest request
data GetAssertionRequest = GetAssertionRequest {
  assertionArtifact :: !Text
} deriving (Eq, Ord, Show)

-- | Construct SOAP request from the 'GetAssertionRequest'.
instance ToXML GetAssertionRequest where
  toXML GetAssertionRequest{..} = do
      elementA "Request" [("xmlns","urn:oasis:names:tc:SAML:1.0:protocol"),("MajorVersion","1"),("MinorVersion","1")] $ do
         element "AssertionArtifact" assertionArtifact

-- | Collect action response.
data GetAssertionResponse = GetAssertionResponse {
  assertionStatusCode :: !Text,
  assertionAttributes :: ![(Text,Text)]
} deriving (Eq, Ord, Show)

-- | Retrieve 'CollectResponse' from SOAP response.
xpGetAssertionResponse :: XMLParser GetAssertionResponse
xpGetAssertionResponse = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "Response" &| (\sr -> GetAssertionResponse {
      assertionStatusCode = Data.Text.concat $ sr $/ laxElement "Status" &/ laxAttribute "Value"
    , assertionAttributes = concatMap (uncurry zip) $
        sr $/ laxElement "Assertion" &/ laxElement "AttributeStatement" &|
          \a -> (a $/ laxAttribute "AttributeName" , a $/ laxElement "Attribute" &/ laxElement "AttributeValue"  &/ content)

    })

